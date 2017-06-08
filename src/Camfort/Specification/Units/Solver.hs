{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Camfort.Specification.Units.Solver where

import Prelude hiding (not, or, and)
import Data.SBV.Bridge.Z3
import Data.Data
import Data.Generics

type T = SReal

-- Fortran code

-- != unit(x) :: m
-- != unit(t) :: s
-- real :: x, t
-- real :: v
-- v = x / t

-- Want: checking (Yes/No because...)
--     : inference- here are the rest of the solutions...


example = do
  satResult <- sat predicate
  thmResult <- prove predicate
  case thmResult of
    ThmResult (Unknown _ model) -> putStrLn $ "Unknown: " ++ show model
    _ -> return ()
  case satResult of
    SatResult (Unknown _ model) -> putStrLn $ "Unknown (SAT): " ++ show model
    _ -> return ()
  return (satResult, thmResult)
  where
    predicate = do
      let (uc_m :: T) = uninterpret "m"
      let (uc_s :: T) = uninterpret "s"

      (uv :: T) <- exists "units(v)"

      (ux :: T) <- exists "units(x)"
      (ut :: T) <- exists "units(t)"

      -- Constraint from the code
      let sig1 = uv .== (ux / ut) &&& ux .== uc_m &&& ut .== uc_s

      return $ sig1

      -- v = x . t^(-1) =>
      -- log u_v = log u_x - log u_t
      -- =>

type SUnits = SBV Units

data Units = M | S
   deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

data UnitsExpr = Mult Units Units
     deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind, SatModel)

mul :: SUnits -> SUnits -> SUnits
mul = uninterpret "MUL"

add :: SUnits -> SUnits -> SUnits
add = uninterpret "ADD"

recip' = uninterpret "RECIP"

example2 =  do
  satResult <- sat predicate
  thmResult <- prove predicate
  case thmResult of
    ThmResult (Unknown _ model) -> putStrLn $ "Unknown: " ++ show model
    _ -> return ()
  case satResult of
    SatResult (Unknown _ model) -> putStrLn $ "Unknown (SAT): " ++ show model
    _ -> return ()
  return (satResult, thmResult)
  where
    predicate = do
      -- addAxiom "m" ["(declare-fun MUL (Units Units) Units)"]
      addAxiom "*comm" [ "(assert (forall ((u Units) (v Units))"
                        , "  (= (MUL u v)"
                        , "     (MUL v u))))" ]

      let m = literal $ M
      let s = literal $ S

      (uv :: SUnits) <- exists "units(v)"
      (ux :: SUnits) <- exists "units(x)"
      (ut :: SUnits) <- exists "units(t)"

      -- Constraint from the code
      --let sig1 = uv .== (ux * (ut ^^ (-1))) &&& ux .== m &&& ut .== s
      let sig1 = uv .== (ux `mul` (recip' ut)) &&& ux .== m &&& ut .== s

      return $ sig1


-----------------------------------------------------------------------------
-- * Representing uninterpreted booleans
-----------------------------------------------------------------------------

-- | The uninterpreted sort 'B', corresponding to the carrier.
-- To prevent SBV from translating it to an enumerated type, we simply attach an unused field
newtype B = B () deriving (Eq, Ord, Show, Read, Data, SymWord, HasKind)

-- | Handy shortcut for the type of symbolic values over 'B'
type SB = SBV B

-----------------------------------------------------------------------------
-- * Uninterpreted connectives over 'B'
-----------------------------------------------------------------------------

-- | Uninterpreted logical connective 'and'
and :: SB -> SB -> SB
and = uninterpret "AND"

-- | Uninterpreted logical connective 'or'
or :: SB -> SB -> SB
or  = uninterpret "OR"

-- | Uninterpreted logical connective 'not'
not :: SB -> SB
not = uninterpret "NOT"

-----------------------------------------------------------------------------
-- * Axioms of the logical system
-----------------------------------------------------------------------------

-- | Distributivity of OR over AND, as an axiom in terms of
-- the uninterpreted functions we have introduced. Note how
-- variables range over the uninterpreted sort 'B'.
ax1 :: [String]
ax1 = [ "(assert (forall ((p B) (q B) (r B))"
      , "   (= (AND (OR p q) (OR p r))"
      , "      (OR p (AND q r)))))"
      ]

-- | One of De Morgan's laws, again as an axiom in terms
-- of our uninterpeted logical connectives.
ax2 :: [String]
ax2 = [ "(assert (forall ((p B) (q B))"
      , "   (= (NOT (OR p q))"
      , "      (AND (NOT p) (NOT q)))))"
      ]

-- | Double negation axiom, similar to the above.
ax3 :: [String]
ax3 = ["(assert (forall ((p B)) (= (NOT (NOT p)) p)))"]

-----------------------------------------------------------------------------
-- * Demonstrated deduction
-----------------------------------------------------------------------------

-- | Proves the equivalence @NOT (p OR (q AND r)) == (NOT p AND NOT q) OR (NOT p AND NOT r)@,
-- following from the axioms we have specified above. We have:
--
-- >>> test
-- Q.E.D.
test :: IO ThmResult
test = prove $ do addAxiom "*comm" [ "(assert (forall ((u B) (v B))"
                        , "  (= (AND u v)"
                        , "     (AND v u))))" ]
                  addAxiom "OR distributes over AND" ax1
                  addAxiom "de Morgan"               ax2
                  addAxiom "double negation"         ax3
                  p <- free "p"
                  q <- free "q"
                  r <- free "r"
                  return $   not (p `or` (q `and` r))
                         .== (not p `and` not q) `or` (not p `and` not r)