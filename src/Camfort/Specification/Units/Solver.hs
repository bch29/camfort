{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Camfort.Specification.Units.Solver where

import Data.SBV.Bridge.Z3
import Data.Data

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

mul, add :: SUnits -> SUnits -> SUnits
mul u v = uninterpret "MUL"
add u v = uninterpret "ADD"

recip' u = uninterpret "RECIP"

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
