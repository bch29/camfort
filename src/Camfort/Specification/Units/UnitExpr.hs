module Camfort.Specification.Units.UnitExpr where

import Data.Set (Set)
import qualified Data.Set as S (empty, singleton, toAscList, union)





-- | Definiton of unit expressions (hopefully an Abelian Group)
data Expr = Var UniqueID
          | Id
          | Product Expr Expr
          | Inverse Expr
  deriving (Eq, Ord, Show)
  -- NB: '==' is merely syntactic equality! Use on 'Expr's in 'normalForm' to
  -- compare for semantic equality.



type UniqueID = Word





-- | Example 'Expr's
eg1 = Product (Var 1) (Inverse (Var 2))
eg2 = Product (Inverse (Var 2)) (Var 1) -- Product (Var 1) (Inverse (Var 2))
eg3 = Product (Var 1) Id -- (Var 1)
eg4 = Product (Var 1) (Inverse (Var 1)) -- Id
eg5 = Product (Var 1) (Product (Var 2) (Inverse (Var 1))) -- (Var 2)
eg6 = Inverse (Inverse (Var 1)) -- (Var 1)





-- | Return the normal form of an expression
normalForm :: Expr -> Expr
normalForm e = fromList $ filter (/= Id) $ zipWith exponToProduct vs (map (expon' e) vs)
  where vs = S.toAscList $ vars e
        expon' = flip expon

-- E.g.
-- [(Var 1),(Var 2),Id] ~>
-- [((Var 1),1),((Var 2),(-1)),(Id,0)] ~>
-- [((Var 1),1),((Var 2),(-1))]



-- | Give the set of 'Var's that occur in an 'Expr'
vars :: Expr -> Set Expr

vars v@Var{} = S.singleton v
vars Id = S.empty
vars (Product e1 e2) = S.union (vars e1) (vars e2)
vars (Inverse e) = vars e



-- | Given a 'Var', return its exponent in the 'Expr'
-- >>> expon (Var 1) (Product (Var 1) (Var 1))
-- 2
expon :: Expr -> Expr -> Int
expon e v@Var{}
  | e == v = 1
  | otherwise = 0
expon _ Id = 0
expon e (Product e1 e2) = expon e e1 + expon e e2
expon e (Inverse e1) = negate $ expon e e1



-- | Given a list of expressions, return a single product expression
fromList :: [Expr] -> Expr
fromList [] = Id
fromList es = foldr1 Product es



-- | Given a 'Var' and a power (Int), return the corresponding 'Expr'
exponToProduct :: Expr -> Int -> Expr
exponToProduct Var{} 0 = Id
exponToProduct v@Var{} 1 = v
exponToProduct v@Var{} (-1) = Inverse v
exponToProduct v@Var{} n
  | n > 1 = Product v (exponToProduct v (n-1))
  | otherwise = Inverse (Product v (exponToProduct v ((-n) - 1)))
exponToProduct _ _ = error "Exponentiation only defined for variables"
