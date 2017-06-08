module UnitExprTests where

import Camfort.Specification.Units.UnitExpr

import qualified Test.QuickCheck as QC
import Control.Monad





instance QC.Arbitrary Expr where
    arbitrary = do
      n <- QC.choose (0,15) :: QC.Gen UniqueID
      QC.oneof [ return (Var n)
               , return Id
               , liftM2 Mult QC.arbitrary QC.arbitrary
               , liftM Inverse QC.arbitrary
               ]





-- | Logical implication operator.
(==>) :: Bool -> Bool -> Bool; infix 1 ==>
a ==> b = a <= b



prop_trans e1 e2 e3 = e1 == e2 && e2 == e3 ==> e1 == e3

prop_cong1 e1 e2 = (e1 == e2) == (Inverse e1 == Inverse e2)

prop_cong2 e1 e2 e3 e4 = e1 == e2 && e3 == e4 ==> Mult e1 e3 == Mult e2 e4

prop_id e = normalForm e == normalForm (Mult e Id)

prop_assoc e1 e2 e3 = normalForm (Mult (Mult e1 e2) e3) == normalForm (Mult e1 (Mult e2 e3))

prop_comm e1 e2 = normalForm (Mult e1 e2) == normalForm (Mult e2 e1)

prop_inv e1 = normalForm (Mult (Inverse e1) e1) == Id
