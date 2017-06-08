module Camfort.Specification.Units.UnitExprTests where



-- | I am currently running tests in ghci with
-- QC.quickCheckWith QC.stdArgs { QC.maxSuccess = 100000 }


import Camfort.Specification.Units.UnitExpr

import qualified Test.QuickCheck as QC
import Control.Monad





instance QC.Arbitrary Expr where
    arbitrary = do
      n <- QC.choose (0,15) :: QC.Gen UniqueID
      QC.oneof [ return (Var n)
               , return Id
               , liftM2 Product QC.arbitrary QC.arbitrary
               , liftM Inverse QC.arbitrary
               ]





-- | Logical implication operator.
(==>) :: Bool -> Bool -> Bool; infix 1 ==>
a ==> b = a <= b



prop_trans e1 e2 e3 = e1 == e2 && e2 == e3 ==> e1 == e3

prop_cong1 e1 e2 = (e1 == e2) == (Inverse e1 == Inverse e2)

prop_cong2 e1 e2 e3 e4 = e1 == e2 && e3 == e4 ==> Product e1 e3 == Product e2 e4

prop_id e = normalForm e == normalForm (Product e Id)

prop_assoc e1 e2 e3 = normalForm (Product (Product e1 e2) e3) == normalForm (Product e1 (Product e2 e3))

prop_comm e1 e2 = normalForm (Product e1 e2) == normalForm (Product e2 e1)

prop_inv e = normalForm (Product (Inverse e) e) == Id
