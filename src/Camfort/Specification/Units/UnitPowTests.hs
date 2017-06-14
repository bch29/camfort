module Camfort.Specification.Units.UnitPowTests where



-- | I am currently running tests in ghci with
-- quickCheckWith stdArgs { maxSuccess = 10000 } prop_xyz



import Camfort.Specification.Units.UnitPow

import Test.QuickCheck
import Control.Monad (liftM)





instance Arbitrary UnitAtom where
  arbitrary = do
    uid <- elements $ map (:"") ['a'..'z']
    p <- elements $ map fromRational $ [-20.0..20.0] ++ [0.5, 0.1]
    return UnitAtom{uniqueId=uid, power=p}

instance Arbitrary Unit where
  arbitrary = liftM Unit arbitrary





prop_assoc e1 e2 e3 = normalForm ((e1 * e2) * e3) == normalForm (e1 * (e2 * e3))

prop_comm e1 e2 = normalForm (e1 * e2) == normalForm (e2 * e1)

prop_inv e = normalForm (recip e * e) == Unit []

prop_idemp e = normalForm e == normalForm (normalForm e)
