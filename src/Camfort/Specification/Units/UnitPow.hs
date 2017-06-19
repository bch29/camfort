module Camfort.Specification.Units.UnitPow where

-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as S (fromList)
import Data.Ratio ((%), denominator, numerator)
import Data.List (nubBy, sort)



data UnitAtom = UnitAtom { uniqueId :: UniqueId
                         , power :: Power
                         }
  deriving (Eq, Ord, Show)

type UniqueId = String -- Word -- Unsigned Int
type Power = Rational

uniqueIdEquality :: UnitAtom -> UnitAtom -> Bool
uniqueIdEquality a1 a2 = uniqueId a1 == uniqueId a2




newtype Unit = Unit [UnitAtom] -- TODO: Consider using Seq instead
  deriving (Ord)

instance Eq Unit where
  u1 == u2 = and $ zipWith (==) (norm u1) (norm u2)
    where norm = unwrap . normalForm
          unwrap (Unit as) = as

instance Num Unit where
  u + _ = u
  u - _ = u
  Unit as * Unit as' = Unit (as ++ as')
  -- Unit as / Unit as' = Unit (as ++ recip as')
  abs u = u
  signum (Unit []) = 0 -- ?
  signum u = 1 -- ?

instance Fractional Unit where
  recip = inverse

instance Show Unit where
  show (Unit []) = ""
  show (Unit (a:as)) = uniqueId a ++ pow ++ " " ++ show (Unit as)
    where pow = case power a of
                  1 -> ""
                  n -> superscript n

superscript :: Rational -> String
superscript n = map sup nstring
  where
    nstring | denominator n == 1 = show $ numerator n
            | otherwise = show $ fromRational n
    sup x = case x of
              '.' -> '˙'
              '-' -> '⁻'
              '0' -> '⁰'
              '1' -> '¹'
              '2' -> '²'
              '3' -> '³'
              '4' -> '⁴'
              '5' -> '⁵'
              '6' -> '⁶'
              '7' -> '⁷'
              '8' -> '⁸'
              '9' -> '⁹'
              x' -> x'


eg1 :: Unit
eg1 = Unit [UnitAtom "m" 1, UnitAtom "s" (-1)]
eg2 = Unit [UnitAtom "s" (-1), UnitAtom "m" 1]

unit u p = Unit [UnitAtom u p]




unique :: Unit -> [UniqueId]
unique (Unit as) = map uniqueId (nubBy uniqueIdEquality as)

-- | For each var, pair with its power
normalForm :: Unit -> Unit
normalForm u = Unit $ sort $ filter notPow0 $ zipWith UnitAtom uids powers
  where uids = unique u
        powers = map (`pow` u) uids
        notPow0 (UnitAtom uid 0) = False
        notPow0 _ = True


pow :: UniqueId -> Unit -> Power
pow _ (Unit []) = 0
pow uid (Unit (a:as))
  | uid == uniqueId a = power a + pow uid (Unit as)
  | otherwise         = pow uid (Unit as)


inverse :: Unit -> Unit
inverse (Unit as) = Unit (map (\a -> a{power = negate (power a)}) as)
