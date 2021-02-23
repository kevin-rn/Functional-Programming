module Lec4 where
import Test.QuickCheck

import Luhn (luhn, luhnSpec)

dist :: Int -> Int -> Int
dist x y = abs (x - y)

prop_dist_self :: Int -> Bool
prop_dist_self x = dist x x == 0

prop_dist_sym :: Int -> Int -> Bool
prop_dist_sym x y = dist x y == dist y x

prop_dist_pos :: Int -> Int -> Bool
prop_dist_pos x y = dist x y > 0

main = do
    quickCheck prop_dist_self
    quickCheck prop_dist_sym
    quickCheck prop_dist_pos


isSorted :: Ord a => [a] -> Bool 
isSorted [] = True 
isSorted [x] = True 
isSorted (x:y:ys) = x <= y && isSorted (y:ys)

prop_isSorted :: [Int] -> Bool 
prop_isSorted xs = isSorted xs

some_property = True

prop_insert_isSorted xs = (isSorted xs && length xs >= 100) ==> some_property








prop_luhn_correct :: Property
prop_luhn_correct = forAll randomBankCard (\xs -> luhn xs == luhnSpec xs)
  where
      randomBankCard :: Gen [Int]
      randomBankCard = listOf1 randomDigit

      randomDigit :: Gen Int 
      randomDigit = chooseInt (0,9)
