{-

The Luhn algorithm (Wikipedia) is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:

    consider each digit as a separate number;
    moving left, double every other number from the second last;
    subtract 9 from each number that is now greater than 9;
    add all the resulting numbers together;
    if the total is divisible by 10, the card number is valid.

Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is greater than 9. For example:

> luhnDouble 3
6

> luhnDouble 6
3

Using luhnDouble and the integer remainder function mod, define a function luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank card number is valid. For example:

> luhn 1 7 8 4
True

> luhn 4 7 8 3
False

Now define a function luhnFinal :: Int -> Int -> Int -> Bool that returns the fourth digit of a four-digit bank card number. For example:

> luhnFinal 1 7 8
4

> luhnFinal 4 7 8
8

-}

luhnDouble :: Int -> Int
luhnDouble = undefined

luhn :: Int -> Int -> Int -> Int -> Bool
luhn = undefined

luhnFinal :: Int -> Int -> Int -> Int
luhnFinal = undefined

-- test
prop_luhnDouble :: Property
prop_luhnDouble = luhnDouble 3 === 6

prop_luhn :: Property
prop_luhn = (luhn 1 7 8 4) === True

prop_luhnFinal :: Property
prop_luhnFinal = (luhnFinal 1 7 8) === 4


-- solution
-- Doubles the current number and substracts 9 if result is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = let y = (2 * x) in if y > 9
    then y - 9
    else y

-- Checks if 4 digits form valid bank card number
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum[luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0

-- Calculates final luhn digit
luhnFinal :: Int -> Int -> Int -> Int
luhnFinal a b c = 10 - rest
                  where
                    rest = sum[luhnDouble a, b, luhnDouble c] `mod` 10
