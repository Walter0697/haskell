------------------------------------------ Main function in this program that will check if a number meets different requirements

--- isApocalyptic :
--- if 2^n contains 666, then n is an apocalyptic number
isApocalyptic :: Int -> Bool
isApocalyptic n = containSixSixSix( twoToThePowerOf n )

--- isAutomorphic:
--- if n^2 contains n at the end, then n is an automorphic number
isAutomorphic :: Int -> Bool
isAutomorphic n = ( mod ( toThePowerOf n 2 ) (teeen (numberLength n)) ) == n

------------------------------------------ Helper functions for specific type of questions

--- two to the power of function
--- twoToThePowerOf n    returns 2^n
twoToThePowerOf n = toThePowerOf 2 n

--- is less than three digits
--- see if the length of a number will be smaller than 3
--- isLessThanTreeDigit 123 = True
--- isLessThanTreeDigit 123445 = False
isLessThanTreeDigit n = isLessThanNDigit n 3

--- contain six six six function
--- see if a number contains 666
--- containSixSixSix 123666 = True
--- containSixSixSix 666123 = True
--- containSixSixSix 126663 = True
--- containSixSixSix 661236 = False
containSixSixSix :: (Integral num) => num -> Bool
containSixSixSix n 
    | (isLessThanTreeDigit n) == True = False
    | (mod n 1000) == 666 = True
    | otherwise = containSixSixSix (quot n 10)


------------------------------------------ Helper functions that is reusable

--- to the power of function
--- toThePowerOf n m   returns n^m
toThePowerOf :: (Integral num) => num -> Int -> num
toThePowerOf n 0 = 1
toThePowerOf n m = n * (toThePowerOf n (m - 1))

--- teeen function
--- teeen n            returns 10^n
teeen n = toThePowerOf 10 n

--- is less than n digit function
--- check if the length of the number is less than a number
--- isLessThanNDigit 123123 3 = False
--- isLessThanNDigit 234234 7 = True
isLessThanNDigit :: (Integral num) => num -> Int -> Bool
isLessThanNDigit n 0 = True
isLessThanNDigit n d = (quot n (teeen (d - 1))) < 10

--- number length function
--- get the numbers of digit in that number
numberLength :: Int -> Int
numberLength 0 = 1
numberLength n 
    | (quot n 10) == 0 = 1
    | otherwise = 1 + (numberLength (quot n 10))


main = undefined


