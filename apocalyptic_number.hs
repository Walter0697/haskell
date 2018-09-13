--- two to the power of 
--- Input : Integer n
--- Output : 2^n
twoToThePowerOf :: (RealFloat num) => Int -> num
twoToThePowerOf 0 = 1
twoToThePowerOf n = 2 * twoToThePowerOf(n - 1)

--- is less than three digits
--- Input : Number
--- Output : boolean for is the number less than three digits
isLessThanTreeDigit :: (RealFloat num) => num  -> Bool
isLessThanTreeDigit n = (n / 10) < 10

--- contain six six six
--- Input : Number
--- Output : boolean for if the number contains 666 
containSixSixSix :: (RealFloat num) => num -> Bool
containSixSixSix n 
    | (isLessThanTreeDigit n) == True = False
    | (mod (round n) 1000) == 666 = True
    | otherwise = containSixSixSix (n / 10)

--- is apocalyptic
--- Input : Integer
--- Output : boolean for if it is a apocalyptic number
isApocalyptic :: Int -> Bool
isApocalyptic n = containSixSixSix( twoToThePowerOf n )

main = undefined


