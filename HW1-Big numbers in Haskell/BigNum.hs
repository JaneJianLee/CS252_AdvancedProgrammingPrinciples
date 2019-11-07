{-
  Name: Ji An Lee (jian.lee@sjsu.edu)
  Class: CS 252 (Fall 2019)
  Assigment: HW1
  Date: Sept. 9th, 2019
  Description: [Haskell] Implement +/-/*/^ operations supporting big numers
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]
maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0

bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] 0 = []
bigAdd' [] [] 1 = [1]
bigAdd' [] (t:ts) c = (mod (t+c) maxblock) : bigAdd' [] ts 0
bigAdd' (h:hs) [] c = (mod (h+c) maxblock) : bigAdd' hs [] 0
bigAdd' (x:xs) (y:ys) z = (mod (x+y+z) maxblock) : bigAdd' xs ys (quot (x+y+z) maxblock)

bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [] [] 1 = error "Error, Negative numbers not supported..!"
bigSubtract' [] [] 0 = []
bigSubtract' (x:xs) [] 0 = [x]
bigSubtract' (x:xs) [] 1 
             | x<1 = (x-1+maxblock) : bigSubtract' xs [] 1
             | otherwise = (x-1): bigSubtract' xs [] 0    
bigSubtract' (x:xs) (y:ys) c 
             |(x-c)> y = ((x-c)-y): bigSubtract' xs ys 0
             |(x-c)< y = ((x-c+maxblock)-y) : bigSubtract' xs ys 1
             |(x-c)==y = 0 : bigSubtract' xs ys 0

bigEq :: BigNum -> BigNum -> Bool
bigEq x y = (x == y)

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

-- Handle multiplication following the same approach you learned in grade
-- school, except dealing with blocks of 3 digits rather than single digits.
-- If you are having trouble finding a solution, write a helper method that
-- multiplies a BigNum by an Int.

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply [] [] = []
bigMultiply listx [] = []
bigMultiply _ [0] = [0]
bigMultiply [0] _ = [0]
bigMultiply listx (numy:listys) = bigAdd (bigMul' listx numy 0) (0:(bigMultiply listx listys))  

bigMul' :: BigNum -> Block -> Block -> BigNum
bigMul' [] n 0 =[]
bigMul' [] n c =[c]
bigMul' (x:xs) n c = (mod ((x*n)+c) maxblock) : (bigMul' xs n (quot ((x*n)+c) maxblock))

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf [0] _ = error "You cannot have zero as base number"
bigPowerOf _ [0] = [1] 
bigPowerOf x y = bigMultiply x (bigPowerOf x (bigDec y))

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]

sig = "9102llaf"



