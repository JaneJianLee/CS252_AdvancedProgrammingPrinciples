> import Data.List

Experiment with foldl, foldr, and foldl'

First, implement your own version of the foldl function,
defined as myFoldl

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl _ acc [] = acc
> myFoldl f acc (x:xs) = myFoldl f (f acc x) xs


Next, define a function to reverse a list using foldl.

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse x = myFoldl (\acc item -> item : acc) [] x


Now define your own version of foldr, named myFoldr

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr _ acc [] = acc
> myFoldr f acc (x:xs) = f x (myFoldr f acc xs)


Now try using foldl (the library version, not yours) to sum up the numbers of a large list.
Why is it so slow?

Due to laxy computation inherent in Haskell, stack/heap overflow occurs when using foldr/foldl with big numbers.
Numbers generated during calculation are all kept in memory until actual it is actually processed to the final result.

Instead of foldl, try using foldl'.
Why is it faster?
(Read http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 for some hints)

Foldl' does not create a big unreduced chain during calculation.
Foldl' reduces the chain in each computational steps so a big thunk does not pile up in memory.

For an extra challenge, try to implement foldl in terms of foldr.
See http://www.haskell.org/haskellwiki/Foldl_as_foldr for details.

Next, using the map function, convert every item in a list to its absolute value

> listAbs :: [Integer] -> [Integer]
> listAbs = map abs

Finally, write a function that takes a list of Integers and returns the sum of
their absolute values.

> sumAbs :: [Integer] -> Integer
> sumAbs x = myFoldr (+) 0 (listAbs x)


> main :: IO ()
> main = do

> putStrLn "\n>Testing myFoldl"
> putStrLn $ show $ myFoldl (+) 0 []
> putStrLn $ show $ myFoldl (+) 1 [1,2,3]
> putStrLn $ show $ myFoldl (+) 0 [1,2,3,4,5,6]
> putStrLn $ show $ myFoldl (+) 1 [1,2,3,4,5,6]
> putStrLn $ show $ myFoldl (+) 3 [1,2,3,4,5,6]

> putStrLn "\n>Testing myReverse"
> putStrLn $ show $ myReverse ([]::[Int])
> putStrLn $ show $ myReverse "abracadabra"
> putStrLn $ show $ myReverse [-1,-2,3]

> putStrLn "\n>Testing myFoldr"
> putStrLn $ show $ myFoldr (+) 0 []
> putStrLn $ show $ myFoldr (+) 1 [1,2,3]
> putStrLn $ show $ myFoldr (+) 0 [1,2,3,4,5,6]
> putStrLn $ show $ myFoldr (+) 1 [1,2,3,4,5,6]
> putStrLn $ show $ myFoldr (+) 3 [1,2,3,4,5,6]

> putStrLn "\n>Testing listAbs"
> putStrLn $ show $ listAbs []
> putStrLn $ show $ listAbs [-1]
> putStrLn $ show $ listAbs [-1,-2,3,4,-10,12,-16]

> putStrLn "\n>Testing sumAbs"
> putStrLn $ show $ sumAbs []
> putStrLn $ show $ sumAbs [-1,-2,3]
> putStrLn $ show $ sumAbs [-1,-2,3,4,-10,12,-16]