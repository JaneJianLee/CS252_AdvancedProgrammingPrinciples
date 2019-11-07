getMax :: [Int] -> Maybe Int
getMax [] = Nothing
getMax (x:xs) = case (getMax xs) of
                  Nothing -> Just x
                  Just y -> if x>y then Just x else Just y



reciprocal :: (Eq a, Fractional a) => a -> Maybe a
reciprocal 0 = Nothing
reciprocal x = Just (1/x)




rectangleArea :: Int -> Int -> Either String Int
rectangleArea x _ | x<0 = Left "Width is not positive"
rectangleArea _ y | y<0 = Left "Height is not positive"
rectangleArea x y | (x>=0 && y>=0) = Right (x*y)

testarea :: Int -> Int -> Either String Int
testarea x y | x<0 || y<0  = Left "Negative"
             | otherwise = Right (x*y)



main :: IO ()
main = do
  print $ getMax []
  print $ getMax [99,12,37]
  print $ getMax [-99,-12,-37]
  print $ reciprocal 4
  print $ reciprocal 2
  print $ reciprocal 0
  print $ rectangleArea 5 10
  print $ rectangleArea (-5) 10
  print $ rectangleArea 5 (-10)
  print $ testarea 5 10
  print $ testarea (-5) 10
  print $ testarea 5 (-10)