--test :: [Int] -> [(Int->Int)]
test :: [Int] -> [(Int->Int)]
test = fmap (+)
adders = test [1,2]

main :: IO ()
main = do
    print $ (head adders) 10
    print $ (head (tail adders)) 13