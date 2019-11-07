convertStringList2NumList :: [String] -> [Integer]
convertStringList2NumList = map read

maybeIncrement :: Maybe Int -> Maybe Int
maybeIncrement num = fmap (+1) num

incrementContents :: (Functor f)=> f Int -> f Int
incrementContents box = fmap (+1) box



main = do
    print $ convertStringList2NumList ["1","2","3"]
    print $ convertStringList2NumList []
    print $ convertStringList2NumList ["41","12"]
    print $ maybeIncrement $ Nothing
    print $ maybeIncrement $ Just 9
    print $ maybeIncrement $ Just 99
    print $ incrementContents $ Nothing
    print $ incrementContents $ Just 42
    print $ incrementContents $ [3,2,99]

    
  