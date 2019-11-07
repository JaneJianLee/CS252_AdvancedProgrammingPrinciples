data Binop =
     Plus     -- +  :: Int  -> Int  -> Int
   | Minus    -- -  :: Int  -> Int  -> Int
   | Times    -- *  :: Int  -> Int  -> Int
   | Divide   -- /  :: Int  -> Int  -> Int
   deriving (Show)

applyOp :: Binop -> Maybe Int -> Maybe Int -> Maybe Int
applyOp Plus mi mj =
   case mi of
     Nothing -> Nothing
     Just i -> case mj of
               Nothing -> Nothing
               Just j -> Just $ i + j

applyOp Minus mi mj = mi >>= (\i -> mj >>= (\j -> Just $ i - j))

applyOp Times mi mj = case mi of
                      Nothing -> Nothing
                      Just i -> case mj of
                                Nothing -> Nothing
                                Just j -> Just $ i * j

applyOp Divide mi mj = mi >>= (\i -> mj >>= (\j -> (if j>0 then (Just $ i `div` j) else Nothing)))


applyOp' :: Binop -> Maybe Int -> Maybe Int -> Maybe Int
applyOp' Plus mi mj = do
  i <- mi
  j <- mj
  return $ i + j
applyOp' Minus mi mj = do
  i <- mi
  j <- mj
  return $ i - j
applyOp' Times mi mj = do
  i <- mi
  j <- mj
  return $ i * j
applyOp' Divide mi mj  = do
  i <- mi
  j <- mj
  if j>0 then return $ i `div` j else Nothing

testapp1' = applyOp' Minus (applyOp' Times (Just 3) (Just 4)) $ applyOp' Divide (Just 8) (Just 2)
testapp2' = applyOp' Minus (applyOp' Times (Just 3) (Just 4)) $ applyOp' Divide (Just 8) (applyOp' Plus (Just 3) (Just (-3)))


testapp1 = applyOp Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (Just 2)
testapp2 = applyOp Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (applyOp Plus (Just 3) (Just (-3)))

main :: IO ()
main = do
    putStrLn $ "Test1 " ++ (show testapp1)
    putStrLn $ "Test2 " ++ (show testapp2)
    putStrLn $ "Test1' " ++ (show testapp1')
    putStrLn $ "Test2' " ++ (show testapp2')