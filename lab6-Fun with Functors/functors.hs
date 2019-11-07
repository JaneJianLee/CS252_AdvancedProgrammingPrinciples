data Tree v =Empty
             | Node v (Tree v) (Tree v)
             deriving (Show,Eq,Ord)

findT :: Ord v => v -> Tree v -> Maybe v
findT _ Empty = Nothing
findT v (Node val left right) = if val == v then
                                    Just val
                                else if v < val then
                                    findT v left
                                    else
                                    findT v right


instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node val left right) = (Node (f val) (fmap f left) (fmap f right))

main :: IO ()
main = do
    print $ fmap (+1) (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))
    print $ fmap (*4) (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))
    --print $ findnew 4 (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))
    print $ findT 4 (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))
    print $ findT 9 (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))
    
    
