data Exp = ETrue
         | EFalse
         | Eif Exp Exp Exp
         | ENumber Integer
         | Succ Exp
         | Pred Exp
         deriving Show

data Val = VTrue
         | VFalse
         | VNumber Integer
         deriving Show

evaluate :: Exp -> Val
evaluate ETrue = VTrue
evaluate EFalse = VFalse
evaluate (ENumber n) = (VNumber n)
evaluate (Succ (ENumber n)) = VNumber (n+1)
evaluate (Pred (ENumber n)) = VNumber (n-1)
evaluate (Eif cond expTrue expFalse) = case (evaluate cond) of
                                     VTrue -> (evaluate expTrue)
                                     VFalse -> (evaluate expFalse)
                                     _ -> error "Not a Bool"

prog1 = Eif ETrue ETrue EFalse
prog2 = Eif (Eif ETrue EFalse ETrue) ETrue (Eif ETrue EFalse ETrue)
prog3 = (ENumber 3)
prog4 = Succ (ENumber 4)
prog5 = Pred (ENumber 5)


main :: IO ()
main = do
 putStrLn $ "Evaluating '" ++ (show prog1) ++ "' results in " ++ (show $ evaluate prog1)
 putStrLn $ "Evaluating '" ++ (show prog2) ++ "' results in " ++ (show $ evaluate prog2)
 putStrLn $ "Evaluating '" ++ (show prog3) ++ "' results in " ++ (show $ evaluate prog3)
 putStrLn $ "Evaluating '" ++ (show prog4) ++ "' results in " ++ (show $ evaluate prog4)
 putStrLn $ "Evaluating '" ++ (show prog5) ++ "' results in " ++ (show $ evaluate prog5)