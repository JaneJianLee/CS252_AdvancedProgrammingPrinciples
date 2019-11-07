data Exp = ETrue Level
         | EFalse Level
         | ENumber Integer Level
         | Eif Exp Exp Exp Level
         | Succ Exp Level
         | Pred Exp Level
         deriving Show
         
data Val = VTrue Level
         | VFalse Level
         | VNumber Integer Level
         deriving Show

data Level = L
           | H
           deriving Show
           
evaluate :: Exp -> Val
evaluate (ETrue x)= (VTrue x)
evaluate (EFalse x) = (VFalse x) 
evaluate (ENumber n x) = (VNumber n x)
evaluate (Succ (ENumber n x) _)= (VNumber (n+1) x)
evaluate (Pred (ENumber n x) _) = (VNumber (n-1) x)
evaluate (Eif cond expTrue expFalse x) = case (evaluate cond) of
                                     (VTrue H) -> (evaluate expTrue)
                                     (VFalse H) -> (evaluate expFalse)
                                     (VTrue L) -> (evaluate expTrue)
                                     (VFalse L) -> (evaluate expFalse)
                                     _ -> error "Not a Bool"

secret :: Exp -> Exp
secret (ETrue _)= (ETrue H)
secret (EFalse _)= (EFalse H)
secret (ENumber n _) = (ENumber n H)
secret (Succ (ENumber n _) _) = (Succ (ENumber n H) H)
secret (Pred (ENumber n _) _) = (Pred (ENumber n H) H)
secret (Eif cond expTrue expFalse _) = (Eif (secret cond) (secret expTrue) (secret expFalse) H)

                                     
prog1 = ETrue L
prog2 = EFalse L
prog3 = ENumber 5 L
prog4 = Eif (ETrue L) (ETrue L) (EFalse L) L
prog5 = Succ (ENumber 4 L) L
prog6 = Pred (ENumber 5 L) L
prog7 = Eif (Eif (ETrue L) (EFalse L) (ETrue L) L) (ETrue L) (Eif (ETrue L) (EFalse L) (ETrue L) L) L

main :: IO ()
main = do

--Low Level Security Functions:
 putStrLn $ "\n\nTesting Low Level Security Functions \n" 
 putStrLn $ "Evaluating '" ++ (show prog1) ++ "' results in " ++ (show $ evaluate prog1)
 putStrLn $ "Evaluating '" ++ (show prog2) ++ "' results in " ++ (show $ evaluate prog2)
 putStrLn $ "Evaluating '" ++ (show prog3) ++ "' results in " ++ (show $ evaluate prog3)
 putStrLn $ "Evaluating '" ++ (show prog4) ++ "' results in " ++ (show $ evaluate prog4)
 putStrLn $ "Evaluating '" ++ (show prog5) ++ "' results in " ++ (show $ evaluate prog5)
 putStrLn $ "Evaluating '" ++ (show prog6) ++ "' results in " ++ (show $ evaluate prog6)
 putStrLn $ "Evaluating '" ++ (show prog7) ++ "' results in " ++ (show $ evaluate prog7)

 --High Level Security Functions:
 putStrLn $ "\n\nTesting Low Level Security Functions (L->H) \n" 
 putStrLn $ "Evaluating '" ++ (show prog1) ++ "' results in " ++ (show $ secret prog1)
 putStrLn $ "Evaluating '" ++ (show prog2) ++ "' results in " ++ (show $ secret prog2)
 putStrLn $ "Evaluating '" ++ (show prog3) ++ "' results in " ++ (show $ secret prog3)
 putStrLn $ "Evaluating '" ++ (show prog4) ++ "' results in " ++ (show $ secret prog4)
 putStrLn $ "Evaluating '" ++ (show prog5) ++ "' results in " ++ (show $ secret prog5)
 putStrLn $ "Evaluating '" ++ (show prog6) ++ "' results in " ++ (show $ secret prog6)
 putStrLn $ "Evaluating '" ++ (show prog7) ++ "' results in " ++ (show $ secret prog7)

  --High Level Security Functions + Evaluate:
 putStrLn $ "\n\nTesting secret + evaluate function \n" 
 putStrLn $ "Evaluating '" ++ (show prog1) ++ "' results in " ++ (show $ evaluate (secret prog1))
 putStrLn $ "Evaluating '" ++ (show prog2) ++ "' results in " ++ (show $ evaluate (secret prog2))
 putStrLn $ "Evaluating '" ++ (show prog3) ++ "' results in " ++ (show $ evaluate (secret prog3))
 putStrLn $ "Evaluating '" ++ (show prog4) ++ "' results in " ++ (show $ evaluate (secret prog4))
 putStrLn $ "Evaluating '" ++ (show prog5) ++ "' results in " ++ (show $ evaluate (secret prog5))
 putStrLn $ "Evaluating '" ++ (show prog6) ++ "' results in " ++ (show $ evaluate (secret prog6))
 putStrLn $ "Evaluating '" ++ (show prog7) ++ "' results in " ++ (show $ evaluate (secret prog7))
 
 
 
 
 