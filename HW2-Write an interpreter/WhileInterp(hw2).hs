{-
  Name: Ji An Lee
  Class: CS 252
  Assigment: HW2
  Date: Sept. 26th, 2019
  Description: While interpreter using big-step operational semantics
-}

module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where
        
import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression          -- binary operation
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  | And Expression Expression               -- ∧ :: Bool -> Bool -> Bool
  | Or Expression Expression                -- ∨ :: Bool -> Bool -> Bool
  | Not Expression                          -- ~ :: Bool -> Bool  
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.
applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal j) | (j == 0) = error "You cannot divide by Zero"
                                     | otherwise = IntVal (i `div` j)
applyOp Gt (IntVal i) (IntVal j) = BoolVal (i>j)
applyOp Ge (IntVal i) (IntVal j) = BoolVal (i>=j)
applyOp Lt (IntVal i) (IntVal j) = BoolVal (i<j)
applyOp Le (IntVal i) (IntVal j) = BoolVal (i<=j)

-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)
-- Val Value : (Val (BoolVal True))
evaluate (Val valu) s = (valu, s)
-- Var Variable : (Var "X")
evaluate (Var str) s = case (Map.lookup str s) of
                                Nothing -> error "There isn't a value for key"
                                (Just val) -> (val,s)
-- Assign Variable Expression : (Assign "X" (Val (BoolVal True)))                             
evaluate (Assign str exp) s = (v, s1)
                               where (v,s0) = (evaluate exp s)
                                     s1 = (Map.insert str v s0)
-- Sequence Expression Expression  : (Sequence (Val (IntVal 1)) (Val (IntVal 2)))                                 
evaluate (Sequence exp1 exp2) s = case (evaluate exp1 s) of
                                  (x,s1)->(evaluate exp2 s1)
-- Op Binop Expression Expression :(Op Plus (Val (IntVal 9)) (Val (IntVal 2)))
evaluate (Op o e1 e2) s =
                         let (v1,s1) = evaluate e1 s
                             (v2,s') = evaluate e2 s1
                          in (applyOp o v1 v2, s')
-- If Expression Expression Expression : (If (Val (BoolVal True)) (Val (IntVal 1)) (Val (IntVal 2)))
evaluate (If exp1 exp2 exp3) s = case (evaluate exp1 s) of
                                 (BoolVal True, y) -> (evaluate exp2 y)
                                 (BoolVal False, y) -> (evaluate exp3 y)
-- While Expression Expression : (While (Val (BoolVal False)) (Val (IntVal 42)))
evaluate (While exp1 exp2) s = evaluate (If exp1 (Sequence exp2 (While exp1 exp2)) (Val (BoolVal False))) s
-- And Expression Expression : (And (Val (BoolVal False)) (Val (BoolVal True)))                    
evaluate (And exp1 exp2) s = case (evaluate exp1 s) of
                             ((BoolVal False),_) -> ((BoolVal False),s)
                             otherwise ->(evaluate exp2 s)
-- Or Expression Expression : (Or (Val (BoolVal True)) (Val (BoolVal False)))
evaluate (Or exp1 exp2) s = case (evaluate exp1 s) of
                             ((BoolVal True),_) -> ((BoolVal True),s)
                             otherwise ->(evaluate exp2 s)
-- Not Expression : (NOT (BoolVal False,fromList[]))
evaluate (Not exp) s = case (evaluate exp s) of
                       ((BoolVal True),x) -> (BoolVal False,x)
                       ((BoolVal False),x) -> (BoolVal True,x)

-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog

