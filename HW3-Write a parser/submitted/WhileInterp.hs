{-
  Name: Ji An Lee
  Class: CS 252
  Assigment: HW3
  Date: 10/21/2019
  Description: <Describe the program and what it does>
-}

module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  runFile,
  showParsedExp,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except

-- We represent variables as strings.
type Variable = String

--We also represent error messages as strings.
type ErrorMsg = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3 endif
  | While Expression Expression             -- while e1 do e2 endwhile
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

-- .imp file parser
fileP :: GenParser Char st Expression
fileP = do
  prog <- exprP
  eof
  return prog

-- expression parser
exprP = do
  e <- exprP'
  rest <- optionMaybe restSeqP
  return (case rest of
    Nothing   -> e
    Just e' -> Sequence e e')

-- Expressions are divided into terms and expressions for the sake of
-- parsing.  Note that binary operators **DO NOT** follow the expected
-- presidence rules.
--
-- ***FOR 2pts EXTRA CREDIT (hard, no partial credit)***
-- Correct the precedence of the binary operators.
exprP' = do
  spaces
  t <- termP
  spaces
  rest <- optionMaybe restP
  spaces
  return (case rest of
    Nothing   -> t
    Just (":=", t') -> (case t of
      Var varName -> Assign varName t'
      _           -> error "Expected var")
    Just (op, t') -> Op (transOp op) t t')

restSeqP = do
  char ';'
  exprP

-- parses texts (operators) -> haskell func
transOp s = case s of
  "+"  -> Plus
  "-"  -> Minus
  "*"  -> Times
  "/"  -> Divide
  ">=" -> Ge
  ">"  -> Gt
  "<=" -> Le
  "<"  -> Lt
  o    -> error $ "Unexpected operator " ++ o

-- Some string, followed by an expression
restP = do
  ch <- string "+"
    <|> string "-"
    <|> string "*"
    <|> string "/"
    <|> try (string "<=")
    <|> string "<"
    <|> try (string ">=")
    <|> string ">"
    <|> string ":=" -- not really a binary operator, but it fits in nicely here.
    <?> "binary operator"
  e <- exprP'
  return (ch, e)

-- All terms can be distinguished by looking at the first character
termP = valP
    <|> ifP
    <|> whileP
    <|> parenP
    <|> varP
    <?> "value, variable, 'if', 'while', or '('"


valP = do
  v <- boolP <|> numberP
  return $ Val v

-- Parses from text : boolean values
boolP = do
  bStr <- string "true" <|> string "false" <|> string "skip"
  return $ case bStr of
    "true" -> BoolVal True
    "false" -> BoolVal False
    "skip" -> BoolVal False -- Treating the command 'skip' as a synonym for false, for ease of parsing

-- Parses from text : numbers
numberP = do
    num <- many1 digit
    return $ IntVal (read num)

-- Parses from text : variables 
varP = do
    var1 <- many1 letter
    var2 <- many (digit <|> char '_')
    return $ Var (var1++var2)
    
-- Parses from text : IF
ifP = do
-- if e1 then e2 else e3 endif
-- the terms in if block are going to be from the set
-- {if, then, else, endif}
  spaces
  string "if"
  e1 <- exprP
  string "then"
  e2 <- exprP
  string "else"
  e3 <- exprP
  string "endif"
  return $ If e1 e2 e3

-- Parses from text : WHILE
whileP = do
    spaces
    string "while"
    e1 <- exprP
    string "do"
    e2 <- exprP
    string "endwhile"
    return $ While e1 e2

-- Parses from text : parenthesis (9-5)*2
parenP = do
    spaces
    char '('
    e1 <- exprP
    char ')'
    rest <- optionMaybe restP
    case rest of
        Nothing -> return e1
        Just (":=", e2) -> case e1 of
          Var var1 -> return (Assign var1 e2)
          _ -> error "error assigning variable"
        Just (otherop,e2) -> return (Op (transOp otherop) e1 e2)

-- This function will be useful for defining binary operations.
-- Unlike in the previous assignment, this function returns an "Either value".
-- The right side represents a successful computaton.
-- The left side is an error message indicating a problem with the program.
-- The first case is done for you.
applyOp :: Binop -> Value -> Value -> Either ErrorMsg Value
applyOp Plus (IntVal i) (IntVal j) = Right $ IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = Right $ IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = Right $ IntVal $ i * j
applyOp Divide (IntVal i) (IntVal 0) = Left "Error, Can't divide by 0."
applyOp Divide (IntVal i) (IntVal j) = Right $ IntVal $ div i j
applyOp Gt (IntVal i) (IntVal j) = Right $ BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = Right $ BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = Right $ BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = Right $ BoolVal $ i <= j
applyOp _ _ _ = Left "Error applying operand. (Not supported)"

-- As with the applyOp method, the semantics for this function
-- should return Either values.  Left <error msg> indicates an error,
-- whereas Right <something> indicates a successful execution.
evaluate :: Expression -> Store -> Either ErrorMsg (Value, Store)
-- Op:Default
evaluate (Op o (Val v1) (Val v2)) s = do
    v <- applyOp o v1 v2
    return (v, s)

evaluate (Op Times (Val v1) (Op o1 e1 e2)) s1 = do
    (v2,s2) <- evaluate (Op Times (Val v1) e1) s1
    (v3,s3) <- evaluate (Op o1 (Val v2) e2) s2
    return (v3,s3)
    
evaluate (Op Divide (Val v1) (Op o1 e1 e2)) s1 = do
    (v2,s2) <- evaluate (Op Divide (Val v1) e1) s1
    (v3,s3) <- evaluate (Op o1 (Val v2) e2) s2
    return (v3,s3)

evaluate (Op o (Val v1) e1) s1 = do
    (v2, s2) <- evaluate e1 s1
    value <- applyOp o v1 v2
    return (value,s2)

evaluate (Op o e1 e2) s = do
  (v1,s1) <- evaluate e1 s
  (v2,s') <- evaluate e2 s1
  v <- applyOp o v1 v2
  return (v, s')

-- Val
evaluate (Val v) s = return (v,s)
-- Var
evaluate (Var x) s = do
    case (Map.lookup x s) of
        Nothing -> Left "There isn't a value for key"
        Just val -> return (val,s)
-- Assign
evaluate (Assign x e1) s = do
    (val,s1) <- evaluate e1 s
    return (val, (Map.insert x val s1))

-- Sequence
evaluate (Sequence e1 e2) s = do
    (v1,s1) <- evaluate e1 s
    (v2,s2) <- evaluate e2 s1
    return (v2,s2)

-- If
evaluate (If e1 e2 e3) s = do
    (v1,s1) <- evaluate e1 s
    case (v1) of
      BoolVal True -> (evaluate e2 s1) 
      BoolVal False-> (evaluate e3 s1)
      _ -> Left $ "Non-boolean value '" ++ show v1 ++ "' used as a conditional"

-- While
evaluate (While e1 e2) s = (evaluate (If e1 (Sequence e2 (While e1 e2)) (Val (BoolVal False))) s)

-- Evaluates a program with an initially empty state
run :: Expression -> Either ErrorMsg (Value, Store)
run prog = evaluate prog Map.empty

showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp

runFile fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s) -> print $ show s



