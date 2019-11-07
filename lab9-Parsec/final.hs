import Text.ParserCombinators.Parsec
import System.Environment

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            | JInteger Integer
  deriving (Eq, Ord, Show, Read)


jsonFile :: GenParser Char st JValue
jsonFile = do
  result <- jsonObject
  spaces
  eof
  return result

jsonElem :: GenParser Char st JValue
jsonElem = do
  spaces
  result <- jsonElem'
  spaces
  return result

jsonElem' = jsonObject
        <|> jsonArr
        <|> jsonString
        <|> jsonNum
        <|> jsonBool
        <|> jsonNull
        <?> "json element"

jsonString :: GenParser Char st JValue
jsonString = jsonStringDQ <|> jsonStringSQ

jsonStringDQ = do
  char '"'
  s <- many $ noneOf "\"" -- crude.  does not allow double quotes within strings
  char '"'
  return $ JString s

jsonStringSQ = do
  char '\''
  s <- many $ noneOf "'" -- crude, same as above
  char '\''
  return $ JString s
  
jsonBool = do
  bStr <- string "true" <|> string "false"
  return $ case bStr of
    "true" -> JBool True
    "false" -> JBool False

jsonNull = do
  string "null"
  return JNull

jsonNum = do
  intnum <- many1 digit
  return $ JInteger (read intnum)

jsonObject = do
  char '{'
  pair <- jsonPair `sepBy` (char ',')
  char '}'
  return $ JObject pair

jsonPair :: GenParser Char st (String, JValue)
jsonPair = do
  spaces
  string <- many1 $ noneOf ":"
  spaces
  char ':'
  jval <- jsonElem
  spaces
  return (string,jval)

jsonArr = do
  char '['
  spaces
  arr <- jsonElem `sepBy` (char ',')
  spaces
  char ']'
  return $ JArray arr

parseJSON :: String -> Either ParseError JValue
parseJSON input = parse jsonFile "(unknown)" input

prettyPrint :: JValue -> String
prettyPrint (JNull) = "JNull"
prettyPrint (JBool True) = "JBool True"
prettyPrint (JBool False) = "JBool False"
prettyPrint (JString str) = "JString " ++ str
prettyPrint (JInteger int) = "JInteger " ++ (show int)
prettyPrint (JNumber num) = "JNumber" ++ (show num)
prettyPrint (JObject obj) = "JObject {" ++ "\n" ++ (objstr obj) ++ "}"
prettyPrint (JArray arr) = "JArray [" ++ "\n" ++ (arrstr arr) ++ "]"

arrstr :: [JValue] -> String
arrstr [] = ""
arrstr (x:[]) = (prettyPrint x)
arrstr (x:xs) = (prettyPrint x) ++ ",\n" ++ (arrstr xs)

objstr :: [(String, JValue)] -> String
objstr [] = ""
objstr (x:[]) = ( "( \""++fst(x)++"\"" ++ " : " ++ prettyPrint (snd(x)) ++")")
objstr (x:xs) = ( "( \""++fst(x)++"\"" ++ " : " ++ prettyPrint (snd(x)) ) ++ "),\n" ++ (objstr xs)


main = do
  args <- getArgs
  p <- parseFromFile jsonFile (head args)
  case p of
    Left err  -> print err
    Right json -> putStrLn $ prettyPrint json


