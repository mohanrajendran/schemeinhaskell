import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
               deriving (Show)

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
                
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (doubleQuote <|> noneOf ['\\', '"'])
                char '"'
                return $ String x

parseExpr :: Parser LispVal
parseExpr = parseAtom <|>
            parseString <|>
            parseNumber

doubleQuote :: Parser Char
doubleQuote = do
                string ['\\', '"']
                return $ '"'

symbol :: Parser Char
symbol = oneOf "!#$%&*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match " ++ show err
  Right val -> "Found value " ++ show val

main :: IO ()
main = do
         args <- getArgs
         putStrLn (readExpr (args !! 0))
