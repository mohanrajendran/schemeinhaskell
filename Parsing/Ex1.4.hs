import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readHex, readOct)

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
parseNumber = parseNumberWithoutBase <|> parseNumberWithBase
  
parseNumberWithoutBase :: Parser LispVal
parseNumberWithoutBase = liftM (Number . read) $ many1 digit

parseNumberWithBase :: Parser LispVal
parseNumberWithBase = do
                        char '#'
                        base <- oneOf ['b' ,'o', 'd', 'x']
                        case base of
                             'b' -> binRead
                             'o' -> octRead
                             'd' -> parseNumberWithoutBase
                             'x' -> hexRead

binRead :: Parser LispVal
binRead = do
            number <- many $ oneOf ['0', '1']
            return $ Number $ readBin number

readBin :: String -> Integer
readBin b = sum $ zipWith (*) (reverse $ map readBit b) [2^i | i <- [0..]]
        where readBit '0' = 0
              readBit '1' = 1

octRead :: Parser LispVal
octRead = do
            number <- many $ oneOf ['0'..'7']
            return $ Number $ (fst . head) $ readOct number

hexRead :: Parser LispVal
hexRead = do
            number <- many $ oneOf (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])
            return $ Number $ (fst . head) $ readHex number

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escapeChars <|> noneOf ['\\', '"'])
                char '"'
                return $ String x

escapeChars :: Parser Char
escapeChars = do
                char '\\'
                escape <- oneOf ['n', 'r', 't', '\\', '"']
                return $ case escape of
                         'n'  -> '\n'
                         'r'  -> '\r'
                         't'  -> '\t'
                         _    -> escape                         

symbol :: Parser Char
symbol = oneOf "!#$%&*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseNumber <|>
            parseAtom <|>
            parseString
            
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match " ++ show err
  Right val -> "Found value " ++ show val

main :: IO ()
main = do
         args <- getArgs
         putStrLn (readExpr (args !! 0))
