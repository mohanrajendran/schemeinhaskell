module Main where
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        putStrLn (addStrings (args !! 0) (args !! 1))

stringToInt :: String -> Int
stringToInt = read

addStrings :: String -> String -> String
addStrings a b = show (stringToInt a + stringToInt b)
