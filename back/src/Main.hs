import Debug.Trace
import Data.Char (isDigit, isSpace)
import Data.List (intersperse)

import Web.Scotty

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as E

main :: IO ()
main = scotty 3000 $ do
    post (capture "/convert") $ do
        sexpr <- body
        let json = convertToJSON . parseSExpr . T.unpack . E.decodeUtf8 $ sexpr
        text (T.pack json)

data SExpr = SInt Int
           | SFloat Double
           | SString String
           | SSym String
           | SList [SExpr]
           deriving (Show)

parseSExpr :: String -> SExpr
parseSExpr input
    | head input == '(' = SList (parseList (tail (init input)))
    | head input == '"' = SString (init (tail input))
    | all isDigitOrMinus input = SInt (read input)
    | isFloat input = SFloat (read input)
    | otherwise = SSym input

isDigitOrMinus :: Char -> Bool
isDigitOrMinus c = isDigit c || c == '-'

isFloat :: String -> Bool
isFloat str = case (reads str :: [(Double, String)]) of
                [(n, "")] -> True
                _         -> False

parseList :: String -> [SExpr]
parseList [] = []
parseList s = let (token, rest) = spanToken s
              in parseSExpr token : parseList (dropWhile isSpace rest)

spanToken :: String -> (String, String)
spanToken [] = ([], [])
spanToken s@(x:xs)
    | x == '(' = spanBrackets s
    | x == '"' = spanQuotes xs
    | otherwise = break isSpace s

spanBrackets :: String -> (String, String)
spanBrackets s = (takeWhile (/= ')') s ++ ")", tail (dropWhile (/= ')') s))

spanQuotes :: String -> (String, String)
spanQuotes s = let (str, rest) = break (== '"') s in ('"':str ++ "\"", tail rest)

convertToJSON :: SExpr -> String
convertToJSON (SInt i) = show i
convertToJSON (SFloat f) = show f
convertToJSON (SString s) = "\"" ++ s ++ "\""
convertToJSON (SSym "true") = "true"
convertToJSON (SSym "false") = "false"
convertToJSON (SSym "null") = "null"
convertToJSON (SSym sym) = "{\"symbol\": \"" ++ sym ++ "\"}"
convertToJSON (SList elems) = "[" ++ (concat . intersperse ", " $ map convertToJSON elems) ++ "]"