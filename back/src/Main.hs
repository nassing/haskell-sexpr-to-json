import Debug.Trace
import Data.Char (isDigit, isSpace)
import Data.List (intersperse)

import Web.Scotty
import Network.Wai.Middleware.Cors

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as E

main :: IO ()
main = scotty 3210 $ do -- Run server on port 3210
    middleware simpleCors -- Enable CORS
    post(capture "/convert") $ do -- Handle POST request to /convert
        sexpr <- body -- Get request body
        let json = convertToJSON . parseSExpr . T.unpack . E.decodeUtf8 $ sexpr -- Convert to JSON
        text (T.pack json) -- Send response

-- S-expression data type
data SExpr = SInt Int
           | SFloat Double
           | SString String
           | SSym String
           | SList [SExpr]
           deriving (Show)


-- Main parsing function
parseSExpr :: String -> SExpr
parseSExpr input
    | head(input) == '(' = SList (parseList(tail (init input))) -- Case of list
    | head(input) == '"' = SString (init(tail input)) -- Case of string
    | all isDigitOrMinus(input) = SInt (read(input)) -- Case of integer
    | isFloat(input) = SFloat (read(input)) -- Case of float
    | otherwise = SSym input -- Case of symbol

isDigitOrMinus :: Char -> Bool
isDigitOrMinus '-' = True
isDigitOrMinus c = isDigit c

isFloat :: String -> Bool
isFloat(str) = case (reads str :: [(Double, String)]) of
                [(n, "")] -> True
                _         -> False

parseList :: String -> [SExpr]
parseList [] = []
parseList s = let (token, rest) = spanToken(s)
              in parseSExpr(token) : parseList(dropWhile isSpace rest)

spanToken :: String -> (String, String)
spanToken [] = ([], [])
spanToken s@(x:xs)
    | x == '(' = spanBrackets(s)
    | x == '"' = spanQuotes(xs)
    | otherwise = break isSpace(s)

spanBrackets :: String -> (String, String)
spanBrackets s = (takeWhile (/= ')') s ++ ")", tail (dropWhile (/= ')') s))

spanQuotes :: String -> (String, String)
spanQuotes s = let (str, rest) = break (== '"') s in ('"':str ++ "\"", tail rest)

convertToJSON :: SExpr -> String
convertToJSON (SInt i) = show(i)
convertToJSON (SFloat f) = show(f)
convertToJSON (SString s) = "\"" ++ s ++ "\""
convertToJSON (SSym "true") = "true"
convertToJSON (SSym "false") = "false"
convertToJSON (SSym "null") = "null"
convertToJSON (SSym sym) = "{\"symbol\": \"" ++ sym ++ "\"}"
convertToJSON (SList items) = "[" ++ (concat . intersperse ", " $ map convertToJSON(items)) ++ "]"