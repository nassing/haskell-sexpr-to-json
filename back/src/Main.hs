import Debug.Trace
import Data.Char (isDigit, isSpace)
import Data.List (intersperse)

import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Status (status200, status400)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as E


-- S-expression data type
data SExpr = SInt Int
           | SFloat Double
           | SString String
           | SSym String
           | SList [SExpr]
           deriving (Show)


main :: IO ()
main = scotty 3210 $ do -- Run server on port 3210
    middleware simpleCors -- Enable CORS
    post(capture "/convert") $ do -- Handle POST request to /convert
        sexpr <- body -- Get request body
        case parseSExpr . T.unpack . E.decodeUtf8 $ sexpr of
            Just json -> do -- If parsing was successful
                status status200
                text (T.pack (convertToJSON json))  -- Send JSON response
            Nothing -> do -- If parsing failed
                status status400
                text (T.pack "\"Invalid input\"")


-- Main parsing function
parseSExpr :: String -> Maybe SExpr
parseSExpr input
    | null input = Nothing -- Case of empty input
    | head input == '(' =
        let content = tail (init input) in
        if not (null content) && head input == '(' && last input == ')'
        then SList <$> parseList content -- Case of list
        else Nothing
    | head input == '"' = 
        if last input == '"' 
        then Just $ SString (init (tail input)) 
        else Nothing -- Case of string
    | all isDigitOrMinus input = Just $ SInt (read input) -- Case of integer
    | isFloat input = Just $ SFloat (read input) -- Case of float
    | otherwise = 
        if all isSymbol input then Just $ SSym input else Nothing -- Case of symbol



-- Parses S-expressions to JSON
convertToJSON :: SExpr -> String
convertToJSON (SInt i) = show(i)
convertToJSON (SFloat f) = show(f)
convertToJSON (SString s) = "\"" ++ s ++ "\""
convertToJSON (SSym "true") = "true"
convertToJSON (SSym "false") = "false"
convertToJSON (SSym "null") = "null"
convertToJSON (SSym sym) = "{\"symbol\": \"" ++ sym ++ "\"}"
convertToJSON (SList items) = "[" ++ (concat . intersperse ", " $ map convertToJSON(items)) ++ "]"


isDigitOrMinus :: Char -> Bool
isDigitOrMinus '-' = True
isDigitOrMinus c = isDigit c

isFloat :: String -> Bool
isFloat(str) = case (reads str :: [(Double, String)]) of
                [(n, "")] -> True
                _         -> False

isSymbol :: Char -> Bool
isSymbol c = not (c `elem` ['(', ')', '"', '\\'])


parseList :: String -> Maybe [SExpr]
parseList [] = Just []
parseList s = do
    let (token, rest) = spanToken s
    parsedToken <- parseSExpr token
    parsedRest <- parseList (dropWhile isSpace rest)
    return (parsedToken : parsedRest)

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
