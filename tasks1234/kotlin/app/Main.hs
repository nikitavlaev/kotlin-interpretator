module Main where

import Text.Parsec

data Expr = NOP
    | Jmp {currentExpr :: Expr, nextExpr :: Expr}
    | JmpIf {condition :: Expr, trueExpr :: Expr, falseExpr :: Expr}
    | VarInit {name :: String, ktype :: KType}
    | VarGet {name :: String}
    | VarSet {name :: String, value :: Expr}
    | VarGetIndex {name :: String, index :: Expr}
    | VarSetIndex {name :: String, index :: Expr, value :: Expr}
    | Val KData
    | BeginFunction {name :: String}
    | EndFunction
    | GenerateException
    | Read
    | Write {message :: Expr}
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | Not Expr
    | Less Expr Expr
    | Equal Expr Expr
    deriving Show

data KType = KTVoid
    | KTBool
    | KTChar
    | KTInt
    | KTDouble
    | KTArray KType
    | KTMaybe KType
    | KTRecord [(String, KType)]
    | KTUnknown
    deriving Show

data KData = KDVoid
    | KDNull
    | KDBool Bool
    | KDChar Char
    | KDInt Int
    | KDDouble Double
    | KDArray [KData]
    | KDRecord [(String, KData)]
    deriving Show

data Label = LabelLoop {breakExpr :: Expr, continueExpr :: Expr} deriving Show

data Object = Variable {objName :: String, objKType :: KType} | Value {objName :: String, objKType :: KType} deriving Show

data Function = Function {funcName :: String, funcArgs :: [Object], begining :: Expr} deriving Show

data Type = Type {typeName :: String, typeDefine :: KType} deriving Show

data ParserState = ParserState {labels :: [Label], objects :: [Object], functions :: [Function], types :: [Type]} deriving Show

type Parser = Parsec String ParserState

parserTest :: Parser a -> String -> Either ParseError a
parserTest parser input = runParser parser (ParserState [] [] [] []) "" input

parseInt :: Parser Expr
parseInt = (Val . KDInt . read) <$> try ((++) <$> (string "-" <* spaces <|> pure "") <*> many1 digit)

parseDouble :: Parser Expr
parseDouble = (Val . KDDouble . read) <$> try ((\a b c d -> a ++ b ++ c ++ d) <$> (string "-" <* spaces <|> pure "") <*> many1 digit <*> string "." <*> many1 digit)

parseString :: Parser Expr
parseString = (Val . KDArray . fmap KDChar) <$> between (char '"') (char '"') (many $ char '\\' *> (char '"' <|> do {char 'n'; return '\n'}) <|> satisfy (\c -> c /= '"' && c /= '\\'))

parseChar :: Parser Expr
parseChar = (Val . KDChar) <$> between (char '\'') (char '\'') (satisfy (/= '\\') <|> char '\\' *> (oneOf ['"', '\''] <|> do {char 'n'; return '\n'}))

parseBool :: Parser Expr
parseBool = do { try $ string "true"; return $ Val $ KDBool True } <|> do { try $ string "false"; return $ Val $ KDBool False }

parseNull :: Parser Expr
parseNull = do { try $ string "null"; return $ Val KDNull }

parseVoid :: Parser Expr
parseVoid = do { try $ string "void"; return $ Val KDVoid }

parseValue :: Parser Expr
parseValue = parseDouble <|> parseInt <|> parseString <|> parseChar <|> parseBool <|> parseNull <|> parseVoid <|> parseInparens

parseInparens :: Parser Expr
parseInparens = char '(' *> spaces *> parseExpr <* spaces <* char ')'

parseOperator :: (Expr -> Expr -> Expr) -> Parser Expr -> String -> Parser Expr -> Parser Expr
parseOperator f p1 op p2 = try $ (f <$> p1 <* spaces <* string op <* spaces <*> p2)

parseExpr :: Parser Expr
parseExpr = parseOr

parseOr :: Parser Expr
parseOr = parseOperator Or parseAnd "||" parseOr <|> parseAnd

parseAnd :: Parser Expr
parseAnd = parseOperator And parseEquation "&&" parseAnd <|> parseEquation

parseEquation :: Parser Expr
parseEquation = parseOperator Equal parseAddSub "==" parseAddSub <|> parseOperator (\x y -> Not $ Equal x y) parseAddSub "!=" parseAddSub <|> parseOperator Less parseAddSub "<" parseAddSub <|> parseOperator (flip Less) parseAddSub ">" parseAddSub <|> parseOperator (\x y -> Not $ Less y x) parseAddSub "<=" parseAddSub <|> parseOperator (\x y -> Not $ Less x y) parseAddSub ">=" parseAddSub <|> parseAddSub

parseAddSub :: Parser Expr
parseAddSub = parseOperator Add parseMulDivMod "+" parseAddSub <|> parseOperator Sub parseMulDivMod "-" parseAddSub <|> parseMulDivMod

parseMulDivMod :: Parser Expr
parseMulDivMod = parseOperator Mul parseNot "*" parseMulDivMod <|> parseOperator Div parseNot "/" parseMulDivMod <|> parseOperator Mod parseNot "%" parseMulDivMod <|> parseNot

parseNot :: Parser Expr
parseNot = string "!" *> spaces *> (Not <$> parseFunction) <|> parseFunction

parseFunction :: Parser Expr
parseFunction = parseValue

main :: IO ()
main = do
    print "Hello world!"
