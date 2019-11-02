module Main where

    --TODO: Casts, Iterable, for first, matrix[1][2] = 3, Collections, When
    import Text.Parsec hiding (spaces)
    import Data.Functor.Identity
    
    data Expr = NOP
        -- Вроде Какаду говорил что нужно без джампов реализовывать, иначе будут проблемы
        -- | Jmp {currentExpr :: Expr, nextExpr :: Expr}
        -- | JmpIf {condition :: Expr, trueExpr :: Expr, falseExpr :: Expr}
        | VarSet {lvalue :: LValue, rvalue :: RValue}
        | RValue
        | LValue
        | Val KData
        | If {cond :: Cond, th :: [Expr], els :: [Expr]}
        | When {}
        | Cond
        | While {cond :: Cond, body :: [Expr]}
        | DoWhile {cond :: Cond, body :: [Expr]}
        | For {counter :: String, iterable :: KData, body :: [Expr]} --В Котлине есть только for как foreach
        | Return
        | Throw {e :: RuntimeException}
        | Read -- что это?
        | Write {message :: Expr}
        | Break -- Как реализовать без jmp?
        | Continue --
        deriving Show

    data Cond = Bool
        | Function {name :: String, args :: [Object]}
        | Add Cond Cond
        | And Cond Cond
        | Or Cond Cond
        | Xor Cond Cond
        | Not Cond
        | Less Cond Cond
        | Equal Cond Cond
        deriving Show
    
    data RuntimeException = String --Возможно еще что-то должно быть

    data LValue = VarGet {name :: String}
        | VarInit {name :: String, ktype :: KType}
        | Index {array :: KDArray, index :: RValue}
        deriving Show

    data RValue = Val KData
        | VarGet {name :: String}
        | Index {array :: KDArray, index :: RValue}
        | Add RValue RValue
        | Mul RValue RValue
        | Sub RValue RValue
        | Div RValue RValue
        | Mod RValue RValue
        | And RValue RValue
        | Or RValue RValue
        | Not RValue
        | Less RValue RValue
        | Equal RValue RValue
        | Function {name :: String, args :: [Object]}
        deriving Show

    data KType = KTVoid
        | KTBool
        | KTChar   
        | KTNum
        | KTIterable
        | KTArray KType
        | KTMaybe KType
        | KTRecord [(String, KType)]
        | KTUnknown
        deriving Show

    data KTIterable = KTIterable {next :: KData, hasNext :: Bool} 
    -- Вообще hasNеxt это функция, но я чет не знаю как это правильно описать

    data KTNum = KTByte
        | KTInt
        | KTLong
        | KTDouble
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
    
    data Function = Function {funcName :: String, funcArgs :: [Object], beginning :: Expr} deriving Show 
    
    data Type = Type {typeName :: String, typeDefine :: KType} deriving Show
    
    data ParserState = ParserState {labels :: [Label], objects :: [Object], functions :: [Function], types :: [Type]} deriving Show
    
    type Parser = Parsec String ParserState
    
    parserTest :: Parser Expr -> String -> Either ParseError Expr
    parserTest parser input = takeExpr 5 $ runParser parser (ParserState [] [] [] []) "" input
    
    takeExpr :: Int -> Either ParseError Expr -> Either ParseError Expr
    takeExpr _ (Left error) = Left error
    takeExpr n (Right expr) = Right $ showExpr n expr where
        showExpr 0 _ = Read
        showExpr n (Jmp ce ne) = Jmp (showExpr (n - 1) ce) (showExpr (n - 1) ne)
        showExpr n (JmpIf c te fe) = JmpIf (showExpr (n - 1) c) (showExpr (n - 1) te) (showExpr (n - 1) fe)
        showExpr n (Val d) = Val d
        showExpr n NOP = NOP
        showExpr n (Add l r) = Add (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (Sub l r) = Sub (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (Mul l r) = Mul (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (Div l r) = Div (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (Mod l r) = Mod (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (Or l r) = Or (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (And l r) = And (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (Less l r) = Less (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (Equal l r) = Equal (showExpr (n - 1) l) (showExpr (n - 1) r)
        showExpr n (Not e) = Not (showExpr (n - 1) e)
    
    spaces :: Parser ()
    spaces = skipMany $ char ' '
    
    separator :: Parser ()
    separator = spaces *> (newline <|> pure ' ') *> spaces
    
    semicolon :: Parser ()
    semicolon = spaces *> (char ';' *> separator <|> newline *> spaces)
    
    parseInt :: Parser Expr
    parseInt = (Val . KDInt . read) <$> try ((++) <$> (string "-" <* spaces <|> pure "") <*> many1 digit)
    
    parseDouble :: Parser Expr
    parseDouble = (Val . KDDouble . read) <$> try ((\a b c d -> a ++ b ++ c ++ d) <$> (string "-" <* spaces <|> pure "") <*> many1 digit <*> string "." <*> many1 digit)
    
    parseString :: Parser Expr
    parseString = (Val . KDArray . fmap KDChar) <$> between (char '"') (char '"') (many $ char '\\' *> (char '"' <|> do {char 'n'; return '\n'}) <|> satisfy (\c -> c /= '"' && c /= '\\'))
    
    parseChar :: Parser Expr
    parseChar = (Val . KDChar) <$> between (char '\'') (char '\'') (satisfy (/= '\\') <|> char '\\' *> (oneOf ['"', '\''] <|> do {char 'n'; return '\n'}))
    
    parseBool :: Parser Expr
    parseBool = do { string "true"; return $ Val $ KDBool True } <|> do { string "false"; return $ Val $ KDBool False }
    
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
    parseFunction = parseVariable
    
    parseVariable :: Parser Expr
    parseVariable = parseIf
    
    parseIf :: Parser Expr
    parseIf = try $ JmpIf <$> (string "if" *> spaces *> parseInparens) <* separator <*> parseBlock <*> (separator *> string "else" *> separator *> parseBlock <|> pure NOP) <|> parseValue
    
    parseBlock :: Parser Expr
    parseBlock = char '{' *> separator *> parseManyPrimitives <* separator <* char '}' <|> parseExpr
    
    parseOnePrimitiv :: Parser Expr
    parseOnePrimitiv = parseExpr <|> parseWhile
    
    parseManyPrimitives :: Parser Expr
    parseManyPrimitives = Jmp <$> parseOnePrimitiv <*> (semicolon *> parseManyPrimitives <|> pure NOP)
    
    parseWhile :: Parser Expr
    parseWhile = do
        string "while"
        spaces
        cond <- parseInparens
        separator
        body <- parseBlock
        let loop = JmpIf cond (Jmp body loop) NOP
        return loop
    
    main :: IO ()
    main = do
        putStrLn "Hello world!"
        test <- getLine
        print $ parserTest parseExpr test
    