{-# LANGUAGE DuplicateRecordFields #-}

module Main where

    --TODO: Casts, Iterable, for first, matrix[1][2] = 3, Collections, When
    import Text.Parsec hiding (spaces, sepBy)
    import Data.Functor.Identity
    
{-    class LValue code

    class RValue code

    class Cond code

    class Expr code

    class Line code

    data NOP = NOP

    data Operation =
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        | Mod Expr Expr
        | And Expr Expr
        | Or Expr Expr
        | Xor Expr Expr
        | Not Expr
        | Less Expr Expr
        | Equal Expr Expr
        deriving Show

    data If cond line = If {ifCond :: cond, ifThen :: [line], ifElse :: [line]}

    data While cond line = While {whileCond :: cond, body :: [line]}

    data ForEach -}

{-    data LValue = VarGet {name :: String}
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

    data Primitive = Nop
        | Assignment {lvalue :: LValue, rvalue :: RValue}
        | Expression Expr

  -} {- data Expr = NOP
        | Read
        | Jmp Expr Expr
        | JmpIf Expr Expr Expr
        | Val KData
        | Add Expr Expr
        | Mul Expr Expr
        | Sub Expr Expr
        | Div Expr Expr
        | Mod Expr Expr
        | And Expr Expr
        | Or Expr Expr
        | Not Expr
        | Less Expr Expr
        | Equal Expr Expr        -- Вроде Какаду говорил что нужно без джампов реализовывать, иначе будут проблемы
        -- | Jmp {currentExpr :: Expr, nextExpr :: Expr}
        -- | JmpIf {condition :: Expr, trueExpr :: Expr, falseExpr :: Expr}
-}{-        | VarSet {lvalue :: LValue, rvalue :: RValue}
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
        | Continue --}
  --      deriving Show

   {- data Cond = Bool
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
-}

    data Primitiv = FunInit Fun | ClassInit Class deriving Show

    data Fun = Fun {name :: String, args :: [Variable], returnType :: KType, body :: [FunPrimitiv]} deriving Show

    data Class = Class {name :: String, fields :: [Variable], functions :: [Fun]} deriving Show

    data FunPrimitiv = 
          Expression Expr
        | ValInit {name :: String, ktype :: KType} -- соответствует val <valName> : <valType>
        | VarInit {name :: String, ktype :: KType} -- соответствует var <valName> : <valType>
        | While {cond :: Expr, body :: [FunPrimitiv]} -- соответствует while (<cond>) {<body>} и do {<body>} while (<cond>)
        | For {iterVal :: String, collection :: KData, body :: [FunPrimitiv]} -- соответствует for (<iterVal> in <collection>) {<body>}
        | Break
        | Continue
        | Return {result :: Expr}
        deriving Show

    data Expr =
          Val KData
        | Var String
        | CallFun {name :: String, fargs :: [Expr]}
        | Lambda {largs :: [Variable], body :: [FunPrimitiv]}
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        | Mod Expr Expr
        | And Expr Expr
        | Or Expr Expr
        | Not Expr
        | Less Expr Expr
        | Equal Expr Expr
        | If {cond :: Expr, thenBranch :: [FunPrimitiv], elseBranch :: [FunPrimitiv]}
        deriving Show

    data KType = KTUnit
        | KTBool
        | KTChar
        | KTByte
        | KTShort
        | KTInt
        | KTLong
        | KTDouble
        | KTArray KType
        | KTNullable KType
        | KTUserType String
        | KTAny
        deriving Show

        -- | KTIterable

    -- data KTIterable = KTIterable {next :: KData, hasNext :: Bool} 
    -- Вообще hasNеxt это функция, но я чет не знаю как это правильно описать

    data KData = KDUnit
        | KDNull
        | KDBool Bool
        | KDChar Char
        | KDInt Int
        | KDDouble Double
        | KDArray [KData]
        | KDRecord [(String, KData)]
        deriving Show
    
    --data Label = LabelLoop {breakExpr :: Expr, continueExpr :: Expr} deriving Show
    
    data Variable = Variable {varMutable :: Bool, varName :: String, varType :: KType} deriving Show
    
    --data Function = Function {funcName :: String, funcArgs :: [Object], beginning :: Expr} deriving Show 
    
    --data Type = Type {typeName :: String, typeDefine :: KType} deriving Show
    
    --data ParserState = ParserState {labels :: [Label], objects :: [Object], functions :: [Function], types :: [Type]} deriving Show
    
    --type Parser = Parsec String ParserState
    
    type Parser = Parsec String ()

    parserTest :: Parser Expr -> String -> Either ParseError Expr
    parserTest parser input = runParser parser () "" input
    --parserTest parser input = takeExpr 5 $ runParser parser (ParserState [] [] [] []) "" input
    
    {-
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
    -}
    spaces :: Parser ()
    spaces = try $ skipMany $ char ' '
    
    separator :: Parser ()
    separator = try $ spaces *> (newline <|> pure ' ') *> spaces
    
    semicolon :: Parser ()
    semicolon = try $ spaces *> (char ';' *> separator <|> newline *> spaces)
    
    sepBy :: Parser a -> Parser b -> Parser [a]
    sepBy mainP sepP = helperSepBy mainP sepP <|> pure [] where
        helperSepBy mainP sepP = try ((:) <$> mainP <* sepP <*> helperSepBy mainP sepP) <|> ( : []) <$> mainP

    parseCharAfterLeftSlash :: Parser Char
    parseCharAfterLeftSlash = char 'n' *> return '\n' <|> char '\'' *> return '\'' <|> char '"' *> return '"'

    parseInt :: Parser Expr
    parseInt = (Val . KDInt . read) <$> try ((++) <$> (string "-" <* spaces <|> pure "") <*> many1 digit)
    
    parseDouble :: Parser Expr
    parseDouble = (Val . KDDouble . read) <$> try ((\a b c d -> a ++ b ++ c ++ d) <$> (string "-" <* spaces <|> pure "") <*> many1 digit <*> string "." <*> many1 digit)
    
    parseString :: Parser Expr
    parseString = (Val . KDArray . fmap KDChar) <$> between (char '"') (char '"') (many $ char '\\' *> parseCharAfterLeftSlash <|> satisfy (/= '"'))

    parseRange :: Parser Expr
    parseRange = try $ parseInt >>= (\(Val (KDInt x)) ->
        ((string ".." *> parseInt) >>= (\(Val (KDInt y)) ->
            try ((spaces *> string "step" *> spaces *> parseInt) >>= (\(Val (KDInt z)) ->
                return $ Val $ KDArray [KDInt z | z <- [x, x + z .. y]])) <|>
            (return $ Val $ KDArray [KDInt z | z <- [x..y]]))) <|>
        ((spaces *> string "downTo" *> spaces *> parseInt) >>= (\(Val (KDInt y)) ->
            try ((spaces *> string "step" *> spaces *> parseInt) >>= (\(Val (KDInt z)) ->
                return $ Val $ KDArray [KDInt z | z <- [x, x - z .. y]])) <|>
            (return $ Val $ KDArray [KDInt z | z <- [x, x - 1 .. y]]))
        ))
    
    parseChar :: Parser Expr
    parseChar = (Val . KDChar) <$> between (char '\'') (char '\'') (char '\\' *> parseCharAfterLeftSlash <|> anyChar)
    
    parseBool :: Parser Expr
    parseBool = do { string "true"; return $ Val $ KDBool True } <|> do { string "false"; return $ Val $ KDBool False }
    
    parseNull :: Parser Expr
    parseNull = do { try $ string "null"; return $ Val KDNull }
    
    parseUnit :: Parser Expr
    parseUnit = do { try $ string "Unit"; return $ Val KDUnit }
    
    parseFunPrimitiv :: Parser FunPrimitiv
    parseFunPrimitiv = Expression <$> parseExpr

    parseValue :: Parser Expr
    parseValue = parseRange <|> parseDouble <|> parseInt <|> parseString <|> parseChar <|> parseBool <|> parseNull <|> parseUnit <|> parseInparens
    
    parseInparens :: Parser Expr
    parseInparens = char '(' *> spaces *> parseExpr <* spaces <* char ')'
    
    --parseOperator :: (Expr -> Expr -> Expr) -> Parser Expr -> String -> Parser Expr -> Parser Expr
    --parseOperator f p1 op p2 = try $ (f <$> p1 <* spaces <* string op <* spaces <*> p2)
    
    parseOperator :: (Expr -> Expr -> Expr) -> Parser Expr -> String -> Parser Expr -> Parser Expr
    parseOperator f p1 op p2 = p1 >>= (\e1 -> parseHalfOperator f e1 op p2 <|> return e1)

    parseHalfOperator :: (Expr -> Expr -> Expr) -> Expr -> String -> Parser Expr -> Parser Expr
    parseHalfOperator f e1 op p2 = try (spaces *> string op *> spaces *> p2) >>= (\e2 -> return $ f e1 e2)

    --parseHalfOperator :: String -> Parser Expr -> Parser Expr
    --parseHalfOperator op p2 = spaces *> string op *> spaces *> p2

    parseExpr :: Parser Expr
    parseExpr = try parseOr
    
    parseOr :: Parser Expr
    parseOr = parseOperator Or parseAnd "||" parseOr
    --parseOr = parseAnd >>= (\expr1 -> (parseHalfOperator "||" parseOr >>= (\expr2 -> return $ Or expr1 expr2)) <|> return expr1)
    --parseOr = parseOperator Or parseAnd "||" parseOr <|> parseAnd
    
    parseAnd :: Parser Expr
    parseAnd = parseOperator And parseEquation "&&" parseAnd
    --parseAnd = parseEquation >>= (\expr1 -> (parseHalfOperator "&&" parseAnd >>= (\expr2 -> return $ And expr1 expr2)) <|> return expr1)
    --parseAnd = parseOperator And parseEquation "&&" parseAnd <|> parseEquation
    
    parseEquation :: Parser Expr
    parseEquation = parseAddSub >>= (\e1 ->
        parseHalfOperator (\x y -> Equal x y) e1 "==" parseAddSub <|>
        parseHalfOperator (\x y -> Not $ Equal x y) e1 "!=" parseAddSub <|>
        parseHalfOperator (\x y -> Less x y) e1 "<" parseAddSub <|>
        parseHalfOperator (\x y -> Less y x) e1 ">" parseAddSub <|>
        parseHalfOperator (\x y -> Not $ Less y x) e1 "<=" parseAddSub <|>
        parseHalfOperator (\x y -> Not $ Less x y) e1 ">=" parseAddSub <|>
        return e1)
    --parseEquation = parseOperator Equal parseAddSub "==" parseAddSub <|> parseOperator (\x y -> Not $ Equal x y) parseAddSub "!=" parseAddSub <|> parseOperator Less parseAddSub "<" parseAddSub <|> parseOperator (flip Less) parseAddSub ">" parseAddSub <|> parseOperator (\x y -> Not $ Less y x) parseAddSub "<=" parseAddSub <|> parseOperator (\x y -> Not $ Less x y) parseAddSub ">=" parseAddSub <|> parseAddSub
    
    parseAddSub :: Parser Expr
    parseAddSub = parseMulDivMod >>= (\e1 ->
        parseHalfOperator Add e1 "+" parseAddSub <|>
        parseHalfOperator Sub e1 "-" parseAddSub <|>
        return e1)
    --parseAddSub = parseOperator Add parseMulDivMod "+" parseAddSub <|> parseOperator Sub parseMulDivMod "-" parseAddSub <|> parseMulDivMod
    
    parseMulDivMod :: Parser Expr
    parseMulDivMod = parseNot >>= (\e1 ->
        parseHalfOperator Mul e1 "*" parseMulDivMod <|>
        parseHalfOperator Div e1 "/" parseMulDivMod <|>
        parseHalfOperator Mod e1 "%" parseMulDivMod <|>
        return e1)
    --parseMulDivMod = parseOperator Mul parseNot "*" parseMulDivMod <|> parseOperator Div parseNot "/" parseMulDivMod <|> parseOperator Mod parseNot "%" parseMulDivMod <|> parseNot
    
    parseNot :: Parser Expr
    parseNot = (string "!" *> spaces *> (Not <$> parseIf)) <|> parseIf

    parseIf :: Parser Expr
    parseIf = try (string "if" *> spaces *> pure If <*> parseInparens <* separator <*> parseBlock <*> (try (separator *> string "else" *> separator *> parseBlock) <|> pure [])) <|> parseFunOrVar <|> parseValue

    parseName :: Parser String
    parseName = (:) <$> letter <*> many (letter <|> digit <|> char '_')

    parseFunOrVar :: Parser Expr
    parseFunOrVar = parseName >>= (\name ->
        ((char '.' *> parseFunOrVar) >>= (\field ->
            return $ case field of
                CallFun ('.' : funName) ((Var fieldName) : funArgs) -> CallFun ('.' : funName) ((Var $ name ++ "." ++ fieldName) : funArgs)
                CallFun funName funArgs -> CallFun ('.' : funName) ((Var name) : funArgs)
                Var fieldName -> Var $ name ++ "." ++ fieldName
            )
        ) <|>
        try ((spaces *> char '(' *> spaces *> (parseExpr `sepBy` try (spaces *> char ',' <* spaces)) <* spaces <* char ')') >>= (\args ->
            return $ CallFun name args)
        ) <|>
        (return $ Var name)
     )

    parseBlock :: Parser [FunPrimitiv]
    parseBlock = try (char '{' *> separator *> sepBy (Expression <$> parseExpr) (try semicolon) <* separator <* char '}') <|> ((( : []) . Expression) <$> parseIf)

    parseKTypeName :: Parser KType
    parseKTypeName = do {
        string "Int";
        return $ KTInt;
    } <|> do {
        string "Long";
        return $ KTLong;
    } <|> do {
        string "Byte";
        return $ KTByte;
    } <|> do {
        string "Short";
        return $ KTShort;
    } <|> do {
        string "Double";
        return $ KTDouble;
    } <|> do {
        string "Char";
        return KTChar;
    } <|> do {
        string "Bool";
        return KTBool;
    } <|> do {
        string "Unit";
        return KTUnit;
    } <|> do {
        typeName <- parseName;
        return $ KTUserType typeName;
    }

    parseSimpleKType :: Parser KType
    parseSimpleKType = parseKTypeName >>= (\ktype ->
        char '?' *> return (KTNullable ktype) <|>
        return ktype
     )

    parseKType :: Parser KType
    parseKType = try (string "Array<" *> pure KTArray <*> parseKType <* string ">") <|> parseSimpleKType

    parseWhile :: Parser FunPrimitiv
    parseWhile = string "while" *> spaces *> pure While <*> parseInparens <* separator <*> parseBlock

    parseVarInit :: Parser FunPrimitiv
    parseVarInit = string "var " *> spaces *> pure VarInit <*> parseName <*> (try (spaces *> string ":" *> spaces *> parseKType) <|> pure KTAny)

    parseValInit :: Parser FunPrimitiv
    parseValInit = string "val " *> spaces *> pure ValInit <*> parseName <*> (try (spaces *> string ":" *> spaces *> parseKType) <|> pure KTAny)

    parseLabels :: Parser FunPrimitiv
    parseLabels = do {
        string "break";
        return Break;
    } <|> do {
        string "continue";
        return Continue;
    } <|> do {
        string "return ";
        --return Continue
        spaces;
        result <- parseIf;
        return $ Return result;
    }

        --(char '(' *> spaces *> char ')' *> return (CallFun {name = name, fargs = []})))

    --parseIf = try $ JmpIf <$> (string "if" *> spaces *> parseInparens) <* separator <*> parseBlock <*> (separator *> string "else" *> separator *> parseBlock <|> pure NOP) <|> parseValue
    
    {-parseBlock :: Parser Expr
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
    -}
    main :: IO ()
    main = do
        putStrLn "Hello world!"
        test <- getLine
        print $ parserTest parseExpr test
    
