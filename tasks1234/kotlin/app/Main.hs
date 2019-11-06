{-# LANGUAGE DuplicateRecordFields #-}

module Main where

    import Text.Parsec hiding (spaces, sepBy)
    import Data.Functor.Identity
    
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

    data KData = KDUnit
        | KDNull
        | KDBool Bool
        | KDChar Char
        | KDInt Int
        | KDDouble Double
        | KDArray [KData]
        | KDRecord [(String, KData)]
        deriving Show
    
    data Variable = Variable {varMutable :: Bool, varName :: String, varType :: KType} deriving Show
    
    type Parser = Parsec String ()

    spaces :: Parser ()
    spaces = try $ skipMany $ char ' '
    
    separator :: Parser ()
    separator = try $ spaces *> (newline <|> pure ' ') *> spaces
    
    semicolon :: Parser ()
    semicolon = try $ spaces *> (char ';' *> separator <|> newline *> spaces)
    
    sepBy :: Parser a -> Parser b -> Parser [a]
    sepBy mainP sepP = helperSepBy mainP sepP <|> pure [] where
        helperSepBy mainP sepP = try ((:) <$> mainP <* sepP <*> helperSepBy mainP sepP)
                                 <|> ( : []) <$> mainP

    parseCharAfterLeftSlash :: Parser Char
    parseCharAfterLeftSlash = char 'n' *> return '\n' <|> char '\'' *> return '\'' <|> char '"' *> return '"'

    parseInt :: Parser Expr
    parseInt = (Val . KDInt . read) <$> try ((++) <$> (string "-" <* spaces <|> pure "") <*> many1 digit)
    
    parseDouble :: Parser Expr
    parseDouble = (Val . KDDouble . read) <$> 
                    try ((\a b c d -> a ++ b ++ c ++ d) <$> 
                        (string "-" <* spaces <|> pure "") <*> many1 digit <*> string "." <*> many1 digit)
    
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
    parseBool = do { string "true"; 
                     return $ Val $ KDBool True } 
                <|> do { string "false"; 
                         return $ Val $ KDBool False }
    
    parseNull :: Parser Expr
    parseNull = do { try $ string "null"; 
                     return $ Val KDNull }
    
    parseUnit :: Parser Expr
    parseUnit = do { try $ string "Unit"; 
                     return $ Val KDUnit }
    
    parseFunPrimitiv :: Parser [FunPrimitiv]
    parseFunPrimitiv = parseWhile <|>
        parseVarInit <|>
        parseValInit <|>
        parseLabels <|>
        parseExpr

    parseValue :: Parser Expr
    parseValue = try parseRange
                 <|> try parseBool
                 <|> try parseDouble 
                 <|> try parseInt 
                 <|> try parseString 
                 <|> try parseChar 
                 <|> try parseNull 
                 <|> try parseUnit 
                 <|> parseInparens
    
    parseInparens :: Parser Expr
    parseInparens = char '(' *> spaces *> parseOr <* spaces <* char ')'
    
    parseOperator :: (Expr -> Expr -> Expr) -> Parser Expr -> String -> Parser Expr -> Parser Expr
    parseOperator f p1 op p2 = p1 >>= (\e1 -> parseHalfOperator f e1 op p2 <|> return e1)

    parseHalfOperator :: (Expr -> Expr -> Expr) -> Expr -> String -> Parser Expr -> Parser Expr
    parseHalfOperator f e1 op p2 = try (spaces *> string op *> spaces *> p2) >>= (\e2 -> return $ f e1 e2)

    parseExpr :: Parser [FunPrimitiv]
    parseExpr = ( : []) <$> Expression <$> (try parseOr)
    
    parseOr :: Parser Expr
    parseOr = parseOperator Or parseAnd "||" parseOr
    
    parseAnd :: Parser Expr
    parseAnd = parseOperator And parseEquation "&&" parseAnd
    
    parseEquation :: Parser Expr
    parseEquation = parseAddSub >>= (\e1 ->
        parseHalfOperator (\x y -> Equal x y) e1 "==" parseAddSub <|>
        parseHalfOperator (\x y -> Not $ Equal x y) e1 "!=" parseAddSub <|>
        parseHalfOperator (\x y -> Less x y) e1 "<" parseAddSub <|>
        parseHalfOperator (\x y -> Less y x) e1 ">" parseAddSub <|>
        parseHalfOperator (\x y -> Not $ Less y x) e1 "<=" parseAddSub <|>
        parseHalfOperator (\x y -> Not $ Less x y) e1 ">=" parseAddSub <|>
        return e1)
    
    parseAddSub :: Parser Expr
    parseAddSub = parseMulDivMod >>= (\e1 ->
        parseHalfOperator Add e1 "+" parseAddSub <|>
        parseHalfOperator Sub e1 "-" parseAddSub <|>
        return e1)
    
    parseMulDivMod :: Parser Expr
    parseMulDivMod = parseNot >>= (\e1 ->
        parseHalfOperator Mul e1 "*" parseMulDivMod <|>
        parseHalfOperator Div e1 "/" parseMulDivMod <|>
        parseHalfOperator Mod e1 "%" parseMulDivMod <|>
        return e1)
    
    parseNot :: Parser Expr
    parseNot = try (string "!" *> spaces *> (Not <$> parseIf)) <|> parseIf

    parseIf :: Parser Expr
    parseIf = try (If <$> (string "if" *> spaces *> parseInparens) <* separator <*> parseBlock <*> (try (separator *> string "else" *> separator *> parseBlock) <|> pure [])) <|>
        parseFunOrVar <|>
        parseValue

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
        try ((spaces *> char '(' *> spaces *> (try parseOr `sepBy` try (spaces *> char ',' <* spaces)) <* spaces <* char ')') >>= (\args ->
            return $ CallFun name args)
        ) <|>
        (return $ Var name)
     )

    parseBlock :: Parser [FunPrimitiv]
    parseBlock = (concat <$> try (char '{' *> separator *> sepBy parseFunPrimitiv (try semicolon) <* (semicolon <|> separator) <* char '}')) <|>
        ((( : []) . Expression) <$> parseIf)

    parseKTypeName :: Parser KType
    parseKTypeName = do {
        string "Int";
        return $ KTInt
    } <|> do {
        string "Long";
        return $ KTLong
    } <|> do {
        string "Byte";
        return $ KTByte
    } <|> do {
        string "Short";
        return $ KTShort
    } <|> do {
        string "Double";
        return $ KTDouble
    } <|> do {
        string "String";
        return $ KTArray KTChar
    } <|> do {
        string "Char";
        return KTChar
    } <|> do {
        string "Bool";
        return KTBool
    } <|> do {
        string "Unit";
        return KTUnit
    } <|> do {
        typeName <- parseName;
        return $ KTUserType typeName
    } 

    parseSimpleKType :: Parser KType
    parseSimpleKType = parseKTypeName >>= (\ktype ->
        try (char '?' *> return (KTNullable ktype)) <|>
        return ktype
     )

    parseKType :: Parser KType
    parseKType = try (string "Array<" *> (KTArray <$> parseKType <* string ">")) <|> parseSimpleKType

    parseWhile :: Parser [FunPrimitiv]
    parseWhile = ( : []) <$> (While <$> (string "while" *> spaces *> parseInparens) <* separator <*> parseBlock)

    parseVarInit :: Parser [FunPrimitiv]
    parseVarInit = try (string "var " *> spaces *> parseName) >>= (\varName ->
        (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny) >>= (\varType ->
            (try (spaces *> char '=' *> spaces *> parseOr) >>= (\varInitValue ->
                return $ [VarInit varName varType, Expression (CallFun ".set" [Var varName, varInitValue])]
             )
            ) <|>
            (return $ [VarInit varName varType])
         )
     )

    parseValInit :: Parser [FunPrimitiv]
    parseValInit = try (string "val " *> spaces *> parseName) >>= (\valName ->
        (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny) >>= (\valType ->
            (try (spaces *> char '=' *> spaces *> parseOr) >>= (\valInitValue ->
                return $ [ValInit valName valType, Expression (CallFun ".set" [Var valName, valInitValue])]
             )
            ) <|>
            (return $ [ValInit valName valType])
         )
     )

    parseLabels :: Parser [FunPrimitiv]
    parseLabels = do {
        string "break";
        return [Break];
    } <|> do {
        string "continue";
        return [Continue];
    } <|> do {
        string "return ";
        spaces;
        result <- parseIf;
        return $ [Return result];
    }

    parseVariableVar :: Parser Variable
    parseVariableVar = string "var " *> spaces *> (Variable <$> pure True <*> parseName <*> (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny))

    parseVariableVal :: Parser Variable
    parseVariableVal = string "val " *> spaces *> (Variable <$> pure False <*> parseName <*> (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny))

    parseFunParameters :: Parser [Variable]
    parseFunParameters = between (char '(') (char ')') ((try separator *> (try parseVariableVal <|> parseVariableVar) <* try separator) `sepBy` try (char ','))

    parseFun :: Parser Fun 
    parseFun = do {
        string "fun";
        spaces;
        Fun <$> parseName <*> parseFunParameters <*> (try separator *> char ':' *> try separator *> parseKType <* try separator) <*> parseBlock 
    }

    main :: IO ()
    main = do
        putStrLn "Hello world!"
        test <- getLine
        parseTest parseExpr test
    
