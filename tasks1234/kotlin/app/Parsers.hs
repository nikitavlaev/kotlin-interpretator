module Parsers where

    import Ast
    import Data.Functor.Identity
    import Text.Parsec hiding (spaces, sepBy)
    import Data.Functor.Identity
 
    type Parser = Parsec String ()

    spaces :: Parser ()
    spaces = skipMany $ char ' '
    
    separator :: Parser ()
    separator = spaces *> (many (char '\n' <|> char ' ')) *> spaces
    
    semicolon :: Parser ()
    semicolon = spaces *> (char ';' *> separator <|> newline *> spaces)
    
    --none of default sepBy variants implement this functionality
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
    
    parseFunPrimitive :: Parser [FunPrimitive]
    parseFunPrimitive = try parseWhile 
                    <|> try parseVarInit 
                    <|> try parseValInit 
                    <|> try parseAssignment
                    <|> try parseLabels 
                    <|> try parseThrow
                    <|> parseExpr

    parseThrow :: Parser [FunPrimitive]
    parseThrow = (:[]) <$> 
                 (Throw <$> 
                  (string "throw" *>
                   spaces *>
                  (Exception <$> parseName)))

    parseAssignment :: Parser [FunPrimitive]
    parseAssignment = do {
        lvalue <- parseFunOrVar;
        index <- (Just <$> try (char '[' *> parseOr <* char ']') <|> pure Nothing);
        spaces;
        char '=';
        spaces;
        rvalue <- parseOr;
        return [Expression $ CallFun ".set" (lvalue : rvalue : (case index of
            Just ind -> [ind]
            Nothing -> []
         ))];
    }

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

    parseExpr :: Parser [FunPrimitive]
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
        (return $ CallFun ".get" [Var name])
     )

    parseBlock :: Parser [FunPrimitive]
    parseBlock = (concat <$> try (char '{' *> separator *> sepBy parseFunPrimitive (try semicolon) <* (semicolon <|> separator) <* char '}')) <|>
        ((( : []) . Expression) <$> parseIf)

    parseKTypeName :: Parser KType
    parseKTypeName = try (do {
        string "Int";
        return $ KTInt
    }) <|> try (do {
        string "Long";
        return $ KTLong
    }) <|> try (do {
        string "Byte";
        return $ KTByte
    }) <|> try (do {
        string "Short";
        return $ KTShort
    }) <|> try (do {
        string "Double";
        return $ KTDouble
    }) <|> try (do {
        string "String";
        return $ KTArray KTChar
    }) <|> try (do {
        string "Char";
        return KTChar
    }) <|> try (do {
        string "Bool";
        return KTBool
    }) <|> try (do {
        string "Unit";
        return KTUnit
    }) <|> do {
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

    parseWhile :: Parser [FunPrimitive]
    parseWhile = ( : []) <$> (While <$> (string "while" *> spaces *> parseInparens) <* separator <*> parseBlock)

    parseVarInit :: Parser [FunPrimitive]
    parseVarInit = try (string "var " *> spaces *> parseName) >>= (\varName ->
        (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny) >>= (\varType ->
            (try (spaces *> char '=' *> spaces *> parseOr) >>= (\varInitValue ->
                return $ [VarInit varName varType, Expression (CallFun ".set" [Var varName, varInitValue])]
             )
            ) <|>
            (return $ [VarInit varName varType])
         )
     )

    parseValInit :: Parser [FunPrimitive]
    parseValInit = try (string "val " *> spaces *> parseName) >>= (\valName ->
        (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny) >>= (\valType ->
            (try (spaces *> char '=' *> spaces *> parseOr) >>= (\valInitValue ->
                return $ [ValInit valName valType, Expression (CallFun ".set" [Var valName, valInitValue])]
             )
            ) <|>
            (return $ [ValInit valName valType])
         )
     )

    parseLabels :: Parser [FunPrimitive]
    parseLabels = do {
        string "break";
        return [Break];
    } <|> do {
        string "continue";
        return [Continue];
    } <|> do {
        string "return ";
        spaces;
        result <- parseOr;
        return $ [Return result];
    }

    parseVariableVar :: Parser Variable
    parseVariableVar = string "var " *> spaces *> (Variable <$> pure True <*> parseName <*> (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny))

    parseVariableVal :: Parser Variable
    parseVariableVal = string "val " *> spaces *> (Variable <$> pure False <*> parseName <*> (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny))

    parseFunParameters :: Parser [Variable]
    parseFunParameters = between (char '(') (char ')') ((separator *> (try parseVariableVal <|> parseVariableVar) <* separator) `sepBy` try (char ','))

    parseFun :: Parser Primitive 
    parseFun = do {
        string "fun";
        spaces;
        Fun <$> (spaces *> parseName) <* spaces <*> parseFunParameters <* spaces <*>
            (try (char ':' *> spaces *> parseKType <* separator) <|> return KTUnit <* separator) <*>
            (try parseBlock <|> char '=' *> separator *> parseExpr)
    }

    removeComments :: String -> Int -> String
    removeComments (s1:s2:xs) parNum | (s1 == '/' && s2 == '*') = removeComments xs (parNum + 1)
                                     | (s1 == '*' && s2 == '/' && parNum > 0) = removeComments xs (parNum - 1)
                                     | (parNum > 0) = removeComments (s2:xs) parNum
                                     | (parNum == 0) = s1 : removeComments (s2:xs) parNum
    removeComments (s1:xs) _ = [s1]
    removeComments [] _ = []

    parseProgram :: Parser [Primitive]
    parseProgram = parseFun `sepBy` separator
 
