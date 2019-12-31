{-# LANGUAGE DuplicateRecordFields #-}

module Parsers where
    import Ast
    import Data.Functor.Identity
    import Text.Parsec hiding (spaces)
    import Data.Functor.Identity

    type Parser = Parsec String ()

    spaces :: Parser ()
    spaces = many (char ' ') *> return ()
    
    separator :: Parser ()
    separator = many (char '\n' <|> char ' ') *> return ()
    
    semicolon :: Parser ()
    semicolon = spaces *> (char ';' <|> char '\n') *> separator
    
    --none of default sepBy variants implement this functionality
    customSepBy :: Parser a -> Parser b -> Parser [a]
    customSepBy mainP sepP = helperSepBy mainP sepP <|> pure [] where
                       helperSepBy mainP sepP = try ((:) <$> mainP <* sepP <*> helperSepBy mainP sepP)
                                                <|> ( : []) <$> mainP

    parseCharAfterLeftSlash :: Parser Char
    parseCharAfterLeftSlash = char 'n' *> return '\n' <|> char '\'' *> return '\'' <|> char '"' *> return '"'

    parseInt :: Parser Expr
    parseInt = (Val . KDInt . read) <$> ((++) <$> (string "-" <* spaces <|> pure "") <*> many1 digit)
    
    parseDouble :: Parser Expr
    parseDouble = (Val . KDDouble . read) <$> 
                        ((\a b c d -> a ++ b ++ c ++ d) <$> 
                        (string "-" <* spaces <|> pure "") <*> many1 digit <*> string "." <*> many1 digit)
    
    parseString :: Parser Expr
    parseString = (Val . KDArray . fmap KDChar) <$> between (char '"') (char '"') (many $ char '\\' *> parseCharAfterLeftSlash <|> satisfy (/= '"'))

    parseRange :: Parser Expr
    parseRange = try $ parseInt >>= (\(Val (KDInt x)) ->
        ((string ".." *> parseInt) >>= (\(Val (KDInt y)) ->
            try ((spaces *> string "step" *> spaces *> parseInt) >>= (\(Val (KDInt z)) ->
                return $ Val $ KDArray [KDInt z | z <- [x, x + z .. y]])) <|>
            (return $ Val $ KDArray [KDInt z | z <- [x..y]]))) <|>
        ((spaces *> string "until" *> spaces *> parseInt) >>= (\(Val (KDInt y)) ->
            try ((spaces *> string "step" *> spaces *> parseInt) >>= (\(Val (KDInt z)) ->
                return $ Val $ KDArray [KDInt z | z <- [x, x + z .. (y - 1)]])) <|>
            (return $ Val $ KDArray [KDInt z | z <- [x..(y - 1)]]))) <|>
        ((spaces *> string "downTo" *> spaces *> parseInt) >>= (\(Val (KDInt y)) ->
            try ((spaces *> string "step" *> spaces *> parseInt) >>= (\(Val (KDInt z)) ->
                return $ Val $ KDArray [KDInt z | z <- [x, x - z .. y]])) <|>
            (return $ Val $ KDArray [KDInt z | z <- [x, x - 1 .. y]]))
        ))
    
    parseChar :: Parser Expr
    parseChar = (Val . KDChar) <$> between (char '\'') (char '\'') ((char '\\' *> parseCharAfterLeftSlash) <|> anyChar)
    
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
    parseAssignment = do 
        lvalue <- parseFunOrVar
        spaces
        char '='
        separator
        rvalue <- parseOr
        return $ case lvalue of
            CallFun ".get" ((Var nameObject) : fields) -> [Expression $ CallFun ".set" (rvalue : (Var nameObject) : fields)]

    parseValue :: Parser Expr
    parseValue = try parseRange
                 <|> try parseDouble 
                 <|> try parseInt 
                 <|> try parseString 
                 <|> try parseChar 
                 <|> parseInparens
    
    parseInparens :: Parser Expr
    parseInparens = char '(' *> spaces *> parseOr <* spaces <* char ')'
    
    parseOperator :: (Expr -> Expr -> Expr) -> Parser Expr -> String -> Parser Expr -> Parser Expr
    parseOperator f p1 op p2 = p1 >>= (\e1 -> try (parseHalfOperator f e1 op p2) <|> return e1)

    parseBoolOperator :: (Expr -> Expr -> Expr) -> Parser Expr -> String -> Parser Expr -> Parser Expr
    parseBoolOperator f p1 op p2 = p1 >>= (\e1 -> try (parseHalfBoolOperator f e1 op p2) <|> return e1)

    parseHalfOperator :: (Expr -> Expr -> Expr) -> Expr -> String -> Parser Expr -> Parser Expr
    parseHalfOperator f e1 op p2 = (spaces *> string op *> spaces *> p2) >>= (\e2 -> return $ f e1 e2)

    parseHalfBoolOperator :: (Expr -> Expr -> Expr) -> Expr -> String -> Parser Expr -> Parser Expr
    parseHalfBoolOperator f e1 op p2 = (spaces *> string op *> spaces *> (p2)) >>= (\e2 -> return $ f (CallFun {name = ".toBool", args = [e1]}) (CallFun {name = ".toBool", args = [e2]}))

    parseExpr :: Parser [FunPrimitive]
    parseExpr = ( : []) <$> Expression <$> parseOr
    
    parseOr :: Parser Expr
    parseOr = parseBoolOperator Or parseAnd "||" parseOr
    
    parseAnd :: Parser Expr
    parseAnd = parseBoolOperator And parseEquation "&&" parseAnd
    
    parseEquation :: Parser Expr
    parseEquation = parseAddSub >>= (\e1 ->
        try (parseHalfOperator (\x y -> Equal x y) e1 "==" parseAddSub) <|>
        try (parseHalfOperator (\x y -> Not $ Equal x y) e1 "!=" parseAddSub) <|>
        try (parseHalfOperator (\x y -> Less x y) e1 "<" parseAddSub) <|>
        try (parseHalfOperator (\x y -> Less y x) e1 ">" parseAddSub) <|>
        try (parseHalfOperator (\x y -> Not $ Less y x) e1 "<=" parseAddSub) <|>
        try (parseHalfOperator (\x y -> Not $ Less x y) e1 ">=" parseAddSub) <|>
        return e1)
    
    parseAddSub :: Parser Expr
    parseAddSub = parseMulDivMod >>= (\e1 ->
        try (parseHalfOperator Add e1 "+" parseAddSub) <|>
        try (parseHalfOperator Sub e1 "-" parseAddSub) <|>
        return e1)
    
    parseMulDivMod :: Parser Expr
    parseMulDivMod = parseNot >>= (\e1 ->
        try (parseHalfOperator Mul e1 "*" parseMulDivMod) <|>
        try (parseHalfOperator Div e1 "/" parseMulDivMod) <|>
        try (parseHalfOperator Mod e1 "%" parseMulDivMod) <|>
        return e1)
    
    parseNot :: Parser Expr
    parseNot = try (string "!" *> spaces *> ((\expr -> Not $ CallFun {name = ".toBool", args = [expr]}) <$> parseNot2)) <|> parseUnnullify

    parseNot2 :: Parser Expr
    parseNot2 = try (string "!" *> spaces *> ((\expr -> Not $ CallFun {name = ".toBool", args = [expr]}) <$> parseIf)) <|> parseIf

    parseUnnullify :: Parser Expr
    parseUnnullify = try ((\expr -> CallFun ".unnullify" [expr]) <$> (try parseFunOrVar <|> parseValue) <* spaces <* string "!!") <|> parseIf

    parseIf :: Parser Expr
    parseIf = try (If <$> ((\expr -> CallFun {name = ".toBool", args = [expr]}) <$> (string "if" *> spaces *> parseInparens)) <* separator <*> parseBlock <*> (try (separator *> string "else" *> separator *> parseBlock) <|> pure [])) <|>
              try parseFunOrVar <|>
              parseValue

    parseName :: Parser String
    parseName = (:) <$> letter <*> many (letter <|> digit <|> char '_')

    parseFunOrVar :: Parser Expr
    parseFunOrVar = helperName (CallFun ".get" []) where
        helperName (CallFun ".get" []) = do
            name <- parseName
            let variable = CallFun ".get" [Var name]
            helperInparens variable <|> helperIndex variable <|> helperPoint variable <|> return variable
        helperName (CallFun ".get" fields) = do
            name <- parseName
            let newField = CallFun ".get" $ (Var name) : fields
            helperInparens newField <|> helperIndex newField <|> helperPoint newField <|> return (CallFun ".get" $ reverse $ (Var name) : fields)
        helperInparens (CallFun ".get" [Var name]) = do
            char '('
            separator
            args <- customSepBy parseOr $ spaces *> char ',' <* separator
            separator
            char ')'
            let function = CallFun ".get" [CallFun name args]
            helperIndex function <|> helperPoint function <|> return function
        helperInparens (CallFun ".get" ((Var field) : variable)) = do
            char '('
            separator
            args <- customSepBy parseOr $ spaces *> char ',' <* separator
            separator
            char ')'
            let method = CallFun ".get" [CallFun ('.' : field) $ (CallFun ".get" $ reverse variable) : args]
            helperIndex method <|> helperPoint method <|> return method
        helperIndex (CallFun ".get" fields) = do
            char '['
            separator
            index <- parseOr
            separator
            char ']'
            let element = CallFun ".get" $ index : fields
            helperIndex element <|> helperPoint element <|> return (CallFun ".get" $ reverse $ index : fields)
        helperPoint (CallFun ".get" fields) = do
            char '.'
            helperName $ CallFun ".get" fields

    parseBlock :: Parser [FunPrimitive]
    parseBlock = (concat <$> try (char '{' *> separator *> customSepBy parseFunPrimitive semicolon <* (semicolon <|> separator) <* char '}')) <|>
        ((( : []) . Expression) <$> parseIf)

    parseKTypeName :: Parser KType
    parseKTypeName = try (do 
                         string "Int"
                         return $ KTInt
                         ) 
                <|> try (do 
                        string "Long"
                        return $ KTLong
                        ) 
                <|> try (do 
                        string "Byte"
                        return $ KTByte
                        ) 
                <|> try (do 
                        string "Short"
                        return $ KTShort
                        ) 
                <|> try (do 
                        string "Double"
                        return $ KTDouble
                        )
                <|> try (do
                        string "String"
                        return $ KTArray KTChar
                        ) 
                <|> try (do 
                        string "Char"
                        return KTChar
                        ) 
                <|> try (do 
                        string "Bool"
                        return KTBool
                        ) 
                <|> try (do 
                        string "Unit"
                        return KTUnit
                        ) 
                <|> do 
                    typeName <- parseName
                    return $ KTUserType typeName
                    

    parseSimpleKType :: Parser KType
    parseSimpleKType = parseKTypeName >>= (\ktype ->
        try (char '?' *> return (KTNullable ktype)) <|>
        return ktype
     )

    parseKType :: Parser KType
    parseKType = try (string "Array<" *> (KTArray <$> parseKType <* string ">")) <|> parseSimpleKType

    parseWhile :: Parser [FunPrimitive]
    parseWhile = ( : []) <$> (While <$> (string "while" *> spaces *> ((\expr -> CallFun ".toBool" [expr]) <$> parseInparens)) <* separator <*> parseBlock)

    parseVarInit :: Parser [FunPrimitive]
    parseVarInit = (string "var " *> spaces *> parseName) >>= (\varName ->
        (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTUnknown) >>= (\varType ->
             (try (spaces *> char '=' *> spaces *> parseOr) >>= (\varInitValue ->
                return $ [VarInit varName varType, Expression (CallFun ".set" [varInitValue, Var varName])]
             ))
             <|>
            (return $ [VarInit varName varType])
         )
     )

    parseValInit :: Parser [FunPrimitive]
    parseValInit = (string "val " *> spaces *> parseName) >>= (\valName ->
        (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTUnknown) >>= (\valType ->
            (try (spaces *> char '=' *> spaces *> parseOr) >>= (\valInitValue ->
                return $ [ValInit valName valType, Expression (CallFun ".set" [valInitValue, Var valName])]
             )
            ) <|>
            (return $ [ValInit valName valType])
         )
     )

    parseLabels :: Parser [FunPrimitive]
    parseLabels = do 
                  string "break"
                  return [Break]
            <|> do 
                string "continue"
                return [Continue]
            <|> do 
                string "return "
                spaces
                result <- parseOr
                return $ [Return result]

    parseVariableVar :: Parser Variable
    parseVariableVar = string "var " *> spaces *> (Variable <$> pure True <*> parseName <*> (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny))

    parseVariableVal :: Parser Variable
    parseVariableVal = string "val " *> spaces *> (Variable <$> pure False <*> parseName <*> (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny))

    parseFunParameters :: Parser [Variable]
    parseFunParameters = between (char '(') (char ')') ((separator *> (try parseVariableVal <|> parseVariableVar) <* separator) `sepBy` (char ','))

    parseFun :: Parser Fun
    parseFun = do 
               string "fun"
               spaces
               Fun <$> (spaces *> parseName) <* spaces <*> parseFunParameters <* spaces <*>
                   (try (char ':' *> spaces *> parseKType <* separator) <|> return KTUnit <* separator) <*>
                   ((try parseBlock) <|> char '=' *> separator *> parseExpr)

    removeComments :: String -> Int -> String
    removeComments ('/':'*':xs) parNum = removeComments xs (parNum + 1)
    removeComments ('*':'/':xs) parNum = if (parNum > 0) then removeComments xs (parNum - 1) else removeComments xs parNum
    removeComments (s1:s2:xs) parNum = if (parNum > 0) then removeComments (s2:xs) parNum else s1 : removeComments (s2:xs) parNum
    removeComments (s1:xs) _ = [s1]
    removeComments [] _ = []


    instance Monoid Class where
       mempty = Class "" [] [] []
       mappend (Class s1 a1 b1 c1) (Class s2 a2 b2 c2) = 
          Class (s1 ++ s2) (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)

    instance Semigroup Class where
        (Class s1 a1 b1 c1) <> (Class s2 a2 b2 c2) = Class (s1 ++ s2) (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)

    parseClass :: Parser Class
    parseClass = do 
                 string "class"
                 spaces
                 name <- parseName
                 spaces
                 char '{'
                 separator
                 cl0 <- return $ Class name [] [] []
                 cl1 <- parseClassNext
                 separator
                 char '}'
                 return $ cl0 `mappend` cl1
    

    parseClassNext :: Parser Class
    parseClassNext = try(do 
                         fun <- parseFun
                         cl1 <- return $ Class "" [] [fun] []
                         separator
                         cl2 <- parseClassNext
                         return $ cl1 `mappend` cl2
                        )
                <|> try (do 
                         val <- parseVariableVal
                         cl1 <- return $ Class "" [val] [] []
                         separator
                         cl2 <- parseClassNext
                         return $ cl1 `mappend` cl2
                        )
                <|> try (do 
                         var <- parseVariableVar
                         cl1 <- return $ Class "" [var] [] []
                         separator
                         cl2 <- parseClassNext
                         return $ cl1 `mappend` cl2
                        )
                <|> try (do 
                         cl <- parseClass' "" -- here parseClass'
                         cl1 <- return $ Class "" [] [] [cl]
                         separator
                         cl2 <- parseClassNext
                         return $ cl1 `mappend` cl2
                        )    
                <|> do 
                    return $ Class "" [] [] []
                

    parseClass' :: String -> Parser Class
    parseClass' className = do 
                            string "class"
                            spaces
                            name <- parseName
                            spaces
                            char '{'
                            separator
                            cl0 <- return $ Class name [] [] []
                            cl1 <- parseClassNext' (name ++ ".")
                            separator
                            char '}'
                            return $ cl0 `mappend` cl1
    

    parseClassNext' :: String -> Parser Class
    parseClassNext' className = try (do 
                                     fun <- parseFun    --main difference starts here (f() -> A.f(x))
                                     funFixed <- return $ Fun (className ++ (name (fun :: Fun))) (args (fun :: Fun)) (returnType (fun :: Fun)) (body (fun :: Fun)) --duplicate records does not infer types
                                     cl1 <- return $ Class "" [] [funFixed] []
                                     separator
                                     cl2 <- parseClassNext' className
                                     return $ cl1 `mappend` cl2
                                    )
                            <|> try (do 
                                     val <- parseVariableVal
                                     cl1 <- return $ Class "" [val] [] []
                                     separator
                                     cl2 <- parseClassNext' className
                                     return $ cl1 `mappend` cl2
                                    )
                            <|> try (do 
                                     var <- parseVariableVar
                                     cl1 <- return $ Class "" [var] [] []
                                     separator
                                     cl2 <- parseClassNext' className
                                     return $ cl1 `mappend` cl2
                                    )
                            <|> try (do 
                                     cl <- parseClass' className
                                     cl1 <- return $ Class "" [] [] [cl]
                                     separator
                                     cl2 <- parseClassNext' className
                                     return $ cl1 `mappend` cl2
                                    )    
                            <|> do 
                                return $ Class "" [] [] []
    
    addOuterClass :: String -> String
    addOuterClass program = "class Main {\n" ++ program ++ " }"

    parseProgram :: Parser Class
    parseProgram = parseClass
    
 
