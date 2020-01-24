{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}
module Parsers where
import Ast
import Data.Functor.Identity
import Text.Parsec hiding (spaces)
import Data.Functor.Identity
import Control.Monad
import Debug.Trace

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

parseInt :: Parser Integer
parseInt = read <$> ((++) <$> (string "-" <* spaces <|> pure "") <*> many1 digit)

parseDouble :: Parser Double
parseDouble = read <$> 
                    ((\a b c d -> a ++ b ++ c ++ d) <$> 
                    (string "-" <* spaces <|> pure "") <*> many1 digit <*> string "." <*> many1 digit)

parseString :: Parser String -- (KDArray . fmap KDChar) <$>
parseString = between (char '"') (char '"') (many $ char '\\' *> parseCharAfterLeftSlash <|> satisfy (/= '"'))

parseRange :: Parser [Integer] --KDArray [KDInt z | z <- [x, x + z .. y]])
parseRange = parseInt >>= (\x ->
    ((string ".." *> parseInt) >>= (\y ->
        try ((spaces *> string "step" *> spaces *> parseInt) >>= (\z ->
            return $  [z | z <- [x, x + z .. y]])) <|>
        (return $ [z | z <- [x..y]]))) <|>
    ((spaces *> string "until" *> spaces *> parseInt) >>= (\y ->
        try ((spaces *> string "step" *> spaces *> parseInt) >>= (\z ->
            return $ [z | z <- [x, x + z .. (y - 1)]])) <|>
        (return $ [z | z <- [x..(y - 1)]]))) <|>
    ((spaces *> string "downTo" *> spaces *> parseInt) >>= (\y ->
        try ((spaces *> string "step" *> spaces *> parseInt) >>= (\z ->
            return $ [z | z <- [x, x - z .. y]])) <|>
        (return $ [z | z <- [x, x - 1 .. y]]))
    )) 

parseChar :: Parser Char
parseChar = between (char '\'') (char '\'') ((char '\\' *> parseCharAfterLeftSlash) <|> anyChar)

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

parseLambdaParams :: Parser [Variable]
parseLambdaParams = try (do 
                        parameters <- (separator *> (try parseOneFunParameter) <* separator) `sepBy` (char ',')
                        spaces
                        string "->"
                        return parameters
                        )
                    <|> return []    

parseLambda :: Parser Expr -- parse lambda in array constructor
parseLambda = do 
    char '{'
    separator
    params <- parseLambdaParams
    separator
    body <- concat <$> (separator *> customSepBy parseFunPrimitive semicolon <* (semicolon <|> separator))
    separator
    char '}' 
    return $ Lambda params body

parseAssignment :: Parser [FunPrimitive]
parseAssignment = do 
    lvalue <- parseFunOrVar
    spaces
    char '='
    separator
    rvalue <- parseRValue
    case lvalue of
        CallFun ".get" ((Var nameObject) : fields) -> 
            return [Expression $ CallFun ".set" (rvalue : (Var nameObject) : fields)]
        _ -> fail "Cannot assign to rvalue"  

parseArrayLambdaCtor :: Parser Expr
parseArrayLambdaCtor = do 
    string "Array"
    char '('
    spaces
    size <- parseInt --TODO : parseNumericValue?
    spaces 
    char ')'
    spaces
    lambda <- parseLambda
    spaces
    return $ CallFun ".array" [Val $ KDInt size, lambda]

parseRValue :: Parser Expr
parseRValue = try parseArrayLambdaCtor
                <|> parseOr

parseFinalValue :: Parser Expr 
parseFinalValue = Val <$> (try ( (KDArray . fmap KDInt) <$> parseRange)
                    <|> try (KDDouble <$> parseDouble) 
                    <|> try (KDInt <$> parseInt) 
                    <|> try ((KDArray . fmap KDChar) <$> parseString) 
                    <|> KDChar <$> parseChar)

parseValue :: Parser Expr
parseValue = try parseFinalValue  
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

parseNameWithDot :: Parser String
parseNameWithDot = (:) <$> letter <*> many (letter <|> digit <|> char '_' <|> char '.')

parseFunOrVar :: Parser Expr
parseFunOrVar = helperName (CallFun ".get" []) where
    helperName (CallFun ".get" []) = do
        name <- parseName
        let gettingVariableByName = CallFun ".get" [Var name] --variable while we have not encountered parentheses
        --continue parsing phrase
        helperInparens gettingVariableByName <|> helperIndex gettingVariableByName <|> helperPoint gettingVariableByName <|> return gettingVariableByName
    helperName (CallFun ".get" prevPhrase) = do -- getting name after point
        name <- parseName
        let gettingField = CallFun ".get" $ (Var name) : prevPhrase
        --reversing because of interpretator convinience, can't construct at correct order from the beginning because of the pattern matching
        helperInparens gettingField <|> helperIndex gettingField <|> helperPoint gettingField <|> return (CallFun ".get" $ reverse $ (Var name) : prevPhrase)
    helperName _ = fail "Wrong helperName argument"    
    helperInparens (CallFun ".get" [Var name]) = do
        char '('
        separator
        args <- customSepBy parseOr $ spaces *> char ',' <* separator
        separator
        char ')'
        let function = CallFun ".get" [CallFun name args]
        helperIndex function <|> helperPoint function <|> return function   
    helperInparens (CallFun ".get" ((Var field) : prevPhrase)) = do
        char '('
        separator
        args <- customSepBy parseOr $ spaces *> char ',' <* separator
        separator
        char ')'
        let method = CallFun ".get" [CallFun ('.' : field) $ (CallFun ".get" $ reverse prevPhrase) : args]
        helperIndex method <|> helperPoint method <|> return method
    helperInparens _ = fail "Wrong helperInparens argument"     
    helperIndex (CallFun ".get" prevPhrase) = do
        char '['
        separator
        index <- parseOr
        separator
        char ']'
        let element = CallFun ".get" $ index : prevPhrase
        helperIndex element <|> helperPoint element <|> return (CallFun ".get" $ reverse $ index : prevPhrase)
    helperIndex _ = fail "Wrong helperIndex argument"     
    helperPoint (CallFun ".get" prevPhrase) = do
        char '.'
        helperName $ CallFun ".get" prevPhrase
    helperPoint _ = fail "Wrong helperPoint argument"     

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
            <|> try (do
                     string "Any"
                     return KTAny 
                    )        
            <|> do 
                typeName <- parseNameWithDot
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
            (try (spaces *> char '=' *> spaces *> parseRValue) >>= (\varInitValue ->
            return $ [VarInit varName varType, Expression (CallFun ".set" [varInitValue, Var varName])]
            ))
            <|>
        (return $ [VarInit varName varType])
        )
    )

parseValInit :: Parser [FunPrimitive]
parseValInit = (string "val " *> spaces *> parseName) >>= (\valName ->
    (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTUnknown) >>= (\valType ->
        (try (spaces *> char '=' *> spaces *> parseRValue) >>= (\valInitValue ->
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

parseOneFunParameter :: Parser Variable
parseOneFunParameter = (Variable <$> pure False <*> parseName <*> (try (spaces *> char ':' *> spaces *> parseKType) <|> pure KTAny))

parseFunParameters :: Parser [Variable]
parseFunParameters = between (char '(') (char ')') ((separator *> (try parseOneFunParameter) <* separator) `sepBy` (char ','))

parseConstructorParams :: Parser [Variable]
parseConstructorParams = between (char '(') (char ')') ((separator *> (
                                                                        try (parseVariableVal) <|> parseVariableVar
                                                                        ) <* separator) `sepBy` (char ','))

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
    mappend (Class s1 a1 b1 c1) (Class "" a2 b2 c2) = 
        Class (s1) (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)
    mappend (Class "" a1 b1 c1) (Class s2 a2 b2 c2) = 
        Class (s2) (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)   
    mappend (Class s1 a1 b1 c1) (Class s2 a2 b2 c2) = 
        Class (s1) (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)

instance Semigroup Class where
    (Class "" a1 b1 c1) <> (Class s2 a2 b2 c2) = Class (s2) (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)
    (Class s1 a1 b1 c1) <> (Class "" a2 b2 c2) = Class (s1) (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)
    (Class s1 a1 b1 c1) <> (Class s2 a2 b2 c2) = Class (s1) (a1 ++ a2) (b1 ++ b2) (c1 ++ c2)

parseClassConstuctorFields :: Parser [Variable]
parseClassConstuctorFields = try (parseConstructorParams) <|> pure []

parseProgram :: Parser Class
parseProgram =  try (do 
                        separator
                        fun <- parseFun
                        cl1 <- return $ Class "" [] [fun] []
                        separator
                        cl2 <- parseProgram
                        return $ cl1 `mappend` cl2
                    )
            <|> try (do 
                        separator
                        val <- parseVariableVal
                        cl1 <- return $ Class "" [val] [] []
                        separator
                        cl2 <- parseProgram
                        return $ cl1 `mappend` cl2
                    )
            <|> try (do 
                        separator
                        var <- parseVariableVar
                        cl1 <- return $ Class "" [var] [] []
                        separator
                        cl2 <- parseProgram
                        return $ cl1 `mappend` cl2
                    )
            <|> try (do 
                        separator
                        (classWithNewConstructors, cl) <- parseClass (Class "Main" [] [] []) ""
                        cl1 <- return $ (Class "" [] [] [cl] `mappend` classWithNewConstructors)
                        separator
                        cl2 <- parseProgram
                        return $ cl1 `mappend` cl2
                    )    
            <|> do 
                separator
                eof
                --adding Object constructor here
                let anyFun = Fun "Any" [] KTAny [Expression $ (CallFun ".get" [Val $ KDAny])]
                return $ Class "Main" [] [anyFun] []

parseClass :: Class -> String -> Parser (Class, Class)
parseClass parentClass parentClassNameWithDot = do
        string "class"
        spaces
        className <- parseName
        spaces
        constructorFields <- parseClassConstuctorFields
        spaces
        char '{'
        separator
        cl0 <- return $ Class className constructorFields [] []
        let standardConstructor = Fun className 
                                      unmodifiedConstructorFields
                                      (KTUserType className) 
                                      [VarInit "this" (KTUserType $ parentClassNameWithDot ++ className)] where
                                        unmodifiedConstructorFields = (\(Variable _ varName varType) -> Variable False varName varType) <$> constructorFields
        let parentClassUpdated = Class parentName parentFields (standardConstructor : parentMethods) parentClasses where
                Class parentName parentFields parentMethods parentClasses = parentClass
        (parentClassUpdated2, cl1) <- parseClassNext parentClassUpdated (parentClassNameWithDot ++ className) constructorFields
        case parentClassUpdated2 of
            (Class parentName parentFields ((Fun _ args returnType (firstPart:initPart)):prevMethods) parentClasses ) -> do
                    let newMethod = Fun className 
                                        (if parentClassNameWithDot == "" then args else (Variable False "'" $ KTUserType $ init parentClassNameWithDot) : args)
                                        returnType ([firstPart] ++
                                                    [(Expression $ (CallFun ".set" ([Val $ KDRecord (translateVarListToEmptyRecord (fields $ cl0 `mappend` cl1))] ++ [Var "this"]) ))] ++
                                                    (getSetters constructorFields) ++
                                                    initPart ++
                                                    [(Expression $ (CallFun ".get" [Var "this"]))]) where
                                                            translateVarListToEmptyRecord :: [Variable] -> [(String, KData, KType, Bool)]
                                                            translateVarListToEmptyRecord ((Variable varMutable varName varType) : vs) = (varName, KDUndefined, varType, varMutable) : (translateVarListToEmptyRecord vs)
                                                            translateVarListToEmptyRecord [] = []
                                                            getSetters :: [Variable] -> [FunPrimitive]
                                                            getSetters [] = []
                                                            getSetters ((Variable {..}): fds) = (Expression $ (CallFun ".set" [CallFun ".get" [Var varName], Var "this", Var varName])) : (getSetters fds)
                    let parentClassUpdated3 = Class parentName parentFields (newMethod : prevMethods) parentClasses  
                    return (parentClassUpdated3, cl0 `mappend` cl1)
            _ -> fail $ "Unexprected construction of class " ++ className        

parseClassNext :: Class -> String -> [Variable] -> Parser (Class,Class)
parseClassNext parentClass@(Class {..}) fullClassName constructorFields = try (do 
                                    fun <- parseFun 
                                    let fun' = Fun funName (Variable True "this" (KTUserType fullClassName) : funArgs) funReturnType funBody where
                                            Fun funName funArgs funReturnType funBody = fun
                                    cl1 <- return $ Class "" [] [fun'] []
                                    separator
                                    (parentClassUpdated, cl2) <- parseClassNext parentClass fullClassName constructorFields
                                    return (parentClassUpdated, cl1 `mappend` cl2)
                                )
                            <|> try (do 
                                    string "init"
                                    spaces
                                    initBody <- ((try parseBlock) <|> char '=' *> separator *> parseExpr)
                                    case parentClass of
                                        (Class pName pFields ((Fun constrName constrArgs constrType constrBody):fs) pClasses) -> do
                                            let newConstructor = Fun constrName constrArgs constrType (constrBody ++ initBody)
                                            let parentClassUpdated = (Class pName pFields (newConstructor:fs) pClasses)
                                            separator
                                            (parentClassUpdated2, cl2) <- parseClassNext parentClassUpdated fullClassName constructorFields
                                            return (parentClassUpdated2, cl2)
                                        _ -> fail $ "Unexprected construction of class " ++ fullClassName    
                                    )     
                        <|> try (do 
                                    val <- parseVariableVal
                                    cl1 <- return $ Class "" [val] [] []
                                    separator
                                    (parentClassUpdated, cl2) <- parseClassNext parentClass fullClassName constructorFields
                                    return (parentClassUpdated, cl1 `mappend` cl2)
                                )
                        <|> try (do 
                                    var <- parseVariableVar
                                    cl1 <- return $ Class "" [var] [] []
                                    separator
                                    (parentClassUpdated, cl2) <- parseClassNext parentClass fullClassName constructorFields
                                    return (parentClassUpdated, cl1 `mappend` cl2)
                                )
                        <|> try (do 
                                    (classWithNewConstructors, cl) <- parseClass (Class (fullClassName ++ ".") [] [] []) (fullClassName ++ ".")
                                    cl1 <- return $ (Class "" [] [] [cl] `mappend` classWithNewConstructors)
                                    separator
                                    (parentClassUpdated, cl2) <- parseClassNext parentClass fullClassName constructorFields
                                    return (parentClassUpdated, cl1 `mappend` cl2) 
                                )    
                        <|> do 
                            separator
                            char '}'
                            return (parentClass, Class "" [] [] [])
