{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Ast where
data Class = Class {name :: String, fields :: [Variable], methods :: [Fun], classes :: [Class]}
    deriving Show

data Fun = Fun {name :: String, args :: [Variable], returnType :: KType, body :: [FunPrimitive]} 
    deriving Show

data FunPrimitive = 
        Expression Expr
    | ValInit {name :: String, ktype :: KType} -- соответствует val <valName> : <valType>
    | VarInit {name :: String, ktype :: KType} -- соответствует var <valName> : <valType>
    | While {cond :: Expr, body :: [FunPrimitive]} -- соответствует while (<cond>) {<body>} и do {<body>} while (<cond>)
    | For {iterVal :: String, collection :: KData, body :: [FunPrimitive]} -- соответствует for (<iterVal> in <collection>) {<body>}
    | Break
    | Continue
    | Return {result :: Expr}
    | Throw {exception :: Exception}
    deriving Show

data Expr =
        Val KData
    | Var String
    | CallFun {name :: String, args :: [Expr]}
    | Lambda {largs :: [Variable], body :: [FunPrimitive]}
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
    | If {cond :: Expr, thenBranch :: [FunPrimitive], elseBranch :: [FunPrimitive]}
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
    | KTUnknown
    deriving Eq

instance Show KType where
    show KTUnit = "Unit"
    show KTBool = "Bool"
    show KTChar = "Char"
    show KTByte = "Byte"
    show KTShort = "Short"
    show KTInt = "Int"
    show KTLong = "Long"
    show KTDouble = "Double"
    show (KTArray ktype) = "Array<" ++ show ktype ++ ">"
    show (KTNullable ktype) = show ktype ++ "?"
    show (KTUserType name) = name
    show (KTAny) = "Any"
    show (KTUnknown) = "???"

instance Ord KType where
    KTByte <= KTByte = True
    KTByte <= KTShort = True
    KTByte <= KTInt = True
    KTByte <= KTLong = True
    KTShort <= KTShort = True
    KTShort <= KTInt = True
    KTShort <= KTLong = True
    KTInt <= KTInt = True
    KTInt <= KTLong = True
    KTLong <= KTLong = True
    _ <= _ = False    

data KData = KDUnit
    | KDNull
    | KDBool Bool
    | KDChar Char
    | KDInt Integer
    | KDDouble Double
    | KDArray [KData]
    | KDRecord [(String, KData, KType, Bool)] -- name, data, type, canModify
    | KDUndefined
    | KDAny
    | KDError String

instance Show KData where
    show KDUnit = "unit"
    show KDNull = "null"
    show KDAny = "any"
    show (KDBool True) = "true"
    show (KDBool False) = "false"
    show (KDChar c) = [c]
    show (KDInt x) = show x
    show (KDDouble x) = show x
    show (KDArray ((KDChar c):cs)) = c : show (KDArray cs)
    show (KDArray []) = []
    show (KDArray xs) = "[" ++ showArray xs ++ "]" where
        showArray [x] = show x
        showArray (x : xs) = show x ++ ", " ++ showArray xs
        showArray [] = ""
    show (KDUndefined) = "???"
    show (KDRecord rc) = "Record: " ++ show rc where
        --showRecord ((name', data', type', canModify'):rest) = name' ++ " " ++ (show data') ++ ":" ++ (show type') ++ " mut:" ++ (show canModify') ++ showRecord rest
        --showRecord [] = []
    show (KDError message) = "Error: " ++ message

data Variable = Variable {varMutable :: Bool, varName :: String, varType :: KType} deriving Show

data Exception = Exception {excMessage :: String} deriving Show
 
