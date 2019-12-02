{-# LANGUAGE DuplicateRecordFields #-}

module Ast where

    data Class = Class {name :: String, fields :: [Variable], methods :: [Fun], classes :: [Class]}
        deriving Show

    data Fun = Fun {name :: String, args :: [Variable], returnType :: KType, body :: [FunPrimitive]} deriving Show

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
        | CallFun {name :: String, fargs :: [Expr]}
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
        deriving Show

    data KData = KDUnit
        | KDNull
        | KDBool Bool
        | KDChar Char
        | KDInt Int
        | KDDouble Double
        | KDArray [KData]
        | KDRecord [(String, KData)]
        | KDUndefined
        deriving Show
    
    data Variable = Variable {varMutable :: Bool, varName :: String, varType :: KType} deriving Show

    data Exception = Exception {excMessage :: String} deriving Show
 
