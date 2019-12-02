{-# LANGUAGE DuplicateRecordFields #-}

module Interpreter where

    import Ast
    import Control.Monad.State.Lazy
    import Control.Monad.Reader

    data InterObject =
          InterFun
        | InterVar {name :: String, ktype :: KType, kdata :: KData, canModify :: Bool, connectedVariable :: Maybe String}
        | InterBlock
        deriving Show

    showKData :: KData -> String
    showKData (KDNull) = "null"
    showKData (KDBool True) = "true"
    showKData (KDBool False) = "false"
    showKData (KDChar c) = [c]
    showKData (KDInt x) = show x
    showKData (KDDouble x) = show x
    showKData (KDArray ((KDChar c):cs)) = c : showKData (KDArray cs)
    showKData (KDArray []) = []

    interpretProgram :: Class -> IO ()
    interpretProgram program = interpretFun (methods program) [] "Main" [] >> return ();

    interpretFun :: [Fun] -> [InterObject] -> String -> [Expr] -> IO (KData, [InterObject])
    interpretFun = \program -> interFun program program where
        interFun program ((Fun {funName = name, funArgs = args, funReturnType = returnType, funBody = body}):ps) stack nameFun argsFun 
            | name == nameFun && name !! 0 /= '.' && length args == length argsFun = do
                                        (kdatas, stack') <- interpretFunArgs program stack argsFun
                                        interpretBlock program (((\(Variable {varMutable = varMutable, varName = varName, varType = varType}, kdata) -> 
                                            InterVar {name = varName, ktype = varType, kdata = kdata, canModify = varMutable, connectedVariable = Nothing}) <$> (zip args kdatas)) ++ [InterFun] ++ stack') body
        interFun program (p:ps) stack nameFun argsFun = interFun program ps stack nameFun argsFun
        interFun program [] stack nameFun argsFun = return (KDUnit, stack)

    interpretFunArgs :: [Fun] -> [InterObject] -> [Expr] -> IO ([KData], [InterObject])
    interpretFunArgs = \program -> interFunArgs program where
        interFunArgs program stack (arg:args) = do
            (kdata, stack') <- interpretExpression program stack arg
            (kdatas, stack'') <- interFunArgs program stack' args
            return (kdata : kdatas, stack'')
        interFunArgs program stack [] = return ([], stack)

    interpretBlock :: [Fun] -> [InterObject] -> [FunPrimitive] -> IO (KData, [InterObject])
    interpretBlock = \program -> interBlock program where
        interBlock program stack [fp] = interpretFunPrimitive program stack fp
        interBlock program stack (fp:fps) = do
            (kdata, stack') <- interpretFunPrimitive program stack fp
            interBlock program stack' fps
        interBlock program stack [] = return (KDUnit, stack)

    interpretFunPrimitive :: [Fun] -> [InterObject] -> FunPrimitive -> IO (KData, [InterObject])
    interpretFunPrimitive = \program -> interFunPrimitive program where
        interFunPrimitive program stack (Expression expr) = interpretExpression program stack expr
        interFunPrimitive program stack (ValInit {name = name, ktype = ktype}) = do
            return (KDUnit, (InterVar {name = name, ktype = ktype, kdata = KDUndefined, canModify = False, connectedVariable = Nothing}) : stack)
        interFunPrimitive program stack (VarInit {name = name, ktype = ktype}) = do
            return (KDUnit, (InterVar {name = name, ktype = ktype, kdata = KDUndefined, canModify = True, connectedVariable = Nothing}) : stack)

    interpretExpression :: [Fun] -> [InterObject] -> Expr -> IO (KData, [InterObject])
    interpretExpression = \program -> interExpression program where
        interExpression :: [Fun] -> [InterObject] -> Expr -> IO (KData, [InterObject])
        interExpression program stack (Val kdata) = return (kdata, stack)
        interExpression program stack (CallFun {name = "print", fargs = [exprMessage]}) = do
            (kdata, stack') <- interExpression program stack exprMessage
            putStr (showKData kdata)
            return (KDUnit, stack')
        interExpression program stack (CallFun {name = "println", fargs = [exprMessage]}) = do
            (kdata, stack') <- interExpression program stack exprMessage
            putStrLn (showKData kdata)
            return (KDUnit, stack')
        interExpression program stack (CallFun {name = "readLine", fargs = []}) = do
            inputStr <- getLine
            return (KDArray $ KDChar <$> inputStr, stack)
        interExpression program stack (CallFun {name = ".set", fargs = [Var varName, expr]}) = do
            (newVal, stack') <- interExpression program stack expr
            interSet stack' varName newVal where
                interSet (obj : objs) varName newVal = case obj of
                    InterVar {name = varName, ktype = varKType, kdata = varKData, canModify = True, connectedVariable = Nothing} ->
                        return (KDUnit, (InterVar {name = varName, ktype = varKType, kdata = newVal, canModify = True, connectedVariable = Nothing}) : objs)
                    InterVar {name = varName, ktype = varKType, kdata = KDUndefined, canModify = False, connectedVariable = Nothing} ->
                        return (KDUnit, (InterVar {name = varName, ktype = varKType, kdata = newVal, canModify = False, connectedVariable = Nothing}) : objs)
                    _ -> do
                        (kdata, objs') <- interSet objs varName newVal
                        return (kdata, obj : objs')
        interExpression program stack (CallFun {name = ".get", fargs = [Var "true"]}) = return (KDBool True, stack)
        interExpression program stack (CallFun {name = ".get", fargs = [Var "false"]}) = return (KDBool False, stack)
        interExpression program (obj : objs) (CallFun {name = ".get", fargs = [Var varName]}) = case obj of
            InterVar {name = varName, ktype = varKType, kdata = varKData, canModify = varCanModify, connectedVariable = Nothing} -> return (varKData, obj : objs)
            _ -> do
                (kdata, objs') <- interExpression program objs (CallFun {name = ".get", fargs = [Var varName]})
                return (kdata, obj : objs')
        interExpression program stack (CallFun {name = name, fargs = fargs}) = interpretFun program stack name fargs

