{-# LANGUAGE DuplicateRecordFields #-}

module Interpreter where

    import Ast
    import Text.Pretty.Simple (pPrint)

    data Log a = Log String a

    instance Show a => Show (Log a) where
        show (Log title datas) = "Log: " ++ title ++ " = " ++ show datas

    data InterObject =
          InterFun
        | InterVar {name :: String, ktype :: KType, kdata :: KData, canModify :: Bool, connectedVariable :: Maybe String}
        | InterBlock
        deriving Show

    dataConversionFromTypeToType :: KData -> KType -> KType -> KData
    dataConversionFromTypeToType (KDError m) _ _ = KDError m
    dataConversionFromTypeToType (KDInt x) _ KTByte = KDInt $ mod (x + 2 ^ 7) (2 ^ 8) - 2 ^ 7
    dataConversionFromTypeToType (KDInt x) _ KTShort = KDInt $ mod (x + 2 ^ 15) (2 ^ 16) - 2 ^ 15
    dataConversionFromTypeToType (KDInt x) _ KTInt = KDInt $ mod (x + 2 ^ 31) (2 ^ 32) - 2 ^ 31
    dataConversionFromTypeToType (KDInt x) _ KTLong = KDInt $ mod (x + 2 ^ 63) (2 ^ 64) - 2 ^ 63
    dataConversionFromTypeToType kdata ktype1 ktype2 | ktype1 == ktype2 = kdata
    dataConversionFromTypeToType kdata ktype1 (KTNullable (KTNullable ktype2)) = KDError "Cannot conversion to double-nullable type"
    dataConversionFromTypeToType KDNull (KTNullable _) (KTNullable _) = KDNull
    dataConversionFromTypeToType kdata (KTNullable ktype1) (KTNullable ktype2) = dataConversionFromTypeToType kdata ktype1 ktype2
    dataConversionFromTypeToType kdata ktype1 (KTNullable ktype2) = dataConversionFromTypeToType kdata ktype1 ktype2
    dataConversionFromTypeToType kdata ktype1 KTAny = kdata
    dataConversionFromTypeToType (KDArray []) (KTArray ktype1) (KTArray ktype2) = KDArray []
    dataConversionFromTypeToType (KDArray (kdata : kdatas)) (KTArray ktype1) (KTArray ktype2) = case dataConversionFromTypeToType kdata ktype1 ktype2 of
        KDError m -> KDError m
        kdata' -> KDArray $ kdata' : kdatas' where
            (KDArray kdatas') = dataConversionFromTypeToType (KDArray kdatas) (KTArray ktype1) (KTArray ktype2)
    dataConversionFromTypeToType (KDInt 0) _ KTBool = KDBool False
    dataConversionFromTypeToType (KDInt x) _ KTBool = KDBool True
    dataConversionFromTypeToType kdata ktype1 ktype2 = KDError $ show kdata ++ " cannot conversion from type " ++ show ktype1 ++ " to type " ++ show ktype2

    autoInferenceTypeFromData :: KData -> KType
    autoInferenceTypeFromData KDUnit = KTUnit
    autoInferenceTypeFromData KDNull = KTNullable KTAny
    autoInferenceTypeFromData (KDBool _) = KTBool
    autoInferenceTypeFromData (KDChar _) = KTChar
    autoInferenceTypeFromData (KDInt _) = KTInt
    autoInferenceTypeFromData (KDDouble _) = KTDouble
    autoInferenceTypeFromData (KDArray []) = KTArray KTAny
    autoInferenceTypeFromData (KDArray (obj : objs)) = KTArray $ autoInferenceTypeFromData obj
    autoInferenceTypeFromData KDUndefined = KTUnknown
    autoInferenceTypeFromData (KDError _) = KTUnknown

    interpretProgram :: [Primitive] -> IO ()
    interpretProgram program = interpretFun program [] "Main" [] >>= (\(kdata, ktype, []) -> case (kdata, ktype) of
        (KDError m, _) -> do
            putStrLn $ "\n\nError: " ++ m
            return ()
        (KDUnit, KTUnit) -> do
            putStrLn $ "\n\nProgram ended successfully"
            return ()
        _ -> do
            putStrLn $ "\n\nError: Function Main returned " ++ show kdata ++ " of type " ++ show ktype ++ ", but was expected unit"
            return ())

    interpretFun :: [Primitive] -> [InterObject] -> String -> [Expr] -> IO (KData, KType, [InterObject])
    interpretFun = \program -> interFun program program where
        interFun program ((Fun {name = name, args = args, returnType = returnType, body = body}):ps) stack nameFun argsFun | name == nameFun && name !! 0 /= '.' && length args == length argsFun = do
            (kdatas, ktypes, stack') <- interpretFunArgs program stack argsFun
            interpretBlock program (((\(Variable {varMutable = varMutable, varName = varName, varType = varType}, kdata, ktype) -> InterVar {name = varName, ktype = varType, kdata = dataConversionFromTypeToType kdata ktype varType, canModify = varMutable, connectedVariable = Nothing}) <$> (zip3 args kdatas ktypes)) ++ [InterFun] ++ stack') body
        interFun program (p:ps) stack nameFun args = interFun program ps stack nameFun args
        interFun program [] stack nameFun args = return (KDError $ "Function " ++ nameFun ++ " was not found", KTAny, stack)

    interpretFunArgs :: [Primitive] -> [InterObject] -> [Expr] -> IO ([KData], [KType], [InterObject])
    interpretFunArgs = \program -> interFunArgs program where
        interFunArgs program stack (arg:args) = do
            (kdata, ktype, stack') <- interpretExpression program stack arg
            (kdatas, ktypes, stack'') <- interFunArgs program stack' args
            return (kdata : kdatas, ktype : ktypes, stack'')
        interFunArgs program stack [] = return ([], [], stack)

    interpretBlock :: [Primitive] -> [InterObject] -> [FunPrimitive] -> IO (KData, KType, [InterObject])
    interpretBlock program stack = interBlock program (InterBlock : stack) where
        deleteBlock (InterBlock : InterFun : objs) = objs
        deleteBlock (InterBlock : objs) = objs
        deleteBlock (_ : objs) = deleteBlock objs
        interBlock program stack [fp] = do
            (kdata, ktype, stack') <- interpretFunPrimitive program stack fp
            return (kdata, ktype, deleteBlock stack')
        interBlock program stack (fp:fps) = do
            (kdata, ktype, stack') <- interpretFunPrimitive program stack fp
            case kdata of
                KDError m -> return (kdata, ktype, deleteBlock stack')
                _         -> interBlock program stack' fps
        interBlock program stack [] = return (KDUnit, KTUnit, deleteBlock stack)

    interpretFunPrimitive :: [Primitive] -> [InterObject] -> FunPrimitive -> IO (KData, KType, [InterObject])
    interpretFunPrimitive = \program -> interFunPrimitive program where
        interFunPrimitive program stack (Expression expr) = interpretExpression program stack expr
        interFunPrimitive program stack (ValInit {name = name, ktype = ktype}) =
            return (KDUndefined, KTUnknown, (InterVar {name = name, ktype = ktype, kdata = KDUndefined, canModify = False, connectedVariable = Nothing}) : stack)
        interFunPrimitive program stack (VarInit {name = name, ktype = ktype}) =
            return (KDUndefined, KTUnknown, (InterVar {name = name, ktype = ktype, kdata = KDUndefined, canModify = True, connectedVariable = Nothing}) : stack)

    interpretExpression :: [Primitive] -> [InterObject] -> Expr -> IO (KData, KType, [InterObject])
    interpretExpression = \program -> interExpression program where
        interExpression program stack (Val kdata) = return (kdata, autoInferenceTypeFromData kdata, stack)
        interExpression program stack (CallFun {name = "print", fargs = [exprMessage]}) = do
            (kdata, ktype, stack') <- interExpression program stack exprMessage
            case kdata of
                KDError m -> return (kdata, ktype, stack')
                _ -> do
                    putStr $ show kdata
                    return (KDUnit, KTUnit, stack')
        interExpression program stack (CallFun {name = "println", fargs = [exprMessage]}) = do
            (kdata, ktype, stack') <- interExpression program stack exprMessage
            case kdata of
                KDError m -> return (kdata, ktype, stack')
                _ -> do
                    putStrLn $ show kdata
                    return (KDUnit, KTUnit, stack')
        interExpression program stack (CallFun {name = "readLine", fargs = []}) = do
            inputStr <- getLine
            return (KDArray $ KDChar <$> inputStr, KTNullable $ KTArray KTChar, stack)
        interExpression program stack (CallFun {name = ".set", fargs = [Var varName, expr]}) = do
            (newVal, typeNewVal, stack') <- interExpression program stack expr
            interSet stack' varName newVal typeNewVal where
                interSet (obj : objs) varName newVal typeNewVal = case obj of
                    InterVar {name = varName, ktype = varKType, kdata = varKData, canModify = True, connectedVariable = Nothing} -> do
                        let (resultKData, resultKType) = if varKType == KTUnknown then (newVal, typeNewVal) else (dataConversionFromTypeToType newVal typeNewVal varKType, varKType)
                        return ((case resultKData of
                            KDError m -> resultKData
                            _ -> KDUndefined), KTUnknown, (InterVar {name = varName, ktype = resultKType, kdata = resultKData, canModify = True, connectedVariable = Nothing}) : objs)
                    InterVar {name = varName, ktype = varKType, kdata = KDUndefined, canModify = False, connectedVariable = Nothing} -> do
                        let (resultKData, resultKType) = if varKType == KTUnknown then (newVal, typeNewVal) else (dataConversionFromTypeToType newVal typeNewVal varKType, varKType)
                        return ((case resultKData of
                            KDError m -> resultKData
                            _ -> KDUndefined), KTUnknown, (InterVar {name = varName, ktype = resultKType, kdata = resultKData, canModify = False, connectedVariable = Nothing}) : objs)
                    _ -> do
                        (kdata, ktype, objs') <- interSet objs varName newVal typeNewVal
                        return (kdata, ktype, obj : objs')
                interSet [] varName newVal typeNewVal = return (KDError $ "Variable " ++ varName ++ " was not found", KTUnknown, [])
        interExpression program stack (CallFun {name = ".get", fargs = [Var "true"]}) = return (KDBool True, KTBool, stack)
        interExpression program stack (CallFun {name = ".get", fargs = [Var "false"]}) = return (KDBool False, KTBool, stack)
        interExpression program stack (CallFun {name = ".get", fargs = [Var "null"]}) = return (KDNull, KTNullable KTAny, stack)
        interExpression program stack (CallFun {name = ".get", fargs = [Var "unit"]}) = return (KDUnit, KTUnit, stack)
        interExpression program (obj : objs) (CallFun {name = ".get", fargs = [Var varName]}) = case obj of
            InterVar {name = varName', ktype = varKType, kdata = varKData, canModify = varCanModify, connectedVariable = Nothing} | varName == varName' -> do
                return (varKData, varKType, obj : objs)
            _ -> do
                (kdata, ktype, objs') <- interExpression program objs (CallFun {name = ".get", fargs = [Var varName]})
                return (kdata, ktype, obj : objs')
        interExpression program [] (CallFun {name = ".get", fargs = [Var varName]}) = return (KDError $ "Variable " ++ varName ++ " was not found", KTUnknown, [])
        interExpression program (obj : objs) (CallFun {name = ".get", fargs = [Var varName, expr]}) = case obj of
            InterVar {name = varName', ktype = KTArray varKTypeKernel, kdata = KDArray varKDatasKernel, canModify = varCanModify, connectedVariable = Nothing} | varName == varName' -> do
                (kdataIndex, ktypeIndex, stack') <- interExpression program (obj : objs) expr
                case kdataIndex of
                    KDError _ -> return (kdataIndex, ktypeIndex, stack')
                    KDInt index -> do
                        return $
                            if ktypeIndex /= KTByte && ktypeIndex /= KTShort && ktypeIndex /= KTInt && ktypeIndex /= KTLong then
                                (KDError "Index of array must be integer", KTUnknown, stack')
                            else if index < 0 || fromInteger index > length varKDatasKernel then
                                (KDError "Index outside the bounds of array", KTUnknown, stack')
                            else
                                (varKDatasKernel !! fromInteger index, varKTypeKernel, stack')
                    _ -> return (KDError "Index of array must be integer", KTUnknown, stack')
            _ -> do
                (kdata, ktype, objs') <- interExpression program objs (CallFun ".get" [Var varName, expr])
                return (kdata, ktype, obj : objs')
        interExpression program [] (CallFun {name = ".get", fargs = [Var varName, expr]}) = return (KDError $ "Variable " ++ varName ++ " was not found", KTUnknown, [])
        interExpression program stack (CallFun {name = ".toBool", fargs = [expr]}) = do
            (kdata, ktype, stack') <- interExpression program stack expr
            let res = dataConversionFromTypeToType kdata ktype KTBool
            return (res, (case res of
                KDError _ -> KTUnknown
                _ -> KTBool), stack')
        interExpression program stack (CallFun ".unnullify" [expr]) = do
            (kdata, ktype, stack') <- interExpression program stack expr
            case (kdata, ktype) of
                (KDError _, _) -> return (kdata, ktype, stack')
                (KDNull, KTNullable _) -> return (KDError "Still null", KTUnknown, stack')
                (_, KTNullable ktypeKernel) -> return (kdata, ktypeKernel, stack')
                (_, _) -> return (kdata, ktype, stack')
        interExpression program stack (CallFun {name = name, fargs = fargs}) = interpretFun program stack name fargs
        interExpression program stack (Add e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interAdd kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interAdd (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x + y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
                interAdd (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) + y), KTDouble)
                interAdd (KDInt 0) _ (KDBool False) KTBool = (KDBool False, KTBool)
                interAdd (KDInt x) _ (KDBool _) KTBool = (KDBool True, KTBool)
                interAdd (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x + (fromInteger y)), KTDouble)
                interAdd (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x + y), KTDouble)
                interAdd (KDDouble x) KTDouble (KDBool y) KTBool = (KDBool True, KTBool)
                interAdd (KDBool False) KTBool (KDInt 0) _ = (KDBool False, KTBool)
                interAdd (KDBool x) KTBool (KDInt y) _ = (KDBool True, KTBool)
                interAdd (KDBool x) KTBool (KDDouble y) KTDouble = (KDBool True, KTBool)
                interAdd (KDBool False) KTBool (KDBool False) KTBool = (KDBool False, KTBool)
                interAdd (KDBool x) KTBool (KDBool y) KTBool = (KDBool True, KTBool)
                interAdd (KDChar x) KTChar (KDChar y) KTChar = (KDArray [KDChar x, KDChar y], KTArray KTChar)
                interAdd (KDChar x) KTChar (KDArray ys) (KTArray KTChar) = (KDArray $ (KDChar x) : ys, KTArray KTChar)
                interAdd (KDArray xs) (KTArray KTChar) (KDChar y) KTChar = (KDArray $ xs ++ [KDChar y], KTArray KTChar)
                interAdd (KDArray []) (KTArray _) (KDArray ys) (KTArray ktype) = (KDArray ys, KTArray ktype)
                interAdd (KDArray (x : xs)) (KTArray ktype1) (KDArray ys) (KTArray ktype2) = case dataConversionFromTypeToType x ktype1 ktype2 of 
                    KDError m -> (KDError m, KTUnknown)
                    kdata' ->
                        let (KDArray kdataRes, KTArray ktypeRes) = interAdd (KDArray xs) (KTArray ktype1) (KDArray ys) (KTArray ktype2) in
                            (KDArray (kdata' : kdataRes), KTArray ktypeRes)
                interAdd kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be added " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
        interExpression program stack (Sub e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interSub kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interSub (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x - y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
                interSub (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) - y), KTDouble)
                interSub (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x - (fromInteger y)), KTDouble)
                interSub (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x - y), KTDouble)
                interSub kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be subtracted " ++ show kdata2 ++ " of type " ++ show ktype2 ++ " from " ++ show kdata1 ++ " of type " ++ show ktype1, KTUnknown)
        interExpression program stack (Mul e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interMul kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interMul (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x * y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
                interMul (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) * y), KTDouble)
                interMul (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x * (fromInteger y)), KTDouble)
                interMul (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x * y), KTDouble)
                interMul kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be multiplied " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " by " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
        interExpression program stack (Div e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interDiv kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interDiv (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x `div` y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
                interDiv (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) / y), KTDouble)
                interDiv (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x / (fromInteger y)), KTDouble)
                interDiv (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x / y), KTDouble)
                interDiv kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be divided " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " by " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
        interExpression program stack (Mod e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interMod kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interMod (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x `mod` y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
                interMod kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot take a module of " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " when dividing by " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
        interExpression program stack (And e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interAnd kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interAnd (KDBool True) KTBool (KDBool True) KTBool = (KDBool True, KTBool)
                interAnd (KDBool _) KTBool (KDBool _) KTBool = (KDBool False, KTBool)
                interAnd kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot take the logical and between " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
        interExpression program stack (Or e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interOr kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interOr (KDBool False) KTBool (KDBool False) KTBool = (KDBool False, KTBool)
                interOr (KDBool _) KTBool (KDBool _) KTBool = (KDBool True, KTBool)
                interOr kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot take the logical or between " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
        interExpression program stack (Not e) = do
            (kdata, ktype, stack') <- interExpression program stack e
            let (kdataRes, ktypeRes) = interNot kdata ktype
            return (kdataRes, ktypeRes, stack') where
                interNot (KDBool x) KTBool = (KDBool (not x), KTBool)
                interNot kdata ktype = (KDError $ "Cannot take the logical not for" ++ show kdata ++ " of type " ++ show ktype, KTUnknown)
        interExpression program stack (Less e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interLess kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interLess (KDInt x) ktype1 (KDInt y) ktype2 = (KDBool (x < y), KTBool)
                interLess (KDInt x) _ (KDDouble y) KTDouble = (KDBool ((fromInteger x) < y), KTBool)
                interLess (KDDouble x) KTDouble (KDInt y) _ = (KDBool (x < (fromInteger y)), KTBool)
                interLess (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDBool (x < y), KTBool)
                interLess kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be compared " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
        interExpression program stack (Equal e1 e2) = do
            (kdata1, ktype1, stack') <- interExpression program stack e1
            (kdata2, ktype2, stack'') <- interExpression program stack' e2
            let (kdataRes, ktypeRes) = interEqual kdata1 ktype1 kdata2 ktype2
            return (kdataRes, ktypeRes, stack'') where
                interEqual (KDInt x) ktype1 (KDInt y) ktype2 = (KDBool (x == y), KTBool)
                interEqual (KDInt x) _ (KDDouble y) KTDouble = (KDBool False, KTBool)
                interEqual (KDDouble x) KTDouble (KDInt y) _ = (KDBool False, KTBool)
                interEqual (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDBool False, KTBool)
                interEqual (KDChar x) KTChar (KDChar y) KTChar = (KDBool (x == y), KTBool)
                interEqual (KDBool x) KTBool (KDBool y) KTBool = (KDBool (x == y), KTBool)
                interEqual (KDArray []) (KTArray ktype1) (KDArray []) (KTArray ktype2) = (KDBool True, KTBool)
                interEqual (KDArray _) (KTArray ktype1) (KDArray []) (KTArray ktype2) = (KDBool False, KTBool)
                interEqual (KDArray []) (KTArray ktype1) (KDArray _) (KTArray ktype2) = (KDBool False, KTBool)
                interEqual (KDArray (x : xs)) (KTArray ktype1) (KDArray (y : ys)) (KTArray ktype2) =
                    case interEqual x ktype1 y ktype2 of
                        (KDError m, KTUnknown) -> (KDError m, KTUnknown)
                        (KDBool False, KTBool) -> (KDBool False, KTBool)
                        (KDBool True, KTBool) -> interEqual (KDArray xs) (KTArray ktype1) (KDArray ys) (KTArray ktype2)
                interEqual kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be compared " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
        interExpression program stack (If cond thenBranch elseBranch) = do
            (kdata, ktype, stack') <- interExpression program stack cond
            case kdata of
                KDError m -> return (kdata, ktype, stack')
                KDBool True -> interpretBlock program stack' thenBranch
                KDBool False -> interpretBlock program stack' elseBranch
