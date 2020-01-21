{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Interpreter where
import Ast
import Text.Pretty.Simple (pPrint)
import Data.List.Split
import Debug.Trace

data Log a = Log String a

instance Show a => Show (Log a) where
    show (Log title datas) = "Log: " ++ title ++ " = " ++ show datas

data InterObject =
        InterFun
    | InterVar {name :: String, ktype :: KType, kdata :: KData, canModify :: Bool, connectedVariable :: Maybe Expr}
    | InterBlock
    | InterMainClass {cl :: Class}
    deriving Show

dataConversionFromTypeToType :: KData -> KType -> KType -> KData
dataConversionFromTypeToType (KDError m) _ _ = KDError m
dataConversionFromTypeToType (KDInt x) _ KTByte = KDInt $ mod (x + 2 ^ 7) (2 ^ 8) - 2 ^ 7
dataConversionFromTypeToType (KDInt x) _ KTShort = KDInt $ mod (x + 2 ^ 15) (2 ^ 16) - 2 ^ 15
dataConversionFromTypeToType (KDInt x) _ KTInt = KDInt $ mod (x + 2 ^ 31) (2 ^ 32) - 2 ^ 31
dataConversionFromTypeToType (KDInt x) _ KTLong = KDInt $ mod (x + 2 ^ 63) (2 ^ 64) - 2 ^ 63
dataConversionFromTypeToType (KDInt x) _ KTDouble = KDDouble $ fromInteger x
dataConversionFromTypeToType kdata ktype1 ktype2 | ktype1 == ktype2 = kdata
dataConversionFromTypeToType kdata ktype1 (KTNullable (KTNullable ktype2)) = KDError "Cannot convert to double-nullable type"
dataConversionFromTypeToType KDNull (KTNullable _) (KTNullable _) = KDNull
dataConversionFromTypeToType kdata (KTNullable ktype1) (KTNullable ktype2) = dataConversionFromTypeToType kdata ktype1 ktype2
dataConversionFromTypeToType kdata ktype1 (KTNullable ktype2) = dataConversionFromTypeToType kdata ktype1 ktype2
dataConversionFromTypeToType kdata ktype1 KTAny = kdata
dataConversionFromTypeToType kdata KTAny ktype2 = kdata
dataConversionFromTypeToType kdata ktype1 KTUnit = KDUnit
dataConversionFromTypeToType (KDArray []) (KTArray ktype1) (KTArray ktype2) = KDArray []
dataConversionFromTypeToType (KDArray (kdata : kdatas)) (KTArray ktype1) (KTArray ktype2) = case dataConversionFromTypeToType kdata ktype1 ktype2 of
    KDError m -> KDError m
    kdata' -> KDArray $ kdata' : kdatas' where
        (KDArray kdatas') = dataConversionFromTypeToType (KDArray kdatas) (KTArray ktype1) (KTArray ktype2)
dataConversionFromTypeToType (KDInt 0) _ KTBool = KDBool False
dataConversionFromTypeToType (KDInt x) _ KTBool = KDBool True
dataConversionFromTypeToType r@(KDRecord _) (KTUserType typeName) other = r
dataConversionFromTypeToType r@(KDRecord _) other (KTUserType typeName) = r
dataConversionFromTypeToType kdata ktype1 ktype2 = KDError $ show kdata ++ " cannot conversion from type " ++ show ktype1 ++ " to type " ++ show ktype2

autoInferenceTypeFromData :: KData -> KType
autoInferenceTypeFromData KDUnit = KTUnit
autoInferenceTypeFromData KDObject = KTAny
autoInferenceTypeFromData KDNull = KTNullable KTAny
autoInferenceTypeFromData (KDBool _) = KTBool
autoInferenceTypeFromData (KDChar _) = KTChar
autoInferenceTypeFromData (KDInt _) = KTInt
autoInferenceTypeFromData (KDDouble _) = KTDouble
autoInferenceTypeFromData (KDArray []) = KTArray KTAny
autoInferenceTypeFromData (KDArray (obj : objs)) = KTArray $ autoInferenceTypeFromData obj
autoInferenceTypeFromData KDUndefined = KTUnknown
autoInferenceTypeFromData (KDError _) = KTUnknown
autoInferenceTypeFromData (KDRecord _) = KTUserType "Unknown user type"

setupProgramStack :: Class -> [InterObject]
setupProgramStack program = ((\(Variable {..}) -> InterVar varName varType KDUndefined varMutable Nothing) <$> fields program) ++ --add all fields as variables to stack
                            [(InterMainClass program)]
interpretProgram :: Class -> IO ()
interpretProgram program =
    interpretFunByName program
                        (setupProgramStack program)
                        "Main" 
                        [] 
    >>= (\(kdata, ktype, stack) -> case (kdata, ktype) of
    (KDError m, _) -> do
        putStrLn $ "\n\nError: " ++ m
        return ()
    (KDUnit, KTUnit) -> do
        putStrLn $ "\n\nProgram ended successfully"
        return ()
    _ -> do
        putStrLn $ "\n\nError: Function Main returned " ++ show kdata ++ " of type " ++ show ktype ++ ", but was expected unit"
        return ())

findClassByName :: String -> [Class] -> Maybe Class
findClassByName targetName (curClass@(Class currentName _ _ _):cls)
    | (targetName == currentName) = return curClass
    | otherwise = findClassByName targetName cls
findClassByName targetName [] = Nothing

launchFun :: Fun -> [InterObject] -> [Expr] -> IO (KData, KType, [InterObject])
launchFun (Fun {..}) stack arguments = do 
        (kdatas, ktypes, stack') <- {-trace name $-} interpretFunArgs stack arguments
        let (kdatas', ktypes') = checkArgsTypes args kdatas ktypes where
            checkArgsTypes :: [Variable] -> [KData] -> [KType] -> ([KData],[KType])
            checkArgsTypes [] [] [] = ([], [])
            checkArgsTypes ((Variable {..}) : prevArgs) (kdata : prevKdatas) (ktype : prevKtypes) = case dataConversionFromTypeToType kdata ktype varType of
                KDError m -> ([KDError m], [KTUnknown])
                kdata' -> let (kdatas', ktypes') = checkArgsTypes prevArgs prevKdatas prevKtypes in
                    (kdata' : kdatas', varType : ktypes')
        case kdatas' of
            [KDError m] -> return (KDError $ "Arguments type mismatched in function " ++ name, KTUnknown, stack)
            _ -> do
                let argNames = varName <$> args
                let argMutables = varMutable <$> args
                let createVariable = (\(kdata', ktype', varName, varMutable, arg) -> InterVar varName ktype' kdata' False (if varMutable then Just arg else Nothing))
                let argsToStackFormat = createVariable <$> (zip5 kdatas' ktypes' argNames argMutables arguments) where
                    zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
                    zip5 (a : as) (b : bs) (c : cs) (d : ds) (e : es) = (a, b, c, d, e) : zip5 as bs cs ds es
                    zip5 [] [] [] [] [] = []
                --pPrint $ Log "Name started function" name
                --pPrint $ Log "Stack before working function" $ init stack'
                (kdataResult, ktypeResult, stack'') <- interpretBlock (argsToStackFormat ++ [InterFun] ++ stack') body
                let stack''' = deleteFun stack'' where
                    deleteFun (InterFun : objs) = objs
                    deleteFun (_ : objs) = deleteFun objs
                --pPrint $ Log "Name ended function" nameS
                --pPrint $ Log "Stack after working function" $ init stack'''
                return $ case dataConversionFromTypeToType kdataResult ktypeResult returnType of
                    KDError m -> trace ("Typecheck fail " ++ show returnType ++ " " ++ show ktypeResult ++ " " ++ show kdataResult) $ (KDError m, KTUnknown, stack''')
                    kdataResult' -> (kdataResult', returnType, stack''')

interpretFunByName :: Class -> [InterObject] -> String -> [Expr] -> IO (KData, KType, [InterObject])
interpretFunByName currentClass stack ('.' : name) (this : args) = do --ClassA.ClassB.f()
    (kdataThis, ktypeThis, stack') <- interpretExpression stack this
    case ktypeThis of
        KTUserType nameUserType -> do
            let subTypes = splitOn "." nameUserType
            descendInClasses currentClass stack' subTypes name (this : args) where
                descendInClasses :: Class -> [InterObject] -> [String] -> String -> [Expr] -> IO (KData, KType, [InterObject])
                descendInClasses currentClass stack subTypes funName args = do
                    case subTypes of
                        [] -> return (KDError $ "Empty name of user type", KTUnknown, stack)
                        (className:cls) -> do
                            let targetClass = findClassByName className (classes currentClass)
                            case targetClass of
                                Nothing -> return (KDError $ "No such class " ++ className, KTUnknown, stack)
                                (Just cl) -> case cls of
                                    [] -> interpretFunByName cl stack funName args
                                    _  -> descendInClasses cl stack cls funName args         
        _ -> return (KDError $ "Cannot override class " ++ show ktypeThis, KTUnknown, stack')

interpretFunByName currentClass@(Class clName clFlds (f@(Fun nameFun argsFun typeFun bodyFun) : otherFuns) clCls) stack name args
    | (nameFun == name) && (length argsFun == length args) = do
        (kdatas, ktypes, stack') <- launchFun f stack args  
        case kdatas of --if args types error then continue search
            (KDError ('A':'r':'g':other)) -> interpretFunByName (Class clName clFlds otherFuns clCls) stack name args
            _ -> return (kdatas, ktypes, stack')
    | otherwise = interpretFunByName (Class clName clFlds otherFuns clCls) stack name args

interpretFunByName (Class _ _ [] _) stack name (arg:args) = return (KDError $ "Function " ++ name ++ " was not found, arg :: " ++ show arg, KTAny, stack)
interpretFunByName (Class _ _ [] _) stack name _ = return (KDError $ "Function " ++ name ++ " was not found", KTAny, stack)

interpretFunArgs :: [InterObject] -> [Expr] -> IO ([KData], [KType], [InterObject])
interpretFunArgs stack (arg:args) = do
    (kdata, ktype, stack') <- interpretExpression stack arg
    (kdatas, ktypes, stack'') <- interpretFunArgs stack' args
    return (kdata : kdatas, ktype : ktypes, stack'')
interpretFunArgs stack [] = return ([], [], stack)

interpretBlock :: [InterObject] -> [FunPrimitive] -> IO (KData, KType, [InterObject])
interpretBlock stack = interBlock (InterBlock : stack) where
    deleteBlock (InterBlock : objs) = objs
    deleteBlock (_ : objs) = deleteBlock objs
    interBlock stack [fp] = do
        --pPrint $ Log "Stack" $ init stack
        --pPrint $ Log "Action" fp
        (kdata, ktype, stack') <- interpretFunPrimitive stack fp
        --pPrint $ Log "Stack" $ init stack'
        return (kdata, ktype, deleteBlock stack')
    interBlock stack (fp:fps) = do
        --pPrint $ Log "Stack" $ init stack
        --pPrint $ Log "Action" fp
        (kdata, ktype, stack') <- interpretFunPrimitive stack fp
        case kdata of
            KDError m -> return (kdata, ktype, deleteBlock stack')
            _         -> interBlock stack' fps
    interBlock stack [] = return (KDUnit, KTUnit, deleteBlock stack)

interpretFunPrimitive :: [InterObject] -> FunPrimitive -> IO (KData, KType, [InterObject])
interpretFunPrimitive stack (Expression expr) = interpretExpression stack expr
interpretFunPrimitive stack (ValInit {name = name, ktype = ktype}) =
    return (KDUndefined, KTUnknown, (InterVar {name = name, ktype = ktype, kdata = KDUndefined, canModify = False, connectedVariable = Nothing}) : stack)
interpretFunPrimitive stack (VarInit {name = name, ktype = ktype}) =
    return (KDUndefined, KTUnknown, (InterVar {name = name, ktype = ktype, kdata = KDUndefined, canModify = True, connectedVariable = Nothing}) : stack)
interpretFunPrimitive stack (While {..}) = do
    (kdataCond, ktypeCond, stack') <- interpretExpression stack cond
    case kdataCond of
        KDError _ -> return (kdataCond, ktypeCond, stack')
        KDBool True -> do
            (kdataBody, ktypeBody, stack'') <- interpretBlock stack' body
            case kdataBody of
                KDError _ -> return (kdataBody, ktypeBody, stack'')
                _ -> interpretFunPrimitive stack'' (While cond body)
        KDBool False -> return (KDUndefined, KTUnknown, stack')

createArrayByLambda :: String -> Integer -> [FunPrimitive] -> [InterObject] -> IO ([KData], KType)
createArrayByLambda _ 0 body stack = return ([], KTAny)
createArrayByLambda indexName size body stack = do
    --we prohibit stack changes in lambdas --here maybe mistake with "index"?
    (kdata, ktype, _ ) <- launchFun (Fun "Element" [Variable False indexName KTInt] KTAny body) stack [Val (KDInt (size - 1))]
    (kdatas, ktypeOther) <- (createArrayByLambda indexName (size - 1) body stack)
    return (kdata:kdatas, ktype)
            
interpretExpression :: [InterObject] -> Expr -> IO (KData, KType, [InterObject])
interpretExpression stack (Val kdata) = return (kdata, autoInferenceTypeFromData kdata, stack)
interpretExpression stack (CallFun {name = "print", args = [exprMessage]}) = do
    (kdata, ktype, stack') <- interpretExpression stack exprMessage
    case kdata of
        KDError m -> return (kdata, ktype, stack')
        _ -> do
            putStr $ show kdata
            return (KDUnit, KTUnit, stack')
interpretExpression stack (CallFun {name = "println", args = [exprMessage]}) = do
    (kdata, ktype, stack') <- interpretExpression stack exprMessage
    case kdata of
        KDError m -> return (kdata, ktype, stack')
        _ -> do
            putStrLn $ show kdata
            return (KDUnit, KTUnit, stack')
interpretExpression stack (CallFun {name = "readLine", args = []}) = do
    inputStr <- getLine
    return (KDArray $ KDChar <$> inputStr, KTNullable $ KTArray KTChar, stack)

interpretExpression stack (CallFun ".array" [Val (KDInt size), (Lambda ((Variable {..}):[]) body) ]) = do
    (array, elementType) <- createArrayByLambda varName size body stack
    return (KDArray (reverse array), KTArray elementType, stack)

interpretExpression stack (CallFun ".array" _ ) = return (KDError "Illegal initial array arguments", KTUnknown, stack)

interpretExpression stack (CallFun ".get" [Var "true"]) = return (KDBool True, KTBool, stack)
interpretExpression stack (CallFun ".get" [Var "false"]) = return (KDBool False, KTBool, stack)
interpretExpression stack (CallFun ".get" [Var "null"]) = return (KDNull, KTNullable KTAny, stack)
interpretExpression stack (CallFun ".get" [Var "unit"]) = return (KDUnit, KTUnit, stack)
interpretExpression stack (CallFun ".get" (variable : fields)) = do
    (kdataVar, ktypeVar, stack') <- case variable of
        Var name -> takeFromStack stack name where
            takeFromStack :: [InterObject] -> String -> IO (KData, KType, [InterObject])
            takeFromStack (obj@(InterVar {..}) : objs) nameVar
                | name == nameVar = return (kdata, ktype, obj : objs)
                | True = do
                    (kdataRes, ktypeRes, _) <- takeFromStack objs nameVar
                    return (kdataRes, ktypeRes, obj : objs)
            takeFromStack (obj : objs) nameVar = do
                (kdataRes, ktypeRes, _) <- takeFromStack objs nameVar
                return (kdataRes, ktypeRes, obj : objs)
            takeFromStack [] nameVar = return (KDError $ "Variable " ++ nameVar ++ " was not found", KTUnknown, [])
        _ -> interpretExpression stack variable
    getFields stack' kdataVar ktypeVar fields where
        getFields stack kdataVar ktypeVar [] = return (kdataVar, ktypeVar, stack)
        getFields stack (KDRecord ((nameFieldVar, kdataFieldVar, ktypeFieldVar, canModifyFieldVar) : fieldsVar)) ktypeVar@(KTUserType _) ((Var fieldName) : fields)
            | nameFieldVar == fieldName = getFields stack kdataFieldVar ktypeFieldVar fields
            | True = getFields stack (KDRecord fieldsVar) ktypeVar ((Var fieldName) : fields)
        getFields stack (KDArray kdatas) (KTArray ktype) (field : fields) = do
            (kdataIndex, ktypeIndex, stack') <- interpretExpression stack field
            case kdataIndex of
                KDError m -> return (KDError m, KTUnknown, stack')
                KDInt index ->
                    if ktypeIndex /= KTByte && ktypeIndex /= KTShort && ktypeIndex /= KTInt && ktypeIndex /= KTLong then
                        return (KDError $ "Index of array cannot of type " ++ show ktypeIndex, KTUnknown, stack')
                    else if index < 0 || fromInteger index >= length kdatas then
                        return (KDError $ "Index " ++ show index ++ " outside the bounds of the array", KTUnknown, stack')
                    else
                        getFields stack' (kdatas !! fromInteger index) ktype fields
                _ -> return (KDError $ show kdataIndex ++ " is not index of array", KTUnknown, stack')
        getFields stack _ ktypeVar ((Var fieldName) : fields) = return (KDError $ "The variable of type " ++ show ktypeVar ++ " does not have the field " ++ fieldName, KTUnknown, stack)
        getFields stack _ ktypeVar (field : fields) = return (KDError $ "Cannot take an index from the variable of type " ++ show ktypeVar, KTUnknown, stack)

interpretExpression stack (CallFun ".set" (exprNewVal : (Var varName) : fields)) = do
    (kdataNewVal, ktypeNewVal, stack') <- interpretExpression stack exprNewVal
    interSet stack' stack' varName fields kdataNewVal ktypeNewVal where
        interSet :: [InterObject] -> [InterObject] -> String -> [Expr] -> KData -> KType -> IO (KData, KType, [InterObject])
        interSet stack (obj@(InterVar {..}) : objs) varName fields kdataNewVal ktypeNewVal
            | (name == varName) = do
                --found needed variable
                (varKData, varKType, stack') <- helperSet stack kdata ktype fields kdataNewVal ktypeNewVal
                updateStack stack' varName varKData varKType -- where
            | otherwise = do
                --continue variable search
                (kdata', ktype', objs') <- interSet stack objs varName fields kdataNewVal ktypeNewVal
                return (kdata', ktype', obj : objs') where

                    helperSet :: [InterObject] -> KData -> KType -> [Expr] -> KData -> KType -> IO (KData, KType, [InterObject])
                    helperSet stack kdataOldVar KTUnknown [] kdataNewVal ktypeNewVal = return (kdataNewVal, ktypeNewVal, stack)
                    helperSet stack kdataOldVar ktypeOldVar [] kdataNewVal ktypeNewVal =
                        return $ case dataConversionFromTypeToType kdataNewVal ktypeNewVal ktypeOldVar of
                            KDError m -> (KDError m, KTUnknown, stack)
                            kdataRes -> (kdataRes, ktypeOldVar, stack)
                    helperSet stack (KDRecord ((fieldNameOldVar, fieldKDataOldVar, fieldKTypeOldVar, fieldCanModifyOldVar) : fieldsOldVar))
                                    (KTUserType nameUserType)
                                    ((Var fieldName) : fields)
                                    kdataNewVals
                                    ktypeNewVal
                        | (fieldNameOldVar == fieldName) && (fieldCanModifyOldVar == True || fieldKTypeOldVar == KTUnknown || ((length fieldsOldVar) > 0)) = do
                            (kdataRes, ktypeRes, stack') <- helperSet stack fieldKDataOldVar fieldKTypeOldVar fields kdataNewVal ktypeNewVal
                            return $ case kdataRes of
                                KDError _ -> (kdataRes, KTUnknown, stack')
                                _ -> (KDRecord ((fieldNameOldVar, kdataRes, ktypeRes, fieldCanModifyOldVar) : fieldsOldVar), KTUserType nameUserType, stack')
                        | (fieldNameOldVar == fieldName) = return (KDError $ "Cannot assign a new value to the val-field " ++ fieldName, KTUnknown, stack)
                        | otherwise = do
                            (kdataRes, ktypeRes, stack') <- helperSet stack (KDRecord fieldsOldVar) (KTUserType nameUserType) ((Var fieldName) : fields) kdataNewVal ktypeNewVal
                            return $ case kdataRes of
                                KDError _ -> (kdataRes, KTUnknown, stack')
                                KDRecord fieldsNewVar -> (KDRecord ((fieldNameOldVar, fieldKDataOldVar, fieldKTypeOldVar, fieldCanModifyOldVar) : fieldsNewVar), KTUserType nameUserType, stack')
                    helperSet stack (KDRecord []) (KTUserType nameUserType) ((Var fieldName) : fields) kdataNewVal ktypeNewVal =
                        return (KDError $ "Field " ++ fieldName ++ " in type " ++ nameUserType ++ " was not found", KTUnknown, stack)
                    helperSet stack _ (KTUserType _) ((Var fieldName) : fields) kdataNewVal ktypeNewVal =
                        return (KDError $ "Data and type mismatch", KTUnknown, stack)
                    helperSet stack _ ktypeOldVar ((Var fieldName) : fields) kdataNewVal ktypeNewVal =
                        return (KDError $ "Type " ++ show ktypeOldVar ++ " has no fields", KTUnknown, stack)
                    helperSet stack (KDArray kdatasKernelOldVar) (KTArray ktypeKernelOldVar) (exprIndex : fields) kdataNewVal ktypeNewVal = do
                        (kdataIndex, ktypeIndex, stack') <- interpretExpression stack exprIndex
                        case kdataIndex of
                            KDError _ -> return (kdataIndex, KTUnknown, stack')
                            KDInt index ->
                                if ktypeIndex == KTByte || ktypeIndex == KTShort || ktypeIndex == KTInt || ktypeIndex == KTLong then
                                    if index < 0 || fromInteger index >= length kdatasKernelOldVar then
                                        return (KDError $ "Index " ++ show index ++ " outside the bounds of the array", KTUnknown, stack')
                                    else do
                                        (kdataNewVar, ktypeNewVar, stack'') <- helperSet stack' (kdatasKernelOldVar !! fromInteger index) ktypeKernelOldVar fields kdataNewVal ktypeNewVal
                                        case kdataNewVar of
                                            KDError _ -> return (kdataNewVar, KTUnknown, stack'')
                                            _ ->
                                                if ktypeNewVar /= ktypeKernelOldVar then return (KDError $ "Cannot change array element type from " ++ show ktypeKernelOldVar ++ " to " ++ show ktypeNewVar, KTUnknown, stack'')
                                                else return (KDArray (take (fromInteger index) kdatasKernelOldVar ++ [kdataNewVar] ++ drop (fromInteger index + 1) kdatasKernelOldVar), KTArray ktypeKernelOldVar, stack'')
                                else return (KDError $ "Index of array cannot be the value of type " ++ show ktypeIndex, KTUnknown, stack')
                            _ -> return (KDError $ "Data " ++ show kdataIndex ++ " cannot be an index of array", KTUnknown, stack')
                    helperSet stack _ (KTArray _) (exprIndex : fields) kdataNewVal ktypeNewVal =
                        return (KDError $ "Data and type mismatch", KTUnknown, stack)
                    helperSet stack _ ktypeOldVar (exprIndex : fields) kdataNewVal ktypeNewVal =
                        return (KDError $ "Type " ++ show ktypeOldVar ++ " has no indexes", KTUnknown, stack)

                    splitStackByInterFun :: [InterObject] -> ([InterObject], [InterObject])
                    splitStackByInterFun (InterFun : objs) = ([], objs)
                    splitStackByInterFun (obj : objs) = (obj : revStackHead, stackTail) where
                        (revStackHead, stackTail) = splitStackByInterFun objs

                    updateStack :: [InterObject] -> String -> KData -> KType -> IO (KData, KType, [InterObject])
                    updateStack stack varName varKData varKType = do
                        let (var, revStackHead, stackTail) = findVarInStack stack varName where
                            findVarInStack :: [InterObject] -> String -> (Maybe InterObject, [InterObject], [InterObject])
                            findVarInStack (obj@(InterVar {..}) : objs) varName
                                | (name == varName) = (Just obj, [], objs)
                                | otherwise = do
                                    let (result, stackHead, stackTail) = findVarInStack objs varName
                                    (result, obj:stackHead, stackTail)
                            findVarInStack (obj:objs) varName = do
                                    let (result, stackHead, stackTail) = findVarInStack objs varName
                                    (result, obj:stackHead, stackTail)
                            findVarInStack [] _ = (Nothing, [], [])
                        case var of
                            (Just (InterVar {..})) -> do
                                let stackHead = reverse revStackHead
                                (KDUndefined, KTUnknown, newStackTail) <- case connectedVariable of
                                    Nothing -> return (KDUndefined, KTUnknown, stackTail)
                                    Just (CallFun ".get" ((Var varName') : fields')) -> do
                                        let (revStackHead', stackTail') = splitStackByInterFun stackTail
                                        (kdataSet, ktypeSet, newStackTail') <- interSet stackTail' stackTail' varName' (fields' ++ fields) kdataNewVal ktypeNewVal
                                        case kdataSet of
                                            KDUndefined -> return ()
                                            _ -> pPrint $ Log "Error" kdataSet
                                        return (KDUndefined, KTUnknown, reverse revStackHead' ++ [InterFun] ++ newStackTail')
                                case ktype of
                                    KTUnknown -> return (KDUndefined, KTUnknown, stackHead ++ ((InterVar name varKType varKData canModify connectedVariable) : newStackTail))
                                    _ -> case dataConversionFromTypeToType varKData varKType ktype of
                                            KDError m -> return (KDError m, KTUnknown, [])
                                            kdataRes -> return (KDUndefined, KTUnknown, stackHead ++ ((InterVar name ktype kdataRes canModify connectedVariable) : newStackTail))
                            Nothing -> return (KDError $ "Variable " ++ varName ++ " was not found", KTUnknown, [])
 
        --skip all non-variables
        interSet stack (obj : objs) varName fields kdataNewVal ktypeNewVal = do
            (kdata', ktype', objs') <- interSet stack objs varName fields kdataNewVal ktypeNewVal
            return (kdata', ktype', obj : objs')

        interSet stack [] varName fields kdataNewVal ktypeNewVal = return (KDError $ "Variable " ++ varName ++ " was not found", KTUnknown, stack)

interpretExpression stack (CallFun {name = ".toBool", args = [expr]}) = do
    (kdata, ktype, stack') <- interpretExpression stack expr
    let res = dataConversionFromTypeToType kdata ktype KTBool
    return (res, (case res of
        KDError _ -> KTUnknown
        _ -> KTBool), stack')
interpretExpression stack (CallFun ".unnullify" [expr]) = do
    (kdata, ktype, stack') <- interpretExpression stack expr
    case (kdata, ktype) of
        (KDError _, _) -> return (kdata, ktype, stack')
        (KDNull, KTNullable _) -> return (KDError "Still null", KTUnknown, stack')
        (_, KTNullable ktypeKernel) -> return (kdata, ktypeKernel, stack')
        (_, _) -> return (kdata, ktype, stack')
interpretExpression stack (CallFun {name = name, args = fargs}) = interpretFunByName program stack name fargs where
                                                                    (InterMainClass program) = last stack
interpretExpression stack (Add e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
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
interpretExpression stack (Sub e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
    let (kdataRes, ktypeRes) = interSub kdata1 ktype1 kdata2 ktype2
    return (kdataRes, ktypeRes, stack'') where
        interSub (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x - y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
        interSub (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) - y), KTDouble)
        interSub (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x - (fromInteger y)), KTDouble)
        interSub (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x - y), KTDouble)
        interSub kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be subtracted " ++ show kdata2 ++ " of type " ++ show ktype2 ++ " from " ++ show kdata1 ++ " of type " ++ show ktype1, KTUnknown)
interpretExpression stack (Mul e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
    let (kdataRes, ktypeRes) = interMul kdata1 ktype1 kdata2 ktype2
    return (kdataRes, ktypeRes, stack'') where
        interMul (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x * y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
        interMul (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) * y), KTDouble)
        interMul (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x * (fromInteger y)), KTDouble)
        interMul (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x * y), KTDouble)
        interMul kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be multiplied " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " by " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
interpretExpression stack (Div e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
    let (kdataRes, ktypeRes) = interDiv kdata1 ktype1 kdata2 ktype2
    return (kdataRes, ktypeRes, stack'') where
        interDiv (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x `div` y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
        interDiv (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) / y), KTDouble)
        interDiv (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x / (fromInteger y)), KTDouble)
        interDiv (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x / y), KTDouble)
        interDiv kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be divided " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " by " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
interpretExpression stack (Mod e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
    let (kdataRes, ktypeRes) = interMod kdata1 ktype1 kdata2 ktype2
    return (kdataRes, ktypeRes, stack'') where
        interMod (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x `mod` y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
        interMod kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot take a module of " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " when dividing by " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
interpretExpression stack (And e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
    let (kdataRes, ktypeRes) = interAnd kdata1 ktype1 kdata2 ktype2
    return (kdataRes, ktypeRes, stack'') where
        interAnd (KDBool True) KTBool (KDBool True) KTBool = (KDBool True, KTBool)
        interAnd (KDBool _) KTBool (KDBool _) KTBool = (KDBool False, KTBool)
        interAnd kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot take the logical and between " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
interpretExpression stack (Or e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
    let (kdataRes, ktypeRes) = interOr kdata1 ktype1 kdata2 ktype2
    return (kdataRes, ktypeRes, stack'') where
        interOr (KDBool False) KTBool (KDBool False) KTBool = (KDBool False, KTBool)
        interOr (KDBool _) KTBool (KDBool _) KTBool = (KDBool True, KTBool)
        interOr kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot take the logical or between " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
interpretExpression stack (Not e) = do
    (kdata, ktype, stack') <- interpretExpression stack e
    let (kdataRes, ktypeRes) = interNot kdata ktype
    return (kdataRes, ktypeRes, stack') where
        interNot (KDBool x) KTBool = (KDBool (not x), KTBool)
        interNot kdata ktype = (KDError $ "Cannot take the logical not for" ++ show kdata ++ " of type " ++ show ktype, KTUnknown)
interpretExpression stack (Less e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
    let (kdataRes, ktypeRes) = interLess kdata1 ktype1 kdata2 ktype2
    return (kdataRes, ktypeRes, stack'') where
        interLess (KDInt x) ktype1 (KDInt y) ktype2 = (KDBool (x < y), KTBool)
        interLess (KDInt x) _ (KDDouble y) KTDouble = (KDBool ((fromInteger x) < y), KTBool)
        interLess (KDDouble x) KTDouble (KDInt y) _ = (KDBool (x < (fromInteger y)), KTBool)
        interLess (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDBool (x < y), KTBool)
        interLess kdata1 ktype1 kdata2 ktype2 = (KDError $ "Cannot be compared " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2, KTUnknown)
interpretExpression stack (Equal e1 e2) = do
    (kdata1, ktype1, stack') <- interpretExpression stack e1
    (kdata2, ktype2, stack'') <- interpretExpression stack' e2
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
interpretExpression stack (If cond thenBranch elseBranch) = do
    (kdata, ktype, stack') <- interpretExpression stack cond
    case kdata of
        KDError m -> return (kdata, ktype, stack')
        KDBool True -> interpretBlock stack' thenBranch
        KDBool False -> interpretBlock stack' elseBranch
