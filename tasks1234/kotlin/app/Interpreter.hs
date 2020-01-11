{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

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
    dataConversionFromTypeToType kdata ktype1 KTUnit = KDUnit
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

    interpretFunByName :: Class -> [InterObject] -> String -> [Expr] -> IO (KData, KType, [InterObject])

    interpretFunByName currentClass stack ('.' : name) (this : args) = do --ClassA.ClassB.f()
        (kdataThis, ktypeThis, stack') <- interpretExpression stack this
        case ktypeThis of
            KTUserType nameUserType -> helperFunByName currentClass stack "" nameUserType name args where 
                -- here we want to go from CallFun .f [CallFun .get [Var ClassA, ClassB]] to choosing currentClass as ClassA.ClassB and
                -- executing f without this 
                helperFunByName :: Class -> [InterObject] -> String -> String -> String -> [Expr] -> IO (KData, KType, [InterObject])

                helperFunByName (Class _ _ _ (cl@(Class nameClass _ _ _) : cls)) stack headUserType "" nameFun argsFun -- no more symbols
                    | (nameClass == reverse headUserType) = interpretFunByName cl stack nameFun argsFun
                    | otherwise = helperFunByName (Class "" [] [] cls) stack headUserType "" nameFun argsFun

                helperFunByName (Class _ _ _ (cl@(Class nameClass _ _ _) : cls)) stack headUserType ('.' : tailUserType) nameFun argsFun
                    | (nameClass == reverse headUserType) = helperFunByName cl stack "" tailUserType nameFun argsFun
                    | otherwise = helperFunByName (Class "" [] [] cls) stack headUserType ('.' : tailUserType) nameFun argsFun

                helperFunByName currentClass stack headUserType (c : tailUserType) nameFun argsFun = -- cut from tail, add to head
                    helperFunByName currentClass stack (c : headUserType) tailUserType nameFun argsFun

                helperFunByName (Class nameClass _ _ []) stack headUserType tailUserType nameFun argsFun = -- if nothing matches - error
                    return (KDError $ "Class " ++ headUserType ++ " in class " ++ nameClass ++ " was not found", KTUnknown, stack)

            _ -> return (KDError $ "Cannot override class " ++ show ktypeThis, KTUnknown, stack')

    interpretFunByName (Class _ _ ((Fun nameFun argsFun typeFun bodyFun) : otherFuns) _) stack name args
        | (nameFun == name) && (length argsFun == length args) = do
            (kdatas, ktypes, stack') <- interpretFunArgs stack args
            let (kdatas', ktypes') = checkArgsTypes argsFun kdatas ktypes where
                checkArgsTypes :: [Variable] -> [KData] -> [KType] -> ([KData],[KType])
                checkArgsTypes [] [] [] = ([], [])
                checkArgsTypes ((Variable {..}) : prevArgsFun) (kdata : prevKdatas) (ktype : prevKtypes) = case dataConversionFromTypeToType kdata ktype varType of
                    KDError m -> ([KDError m], [KTUnknown])
                    kdata' -> let (kdatas', ktypes') = checkArgsTypes prevArgsFun prevKdatas prevKtypes in
                        (kdata' : kdatas', varType : ktypes')
            case kdatas' of
                [KDError m] -> interpretFunByName (Class "" [] otherFuns []) stack' name args
                _ -> do
                    let argNames = varName <$> argsFun
                    let createVariable = \(kdata', ktype', varName) -> InterVar varName ktype' kdata' False Nothing
                    let argsToStackFormat = createVariable <$> (zip3 kdatas' ktypes' argNames)
                    (kdataResult, ktypeResult, stack'') <- interpretBlock (argsToStackFormat ++ [InterFun] ++ stack') bodyFun
                    return $ case dataConversionFromTypeToType kdataResult ktypeResult typeFun of
                        KDError m -> (KDError m, KTUnknown, stack'')
                        kdataResult' -> (kdataResult', typeFun, stack'')
        | otherwise = interpretFunByName (Class "" [] otherFuns []) stack name args
   -- interpretFunByName (Class _ _ _ (cl@(Class nameClass _ _ _) : cls)) stack name args --check if class constructor
   --     | (nameClass == name) && (length argsFun == length args) = do

    interpretFunByName (Class _ _ [] _) stack name args = return (KDError $ "Function " ++ name ++ " was not found", KTAny, stack)

    interpretFunArgs :: [InterObject] -> [Expr] -> IO ([KData], [KType], [InterObject])
    interpretFunArgs stack (arg:args) = do
        (kdata, ktype, stack') <- interpretExpression stack arg
        (kdatas, ktypes, stack'') <- interpretFunArgs stack' args
        return (kdata : kdatas, ktype : ktypes, stack'')
    interpretFunArgs stack [] = return ([], [], stack)

    interpretBlock :: [InterObject] -> [FunPrimitive] -> IO (KData, KType, [InterObject])
    interpretBlock stack = interBlock (InterBlock : stack) where
        deleteBlock (InterBlock : InterFun : objs) = objs
        deleteBlock (InterBlock : objs) = objs
        deleteBlock (_ : objs) = deleteBlock objs
        interBlock stack [fp] = do
            (kdata, ktype, stack') <- interpretFunPrimitive stack fp
            return (kdata, ktype, deleteBlock stack')
        interBlock stack (fp:fps) = do
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

    interpretExpression stack (CallFun ".init" [(Var className)]) = do
            let targetClass = findClassByName className (classes program) where
                (InterMainClass program) = last stack
                findClassByName :: String -> [Class] -> Maybe Class
                findClassByName targetName (curClass@(Class currentName _ _ _):cls)
                    | (targetName == currentName) = return curClass
                    | otherwise = findClassByName targetName cls
                findClassByName targetName [] = Nothing
            case targetClass of 
                Nothing -> return (KDError $ "No class found with name " ++ className, KTUnknown, [])    
                Just cl -> do
                        let newFields = translateVarListToEmptyRecord (fields cl) where
                            translateVarListToEmptyRecord :: [Variable] -> [(String, KData, KType, Bool)]
                            translateVarListToEmptyRecord ((Variable varMutable varName varType) : vs) = (varName, KDUndefined, varType, varMutable) : (translateVarListToEmptyRecord vs)
                            translateVarListToEmptyRecord [] = []       
                        let newClass = InterVar "this" (KTUserType className) (KDRecord newFields) True Nothing
                        let stack' = newClass : stack
                        let targetInit = findMethodByName "init" (methods cl) where
                            findMethodByName :: String -> [Fun] -> Maybe Fun
                            findMethodByName targetName (curFun@(Fun currentName _ _ _):fs)
                                | (targetName == currentName) = return curFun
                                | otherwise = findMethodByName targetName fs
                            findMethodByName targetName [] = Nothing
                        case targetInit of 
                            Nothing -> return (KDRecord newFields, KTUserType className, stack)   
                            Just init -> do 
                                    (kdata, ktype, stack'') <- interpretBlock stack' (body (init::Fun))
                                    let targetVar = findVarInStack "this" stack'' where
                                        findVarInStack :: String -> [InterObject] -> Maybe InterObject
                                        findVarInStack varName (v@(InterVar {..}) : objs)
                                            | (name == varName) = return v
                                            | otherwise = findVarInStack varName objs
                                        findVarInStack varName (obj: objs) = findVarInStack varName objs
                                        findVarInStack varname [] = Nothing
                                    case targetVar of
                                        Nothing -> return (KDError $ "Internal stack error", KTUnknown, [])   
                                        Just (InterVar _ newktype newkdata _ _) -> return (newkdata, newktype, stack)
                                
    interpretExpression stack (CallFun ".init" []) = return (KDError $ "Class init with no args", KTUnknown, [])
    interpretExpression stack (CallFun ".init" [_]) = return (KDError $ "Wrong class init parameter type", KTUnknown, [])
    interpretExpression stack (CallFun ".init" (p:ps)) = return (KDError $ "Class init too much args", KTUnknown, [])

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
                                        kdataNewVal
                                        ktypeNewVal
                            | (fieldNameOldVar == fieldName) && (fieldCanModifyOldVar == True || fieldKTypeOldVar == KTUnknown) = do
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

                        updateStack :: [InterObject] -> String -> KData -> KType -> IO (KData, KType, [InterObject])
                        updateStack (obj@(InterVar name ktype kdata canModify Nothing) : objs) varName varKData varKType
                            | name == varName && ktype /= KTUnknown && canModify == True = do
                                case dataConversionFromTypeToType varKData varKType ktype of
                                    KDError m -> return (KDError m, KTUnknown, obj : objs)
                                    kdataRes -> return (KDUndefined, KTUnknown, (InterVar name ktype kdataRes True Nothing) : objs)
                            | name == varName && ktype == KTUnknown = return (KDUndefined, KTUnknown, (InterVar name varKType varKData canModify Nothing) : objs)
                            | name == varName && ktype /= KTUnknown && canModify == False =
                                return (KDError $ "Cannot assign a new value to the val-variable " ++ varName, KTUnknown, obj : objs)
                            | otherwise = do
                                (kdataRes, ktypeRes, objs') <- updateStack objs varName varKData varKType
                                return (kdataRes, ktypeRes, obj : objs')
                        updateStack (obj : objs) varName varKData varKType = do
                            (kdataRes, ktypeRes, objs') <- updateStack objs varName varKData varKType
                            return (kdataRes, ktypeRes, obj : objs')
                        updateStack [] varName varKData varKType = return (KDError $ "Variable " ++ varName ++ " was not found", KTUnknown, [])
                        
                            
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
