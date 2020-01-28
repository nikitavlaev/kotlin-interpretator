{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Interpreter where
import Ast
import Text.Pretty.Simple (pPrint)
import Data.List
import Data.List.Split
import Debug.Trace
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Except

data Log a = Log String a

instance Show a => Show (Log a) where
    show (Log title datas) = "Log: " ++ title ++ " = " ++ show datas

data InterObject =
        InterFun
    | InterVar {name :: String, ktype :: KType, kdata :: KData, canModify :: Bool, connectedVariable :: Maybe Expr}
    | InterBlock
    | InterMainClass {cl :: Class}
    deriving Show

instance Eq InterObject where --wrong comparison, but enough to fulfill its purpose for now
    InterFun == InterFun = True
    (InterVar n1 t1 d1 f1 _) == (InterVar n2 t2 d2 f2 _) = (n1 == n2) && (t1 == t2) && (show d1 == show d2) && (f1 == f2)
    InterBlock == InterBlock = True
    InterMainClass _ == InterMainClass _ = True
    _ == _ = False

type InterState = ExceptT String (StateT [InterObject]  (ReaderT Class IO))

pushToStack :: InterObject -> InterState () 
pushToStack obj = lift $ modify (obj:)

pushListToStack :: [InterObject] -> InterState ()
pushListToStack [] = return ()
pushListToStack (obj:objs) = do
                pushListToStack objs
                pushToStack obj

dataConversionFromTypeToType :: KData -> KType -> KType -> KData
dataConversionFromTypeToType (KDError m) _ _ = KDError m
dataConversionFromTypeToType (KDInt x) _ KTByte = KDInt $ mod (x + 2 ^ 7) (2 ^ 8) - 2 ^ 7
dataConversionFromTypeToType (KDInt x) _ KTShort = KDInt $ mod (x + 2 ^ 15) (2 ^ 16) - 2 ^ 15
dataConversionFromTypeToType (KDInt x) _ KTInt = KDInt $ mod (x + 2 ^ 31) (2 ^ 32) - 2 ^ 31
dataConversionFromTypeToType (KDInt x) _ KTLong = KDInt $ mod (x + 2 ^ 63) (2 ^ 64) - 2 ^ 63
dataConversionFromTypeToType (KDInt x) _ KTDouble = KDDouble $ fromInteger x
dataConversionFromTypeToType kdata ktype1 (KTNullable (KTNullable ktype2)) = KDError "Cannot convert to double-nullable type"
dataConversionFromTypeToType KDNull (KTNullable _) (KTNullable _) = KDNull
dataConversionFromTypeToType kdata (KTNullable ktype1) (KTNullable ktype2) = dataConversionFromTypeToType kdata ktype1 ktype2
dataConversionFromTypeToType kdata ktype1 (KTNullable ktype2) = dataConversionFromTypeToType kdata ktype1 ktype2
dataConversionFromTypeToType (KDArray []) (KTArray ktype1) (KTArray ktype2) = KDArray []
dataConversionFromTypeToType (KDArray (kdata : kdatas)) (KTArray ktype1) (KTArray ktype2) = case dataConversionFromTypeToType kdata ktype1 ktype2 of
    KDError m -> KDError m
    kdata' -> case dataConversionFromTypeToType (KDArray kdatas) (KTArray ktype1) (KTArray ktype2) of
              KDError m -> KDError m
              (KDArray kdatas') -> KDArray $ kdata': kdatas'
              _ -> KDError "Internal error: function conversion KDArray from KTArray to KTArray can return only KDError or KDArray"
dataConversionFromTypeToType (KDInt 0) _ KTBool = KDBool False
dataConversionFromTypeToType (KDInt x) _ KTBool = KDBool True
dataConversionFromTypeToType r@(KDRecord _) (KTUserType typeName) other = r
dataConversionFromTypeToType r@(KDRecord _) other (KTUserType typeName) = r
dataConversionFromTypeToType kdata ktype1 ktype2 | ktype1 == ktype2 = kdata
dataConversionFromTypeToType kdata ktype1 KTAny = kdata
dataConversionFromTypeToType kdata KTAny ktype2 = kdata
dataConversionFromTypeToType kdata ktype1 KTUnit = KDUnit
dataConversionFromTypeToType kdata KTUnit ktype2 = KDUnit
dataConversionFromTypeToType kdata ktype1 ktype2 = KDError $ show kdata ++ ": cannot convert from type " ++ show ktype1 ++ " to type " ++ show ktype2

autoInferenceTypeFromData :: KData -> KType
autoInferenceTypeFromData = \case
    KDUnit -> KTUnit
    KDAny -> KTAny
    KDNull -> KTNullable KTAny
    (KDBool _) -> KTBool
    (KDChar _) -> KTChar
    (KDInt _) -> KTInt
    (KDDouble _) -> KTDouble
    (KDArray []) -> KTArray KTAny
    (KDArray (obj : objs)) -> KTArray $ autoInferenceTypeFromData obj
    KDUndefined -> KTUnknown
    (KDError _) -> KTUnknown
    (KDRecord _) -> KTUserType "Unknown user type"

setupProgramStack :: Class -> [InterObject]
setupProgramStack program = ((\(Variable {..}) -> InterVar varName varType KDUndefined varMutable Nothing) <$> fields program) ++ --add all fields as variables to stack
                            [(InterMainClass program)]
interpretProgram :: Class -> IO ()
interpretProgram program = do
        result <- runReaderT (evalStateT (runExceptT (interpretFunByName program "Main" [])) (setupProgramStack program)) program
        case result of 
            (Left m) -> do
                putStrLn m
                return ()
            (Right (KDUnit, KTUnit)) -> do     
                putStrLn $ "\n\nProgram ended successfully"
                return ()   
            (Right (kdata, ktype)) -> do    
                putStrLn $ "\n\nError: Function Main returned " ++ show kdata ++ " of type " ++ show ktype ++ ", but was expected unit"
                return ()

checkArgsTypes :: [Variable] -> [KData] -> [KType] -> Either String ([KData],[KType])
checkArgsTypes [] [] [] = Right ([], [])
checkArgsTypes ((Variable {..}) : prevArgs) (kdata : prevKdatas) (ktype : prevKtypes) = case (ktype == varType) of
    False -> Left "" 
    True -> case checkArgsTypes prevArgs prevKdatas prevKtypes of
                (Left m) -> (Left m)
                (Right (kdatas', ktypes')) -> (Right (kdata : kdatas', ktype : ktypes'))
checkArgsTypes _ _ _ = Left "Internal error in checking types: args length was checked before"

launchFun :: Fun -> [Expr] -> InterState (KData, KType)
launchFun (Fun {..}) arguments = do 
        (kdatas, ktypes) <- interpretFunArgs arguments
        case checkArgsTypes args kdatas ktypes of
            (Left m) -> throwError $ "Arguments type mismatched in function " ++ name
            (Right (kdatas',ktypes')) -> do
                let argNames = varName <$> args
                let argMutables = varMutable <$> args
                let createVariable = (\(kdata', ktype', varName, varMutable, arg) -> InterVar varName ktype' kdata' False (if varMutable then Just arg else Nothing))
                let argsToStackFormat = createVariable <$> (zip5 kdatas' ktypes' argNames argMutables arguments) where
                    zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
                    zip5 (a : as) (b : bs) (c : cs) (d : ds) (e : es) = (a, b, c, d, e) : zip5 as bs cs ds es
                    zip5 [] [] [] [] [] = []
                    zip5 _ _ _ _ _ = [] -- here lists are of equal length, so in case of such internal error we drop everything else
                --pPrint $ Log "Name started function" name
                --pPrint $ Log "Stack before working function" $ init stack'
                pushToStack InterFun
                pushListToStack argsToStackFormat
                (kdataResult, ktypeResult) <- interpretBlock body
                --pPrint $ Log ("kdataResult in function" ++ name) $ kdataResult
                --pPrint $ Log ("ktypeResult in function" ++ name) $ ktypeResult
                lift $ modify $ delete InterFun
                --pPrint $ Log "Name ended function" name
                --pPrint $ Log "Stack after working function" $ init stack'''
                case dataConversionFromTypeToType kdataResult ktypeResult returnType of
                    KDError m -> throwError $ "Typecheck fail in " ++ name ++ 
                                    " between " ++ show returnType ++ " and " ++ show ktypeResult ++ " " ++ "\n       " ++ m
                    kdataResult' -> return (kdataResult', ktypeResult)

interpretFunByName :: Class -> String -> [Expr] -> InterState (KData, KType)
interpretFunByName currentClass ('.' : name) (this : args) = do --ClassA.ClassB.f()
    (kdataThis, ktypeThis) <- interpretExpression this
    case ktypeThis of
        KTUserType nameUserType -> do
            let subTypes = splitOn "." nameUserType
            descendInClasses currentClass subTypes name (this : args) where
                descendInClasses :: Class -> [String] -> String -> [Expr] -> InterState (KData, KType)
                descendInClasses currentClass subTypes funName args = do
                    case subTypes of
                        [] -> throwError $ "Empty name of user type"
                        (className:cls) -> do
                            let targetClass = find (\(Class name _ _ _ ) -> name == className) (classes currentClass)
                            case targetClass of
                                Nothing -> throwError $  "No such class " ++ className
                                (Just cl) -> case cls of
                                    [] -> interpretFunByName cl funName args
                                    _  -> descendInClasses cl cls funName args         
        _ -> throwError $ "Cannot override class " ++ show ktypeThis

interpretFunByName currentClass@(Class clName clFlds (f@(Fun nameFun argsFun typeFun bodyFun) : otherFuns) clCls) name args
    | (nameFun == name) && (length argsFun == length args) = do
        stack <- lift $ get 
        res <- launchFun f args --TODO --if args types error then continue search
        lift $ put stack 
        return res 
    | otherwise = interpretFunByName (Class clName clFlds otherFuns clCls) name args

interpretFunByName (Class _ _ [] _) name (arg:args) = throwError $ "Function " ++ name ++ " was not found, arg :: " ++ show arg
interpretFunByName (Class _ _ [] _) name _ = throwError $ "Function " ++ name ++ " was not found"

interpretFunArgs :: [Expr] -> InterState ([KData], [KType])
interpretFunArgs (arg:args) = do
    (kdata, ktype) <- interpretExpression arg
    (kdatas, ktypes) <- interpretFunArgs args
    return (kdata : kdatas, ktype : ktypes)
interpretFunArgs [] = return ([], [])

interpretBlock :: [FunPrimitive] -> InterState (KData, KType)
interpretBlock fs = do
    pushToStack InterBlock
    interBlock fs where
        interBlock [fp] = do
        -- pPrint $ Log "Stack" $ init stack
        -- pPrint $ Log "Action" fp
            (kdata, ktype) <- interpretFunPrimitive fp
        -- pPrint $ Log "Stack" $ init stack'
            lift $ modify $ delete InterBlock
            return (kdata, ktype)
        interBlock (fp:fps) = do
        -- pPrint $ Log "Stack" $ init stack
        -- pPrint $ Log "Action" fp
            (kdata, ktype) <- interpretFunPrimitive fp
            interBlock fps
        interBlock [] = do
            lift $ modify $ delete InterBlock
            return (KDUnit, KTUnit)

interpretFunPrimitive :: FunPrimitive -> InterState (KData, KType)
interpretFunPrimitive (Expression expr) = interpretExpression expr
interpretFunPrimitive (ValInit {..}) = do
    pushToStack $ InterVar {name = name, ktype = ktype, kdata = KDUndefined, canModify = False, connectedVariable = Nothing}
    return (KDUndefined, KTUnknown)
interpretFunPrimitive (VarInit {..}) =do 
    pushToStack $ InterVar {name = name, ktype = ktype, kdata = KDUndefined, canModify = True, connectedVariable = Nothing}
    return (KDUndefined, KTUnknown)
interpretFunPrimitive (While {..}) = do
    (kdataCond, ktypeCond) <- interpretExpression cond
    case kdataCond of
        KDBool True -> do
            (kdataBody, ktypeBody) <- interpretBlock body
            interpretFunPrimitive (While cond body)
        KDBool False -> return (KDUndefined, KTUnknown)
        _ -> throwError "Invalid condition in While" 
interpretFunPrimitive _ = throwError $ "This instruction is unsupported for now" 

createArrayByLambda :: String -> Integer -> [FunPrimitive] -> InterState (KData, KType)
createArrayByLambda indexName size body = do
    pushToStack $ InterVar indexName KTInt (KDInt 0) False Nothing
    (kdata, ktypeElement) <- interpretBlock body
    let mOtherResult = foldr f (return (KDArray [], KTArray KTAny)) [1..(size-1)] where 
                            f :: Integer -> InterState (KData, KType) -> InterState (KData, KType)
                            f i mans = do 
                                (kdata, ktype) <- launchFun (Fun "Element" [Variable False indexName KTInt] ktypeElement body) [Val (KDInt i)]
                                ans <- mans
                                case ans of 
                                    (KDArray kdatas, KTArray ktypeOther) -> return (KDArray $ kdata : kdatas, KTArray ktypeElement)
                                    _ -> throwError $ "Internal error in creating array by lambda: created smth else but not an array"      
    otherResult <- mOtherResult                                                         
    case otherResult of
        (KDArray kdatas, KTArray ktypeOther) -> return (KDArray $ kdata : kdatas, KTArray ktypeElement)
        _ -> throwError $ "Internal error in creating array by lambda: created smth else but not an array"                                       
            
checkIfInteger :: KType -> Bool 
checkIfInteger t =  t == KTByte || t == KTShort || t == KTInt || t == KTLong

findStackVarByName :: String -> InterState (Maybe InterObject)
findStackVarByName vName = lift $ gets $ find (checkVar vName) where 
                    checkVar vName (InterVar {..}) = name == vName
                    checkVar _ obj = False 

interpretExpression:: Expr -> InterState (KData, KType)
interpretExpression (Val kdata) = return (kdata, autoInferenceTypeFromData kdata)
interpretExpression (CallFun {name = "print", args = [exprMessage]}) = do
    (kdata, ktype) <- interpretExpression exprMessage
    lift $ lift $ lift $ putStr $ show kdata
    return (KDUnit, KTUnit)
interpretExpression (CallFun {name = "println", args = [exprMessage]}) = do
    (kdata, ktype) <- interpretExpression exprMessage
    lift $ lift $ lift $ putStrLn $ show kdata
    return (KDUnit, KTUnit)
interpretExpression (CallFun {name = "readLine", args = []}) = do
    inputStr <- lift $ lift $ lift $ getLine
    return (KDArray $ KDChar <$> inputStr, KTNullable $ KTArray KTChar)

interpretExpression (CallFun "Object" []) = return (KDAny, KTAny)

interpretExpression (CallFun ".array" [Val (KDInt size), (Lambda ((Variable {..}):[]) body) ]) = createArrayByLambda varName size body

interpretExpression (CallFun ".array" _ ) = return (KDError "Illegal initial array arguments", KTUnknown)

interpretExpression (CallFun ".get" [Var "true"]) = return (KDBool True, KTBool)
interpretExpression (CallFun ".get" [Var "false"]) = return (KDBool False, KTBool)
interpretExpression (CallFun ".get" [Var "null"]) = return (KDNull, KTNullable KTAny)
interpretExpression (CallFun ".get" [Var "unit"]) = return (KDUnit, KTUnit)
interpretExpression (CallFun ".get" (variable : fields)) = do
    (kdataVar, ktypeVar) <- case variable of
        (Var vName) -> do
            result <- findStackVarByName vName
            case result of
                Just (InterVar {..}) -> return (kdata,ktype)
                Just _ -> throwError "Wrong typed variable found"
                Nothing -> throwError $ "Variable " ++ vName ++ " was not found"   
        _ -> interpretExpression variable
    getFields kdataVar ktypeVar fields where
        getFields kdataVar ktypeVar [] = return (kdataVar, ktypeVar)
        getFields (KDRecord ((nameFieldVar, kdataFieldVar, ktypeFieldVar, canModifyFieldVar) : fieldsVar)) ktypeVar@(KTUserType _) ((Var fieldName) : fields)
            | nameFieldVar == fieldName = getFields kdataFieldVar ktypeFieldVar fields
            | True = getFields (KDRecord fieldsVar) ktypeVar ((Var fieldName) : fields)
        getFields (KDArray kdatas) (KTArray ktype) (field : fields) = do
            (kdataIndex, ktypeIndex) <- interpretExpression field
            case kdataIndex of
                KDInt index 
                    | (checkIfInteger ktypeIndex == False) -> throwError $ "Index of array cannot be of type " ++ show ktypeIndex
                    | (index < 0 || fromInteger index >= length kdatas) -> throwError $ "Index " ++ show index ++ " outside the bounds of the array"
                    | otherwise -> getFields (kdatas !! fromInteger index) ktype fields
                _ -> throwError $ show kdataIndex ++ " is not index of array"
        getFields _ ktypeVar ((Var fieldName) : fields) = throwError $ "The variable of type " ++ show ktypeVar ++ " does not have the field " ++ fieldName
        getFields _ ktypeVar (field : fields) = throwError $ "Cannot take an index from the variable of type " ++ show ktypeVar

interpretExpression (CallFun ".set" (exprNewVal : (Var varName) : fields)) = do
    (kdataNewVal, ktypeNewVal) <- interpretExpression exprNewVal
    foundVar <- findStackVarByName varName
    case foundVar of
        Nothing -> throwError $ "Variable " ++ varName ++ " was not found"
        Just (InterVar {..}) -> do
                (varKData, varKType) <- helperSet kdata ktype fields kdataNewVal ktypeNewVal
                updateStack varName varKData varKType where
                    helperSet :: KData -> KType -> [Expr] -> KData -> KType -> InterState (KData, KType)
                    helperSet kdataOldVar KTUnknown [] kdataNewVal ktypeNewVal = return (kdataNewVal, ktypeNewVal)
                    helperSet kdataOldVar ktypeOldVar [] kdataNewVal ktypeNewVal =
                        case dataConversionFromTypeToType kdataNewVal ktypeNewVal ktypeOldVar of
                            KDError m -> throwError m
                            kdataRes -> return (kdataRes, ktypeOldVar)
                    helperSet (KDRecord ((fieldNameOldVar, fieldKDataOldVar, fieldKTypeOldVar, fieldCanModifyOldVar) : fieldsOldVar))
                            (KTUserType nameUserType)
                            ((Var fieldName) : fields)
                            kdataNewVals
                            ktypeNewVal
                        | (fieldNameOldVar == fieldName) && (fieldCanModifyOldVar == True || fieldKTypeOldVar == KTUnknown || ((length fieldsOldVar) > 0)) = do
                            (kdataRes, ktypeRes) <- helperSet fieldKDataOldVar fieldKTypeOldVar fields kdataNewVal ktypeNewVal
                            return (KDRecord ((fieldNameOldVar, kdataRes, ktypeRes, fieldCanModifyOldVar) : fieldsOldVar), KTUserType nameUserType)
                        | (fieldNameOldVar == fieldName) = throwError $ "Cannot assign a new value to the val-field " ++ fieldName
                        | otherwise = do
                            (kdataRes, ktypeRes) <- helperSet (KDRecord fieldsOldVar) (KTUserType nameUserType) ((Var fieldName) : fields) kdataNewVal ktypeNewVal
                            case kdataRes of
                                KDRecord fieldsNewVar -> return (KDRecord ((fieldNameOldVar, fieldKDataOldVar, fieldKTypeOldVar, fieldCanModifyOldVar) : fieldsNewVar), KTUserType nameUserType)
                                _ -> throwError $ "Internal error in changing class field: sudden type change"
                    helperSet (KDRecord []) (KTUserType nameUserType) ((Var fieldName) : fields) kdataNewVal ktypeNewVal =
                        throwError $ "Field " ++ fieldName ++ " in type " ++ nameUserType ++ " was not found"
                    helperSet _ (KTUserType _) ((Var fieldName) : fields) kdataNewVal ktypeNewVal =
                        throwError $ "Data and type mismatch"
                    helperSet _ ktypeOldVar ((Var fieldName) : fields) kdataNewVal ktypeNewVal =
                        throwError $ "Type " ++ show ktypeOldVar ++ " has no fields"
                    helperSet (KDArray kdatasKernelOldVar) (KTArray ktypeKernelOldVar) (exprIndex : fields) kdataNewVal ktypeNewVal = do
                        (kdataIndex, ktypeIndex) <- interpretExpression exprIndex
                        case kdataIndex of
                            KDInt index 
                                | (checkIfInteger ktypeIndex == False) -> throwError $ "Index of array cannot be the value of type " ++ show ktypeIndex
                                | (index < 0 || fromInteger index >= length kdatasKernelOldVar) -> throwError $ "Index " ++ show index ++ " outside the bounds of the array"
                                | otherwise -> do
                                        (kdataNewVar, ktypeNewVar) <- helperSet (kdatasKernelOldVar !! fromInteger index) ktypeKernelOldVar fields kdataNewVal ktypeNewVal
                                        if ktypeNewVar /= ktypeKernelOldVar then throwError $ "Cannot change array element type from " ++ show ktypeKernelOldVar ++ " to " ++ show ktypeNewVar
                                        else return (KDArray (take (fromInteger index) kdatasKernelOldVar ++ [kdataNewVar] ++ drop (fromInteger index + 1) kdatasKernelOldVar), KTArray ktypeKernelOldVar)
                            _ -> throwError $ "Data " ++ show kdataIndex ++ " cannot be an index of array"
                    helperSet _ (KTArray _) (exprIndex : fields) kdataNewVal ktypeNewVal =
                        throwError $ "Data and type mismatch"
                    helperSet _ ktypeOldVar (exprIndex : fields) kdataNewVal ktypeNewVal =
                        throwError $ "Type " ++ show ktypeOldVar ++ " has no indexes"

                    splitStackByObject :: InterObject -> [InterObject] -> ([InterObject], [InterObject])
                    splitStackByObject pivot (obj : objs) 
                        | pivot == obj = ([], objs)
                        | otherwise = do 
                            let (revStackHead, stackTail) = splitStackByObject pivot objs
                            (obj:revStackHead, stackTail)
                    splitStackByObject pivot [] = ([],[])   

                    updateStack :: String -> KData -> KType -> InterState (KData, KType)
                    updateStack varName varKData varKType = do
                        foundVar <- findStackVarByName varName
                        case foundVar of
                            (Just (v@InterVar {..})) -> do
                                (revStackHead, stackTail) <- lift $ gets $ splitStackByObject v
                                let stackHead = reverse revStackHead
                                connected <- case connectedVariable of
                                    Nothing -> do 
                                        lift $ put stackTail 
                                        return (KDUndefined, KTUnknown)
                                    Just (CallFun ".get" ((Var varName') : fields')) -> do 
                                        lift $ put stackTail 
                                        let (revStackHead', stackTail') = splitStackByObject InterFun stackTail
                                        lift $ put stackTail'
                                        (kdataSet, ktypeSet) <- interpretExpression (CallFun ".set" (exprNewVal: (Var varName') : (fields' ++ fields)))
                                        case kdataSet of
                                            KDUndefined -> return ()
                                            _ -> throwError $ "Set failed " ++ show kdataSet
                                        newStackTail' <- lift $ get
                                        lift $ put (reverse revStackHead' ++ (InterFun):newStackTail')
                                        return (KDUndefined, KTUnknown)
                                    _ ->  throwError $ "Internal error: Invalid connected variable type"
                                newStackTail <- lift $ get      
                                case ktype of
                                    KTUnknown -> do 
                                        lift $ put (stackHead ++ (InterVar name varKType varKData canModify connectedVariable) : newStackTail)
                                        return (KDUndefined, KTUnknown) 
                                    _ -> case dataConversionFromTypeToType varKData varKType ktype of
                                            KDError m -> throwError $ "Set type check failed " ++ m
                                            kdataRes -> do
                                                lift $ put (stackHead ++ (InterVar name ktype kdataRes canModify connectedVariable) : newStackTail)
                                                return (KDUndefined, KTUnknown)
                            _ -> throwError $ "Variable " ++ varName ++ " was not found"
        Just _ -> throwError "Wrong typed variable found"                    
       
                    
interpretExpression (CallFun {name = ".toBool", args = [expr]}) = do
    (kdata, ktype) <- interpretExpression expr
    let res = dataConversionFromTypeToType kdata ktype KTBool
    case res of 
        KDError _ -> throwError ".toBool failed"
        _ -> return (res, KTBool)

interpretExpression (CallFun ".unnullify" [expr]) = do
    (kdata, ktype) <- interpretExpression expr
    case (kdata, ktype) of
        (KDError _, _) -> return (kdata, ktype)
        (KDNull, KTNullable _) -> return (KDError "Still null", KTUnknown)
        (_, KTNullable ktypeKernel) -> return (kdata, ktypeKernel)
        (_, _) -> return (kdata, ktype)
interpretExpression (CallFun {name = name, args = fargs}) = do
    program <- lift $ lift $ ask
    interpretFunByName program name fargs
                                                                    
interpretExpression (Add e1 e2) = do
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interAdd kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot be added " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2
        _ -> return (kdataRes, ktypeRes)
        where
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
            KDError m -> (KDError "", KTUnknown)
            kdata' ->
                case interAdd (KDArray xs) (KTArray ktype1) (KDArray ys) (KTArray ktype2) of
                    (KDArray kdataRes, KTArray ktypeRes) -> (KDArray (kdata' : kdataRes), KTArray ktypeRes)
                    _ -> (KDError "", KTUnknown)
        interAdd kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown)
interpretExpression (Sub e1 e2) = do
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interSub kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot be subtracted " ++ show kdata2 ++ " of type " ++ show ktype2 ++ " from " ++ show kdata1 ++ " of type " ++ show ktype1
        _ -> return (kdataRes, ktypeRes)
        where
        interSub (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x - y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
        interSub (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) - y), KTDouble)
        interSub (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x - (fromInteger y)), KTDouble)
        interSub (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x - y), KTDouble)
        interSub kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown)
interpretExpression (Mul e1 e2) = do
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interMul kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot be multiplied " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " by " ++ show kdata2 ++ " of type " ++ show ktype2
        _ -> return (kdataRes, ktypeRes)
        where
        interMul (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x * y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
        interMul (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) * y), KTDouble)
        interMul (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x * (fromInteger y)), KTDouble)
        interMul (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x * y), KTDouble)
        interMul kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown)
interpretExpression (Div e1 e2) = do        
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interDiv kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot be divided " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " by " ++ show kdata2 ++ " of type " ++ show ktype2
        _ -> return (kdataRes, ktypeRes)
        where
        interDiv (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x `div` y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
        interDiv (KDInt x) _ (KDDouble y) KTDouble = (KDDouble ((fromInteger x) / y), KTDouble)
        interDiv (KDDouble x) KTDouble (KDInt y) _ = (KDDouble (x / (fromInteger y)), KTDouble)
        interDiv (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDDouble (x / y), KTDouble)
        interDiv kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown)
interpretExpression (Mod e1 e2) = do
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interMod kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot take a module of " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " when dividing by " ++ show kdata2 ++ " of type " ++ show ktype2
        _ -> return (kdataRes, ktypeRes)
        where
        interMod (KDInt x) ktype1 (KDInt y) ktype2 = (dataConversionFromTypeToType (KDInt (x `mod` y)) KTInt (max ktype1 ktype2), max ktype1 ktype2)
        interMod kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown)
interpretExpression (And e1 e2) = do
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interAnd kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot take the logical and between " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2
        _ -> return (kdataRes, ktypeRes)
        where
        interAnd (KDBool True) KTBool (KDBool True) KTBool = (KDBool True, KTBool)
        interAnd (KDBool _) KTBool (KDBool _) KTBool = (KDBool False, KTBool)
        interAnd kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown)
interpretExpression (Or e1 e2) = do
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interOr kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot take the logical or between " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2
        _ -> return (kdataRes, ktypeRes)
        where
        interOr (KDBool False) KTBool (KDBool False) KTBool = (KDBool False, KTBool)
        interOr (KDBool _) KTBool (KDBool _) KTBool = (KDBool True, KTBool)
        interOr kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown) 
interpretExpression (Not e) = do
    (kdata, ktype) <- interpretExpression e
    let (kdataRes, ktypeRes) = interNot kdata ktype
    case kdataRes of
        (KDError _) -> throwError $ "Cannot take the logical not for" ++ show kdata ++ " of type " ++ show ktype
        _ -> return (kdataRes, ktypeRes)
        where
        interNot (KDBool x) KTBool = (KDBool (not x), KTBool)
        interNot kdata ktype = (KDError "", KTUnknown) 
interpretExpression (Less e1 e2) = do
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interLess kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot be compared " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2
        _ -> return (kdataRes, ktypeRes)
        where
        interLess (KDInt x) ktype1 (KDInt y) ktype2 = (KDBool (x < y), KTBool)
        interLess (KDInt x) _ (KDDouble y) KTDouble = (KDBool ((fromInteger x) < y), KTBool)
        interLess (KDDouble x) KTDouble (KDInt y) _ = (KDBool (x < (fromInteger y)), KTBool)
        interLess (KDDouble x) KTDouble (KDDouble y) KTDouble = (KDBool (x < y), KTBool)
        interLess kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown) 
interpretExpression (Equal e1 e2) = do
    (kdata1, ktype1) <- interpretExpression e1
    (kdata2, ktype2) <- interpretExpression e2
    let (kdataRes, ktypeRes) = interEqual kdata1 ktype1 kdata2 ktype2
    case kdataRes of
        (KDError _) -> throwError $ "Cannot be compared " ++ show kdata1 ++ " of type " ++ show ktype1 ++ " and " ++ show kdata2 ++ " of type " ++ show ktype2
        _ -> return (kdataRes, ktypeRes)
        where
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
                _ -> (KDError "", KTUnknown) 
        interEqual kdata1 ktype1 kdata2 ktype2 = (KDError "", KTUnknown) 
interpretExpression (If cond thenBranch elseBranch) = do
    (kdata, ktype) <- interpretExpression cond
    case kdata of
        KDError m -> return (kdata, ktype)
        KDBool True -> interpretBlock thenBranch
        KDBool False -> interpretBlock elseBranch
        _ -> throwError "Invalid condition in If"
interpretExpression (Lambda _ _) = throwError "Lambda expression is unsupported for now" 
interpretExpression (Var _) = throwError "Var expression is unsupported for now" --this is not VarInit, needed for args  
