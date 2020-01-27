{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Translator where
import Prelude hiding (getChar)
import Ast 
import Control.Monad.Reader
import Control.Monad.State.Lazy
import System.IO hiding (getChar)
import Data.Tuple.Select
import Data.List
import Data.Char
import Debug.Trace
import Control.Monad.Except

type LocalVariable = (String, KType, Int, Bool) -- (name, type, index, canModify)

type TableLocalVariables = [LocalVariable]

type SuperState = ExceptT String (StateT TableLocalVariables (StateT [String]  (Reader Class)))

genUnqID :: String -> Int -> String 
genUnqID prefix lineNum = prefix ++ show lineNum

pushStr :: String -> SuperState ()
pushStr newStr = lift $ lift $ modify ((newStr ++ "\n"): )

updateLocal :: String -> (LocalVariable -> LocalVariable) -> SuperState ()
updateLocal nameVar funUpdating = modify (helper) where
    helper [] = []
    helper (var@(name, _, _, _) : vars) | name == nameVar = (funUpdating var) : vars
    helper (var : vars) = var : (helper vars)

pushLocal :: (String, KType, Int, Bool) -> SuperState ()
pushLocal newL = modify (newL : )

deleteLocalByIndex :: Int -> SuperState ()
deleteLocalByIndex indexVar = modify (helper) where
    helper [] = []
    helper (var@(_, _, index, _) : vars) | index == indexVar = vars
    helper (var : vars) = var : (helper vars)

translatorBinOp :: Expr -> Expr -> String -> SuperState KType
translatorBinOp e1 e2 postfix = do
            ktype1 <- translatorExpression e1
            ktype2 <- translatorExpression e2
            case (ktype1, ktype2) of
                (KTInt, KTInt) -> do
                    pushStr $ "i" ++ postfix
                    return KTInt
                (KTLong, KTInt) -> do
                    pushStr "i2l"
                    pushStr $ "l" ++ postfix
                    return KTLong
                (KTInt, KTLong) -> do
                    lengthTableLocalVariables <- lift $ gets length
                    pushStr $ "lstore " ++ show lengthTableLocalVariables
                    pushStr "i2l"
                    pushStr $ "lload " ++ show lengthTableLocalVariables
                    pushStr $ "l" ++ postfix
                    return KTLong
                (KTLong, KTLong) -> do
                    pushStr $ "l" ++ postfix
                    return KTLong --if_icmpeq
                (KTDouble, KTDouble) -> do
                    pushStr $ "d" ++ postfix
                    return KTLong
                _ -> throwError ("ERROR") 

translatorExpression :: Expr -> SuperState KType
translatorExpression = 
    \case 
        Val (KDInt val) 
            | (abs val < 2^31) -> do
                pushStr $ "ldc " ++ show val
                return KTInt
            | otherwise -> do
                pushStr $ "ldc_w " ++ show val
                return KTLong   
        Val (KDArray str@((KDChar c) : cs)) -> do
            pushStr $ "ldc " ++ ['"'] ++ (getChar <$> str) ++ ['"']
            return $ KTArray KTChar
        Val (KDDouble val) -> do
            pushStr $ "ldc_w " ++ show val
            return KTDouble
        CallFun ".get" [Var varName] -> do
            tableLocalVariables <- get
            case find ((== varName) . sel1) tableLocalVariables of
                Just (name, ktype, index, canModify) -> do
                    pushStr $ case ktype of
                        KTInt -> "iload " ++ show index
                        KTLong -> "lload " ++ show index
                        KTDouble -> "dload " ++ show index
                        _ -> "aload " ++ show index
                    return ktype
                Nothing -> throwError $ "Variable " ++ varName ++ " not found "
        CallFun ".get" (variable : fields) -> do --TODO FIX!
            ktype <- case variable of 
                (Var varName) -> do        
                    tableLocalVariables <- get
                    case find ((== varName) . sel1) tableLocalVariables of
                        Just (name, ktype, index, canModify) -> do
                            case ktype of
                                KTUserType nameUserType -> pushStr $ "aload " ++ show index
                                KTArray ktypeArrayElem -> pushStr $ "aload " ++ show index
                                _ -> throwError $ "Unsupported type"
                            return ktype
                        Nothing -> throwError $ "Variable " ++ varName ++ " not found"
                _ -> translatorExpression variable        
            case fields of
                [] -> return ktype 
                _ -> do
                    translatorGetFields $ init fields
                    case last fields of
                        Var nameLastField -> do
                            pushStr $ "getfield " ++ nameLastField
                            return KTUnknown -- TODO
                        indexInArray -> do
                            translatorExpression indexInArray
                            let ktypeRes = getKTypeFromFields ktype fields
                            pushStr $ case ktypeRes of
                                KTInt -> "iaload"
                                KTLong -> "laload"
                                KTDouble -> "daload"
                                _ -> "aaload"
                            return ktypeRes  
        Add e1 e2 -> translatorBinOp e1 e2 "add"  
        Sub e1 e2 -> translatorBinOp e1 e2 "sub"
        Mul e1 e2 -> translatorBinOp e1 e2 "mul"
        Div e1 e2 -> translatorBinOp e1 e2 "div"
        Mod e1 e2 -> translatorBinOp e1 e2 "rem"
        CallFun ".set" (rvalue : (Var varName) : []) -> do --TODO: val a: Int = 3
            tableLocalVariables <- get
            case find ((== varName) . sel1) tableLocalVariables of
                Just (name, ktype, index, canModify)
                    | canModify == False && ktype == KTUnknown -> do
                        ktypeRes <- translatorExpression rvalue
                        pushStr $ case ktypeRes of
                            KTInt -> "istore " ++ show index
                            KTLong -> "lstore " ++ show index
                            KTDouble -> "dstore " ++ show index
                            _ -> "astore " ++ show index
                        updateLocal name (\(name', _, index', canModify') -> (name', ktypeRes, index', canModify'))
                        return KTUnit
                    | canModify == False -> throwError $ "Cannot modify " ++ name
                    | otherwise -> do 
                        ktypeRes <- translatorExpression rvalue
                        pushStr $ case ktype of
                            KTInt -> "istore " ++ show index
                            KTLong -> "lstore " ++ show index
                            KTDouble -> "dstore " ++ show index
                            _ -> "astore " ++ show index
                        updateLocal name (\(name', _, index', canModify') -> (name', ktypeRes, index', canModify'))
                        return KTUnit        
                _ -> throwError $ "Variable " ++ varName ++ " not found"
        CallFun ".set" (rvalue : (Var varName) : fields) -> do
            tableLocalVariables <- get
            case find ((== varName) . sel1) tableLocalVariables of
                Just (name, ktype, index, canModify) -> do
                        case ktype of
                            KTUserType nameUserType -> pushStr $ "aload " ++ show index
                            KTArray ktypeArrayElem -> pushStr $ "aload " ++ show index
                            _ -> throwError "Unsupported type"
                        translatorGetFields $ init fields
                        case last fields of
                            Var nameLastField -> do
                                ktypeRes <- translatorExpression rvalue
                                pushStr $ "putfield " ++ nameLastField
                            indexInArray -> do
                                translatorExpression indexInArray
                                ktypeRes <- translatorExpression rvalue
                                case ktypeRes of
                                    KTInt -> pushStr $ "iastore"
                                    KTLong -> pushStr $ "lastore"
                                    KTDouble -> pushStr $ "dastore"
                                    KTUserType _ -> pushStr $ "aastore"
                                    _ -> throwError $ "ktypeRes = " ++ show ktypeRes
                        return KTUnit
                _ -> throwError $ "Variable " ++ varName ++ " not found"
        If cond thenBranch elseBranch -> do
            translatorExpression cond
            lengthCurrentLines <- lift $ lift $ gets length
            pushStr $ "ifeq else" ++ show lengthCurrentLines
            ktype1 <- manyTranslatorFunPrimitive thenBranch
            pushStr $ "goto fi" ++ show lengthCurrentLines
            pushStr $ "else" ++ show lengthCurrentLines ++ ":"
            ktype2 <- manyTranslatorFunPrimitive elseBranch
            pushStr $ "fi" ++ show lengthCurrentLines ++ ":"
            --TODO fix types
            return ktype1
        Equal e1 e2 -> do
            lengthCurrentLines <- lift $ lift $ gets length
            ktype1 <- translatorExpression e1
            ktype2 <- translatorExpression e2
            pushStr $ case (ktype1, ktype2) of
                (KTInt, KTInt) -> "if_icmpne else" ++ show lengthCurrentLines
                _ -> "if_acmpne else" ++ show lengthCurrentLines
            pushStr $ "iconst_1"
            pushStr $ "goto fi" ++ show lengthCurrentLines
            pushStr $ "else" ++ show lengthCurrentLines ++ ":"
            pushStr $ "iconst_0"
            pushStr $ "fi" ++ show lengthCurrentLines ++ ":"
            return KTInt
        CallFun ".toBool" [arg] -> translatorExpression arg --TODO
        CallFun ".array" [Val (KDInt lengthArray), Lambda [Variable _ varName _] body] -> do
            lengthTableLocalVariables <- lift $ gets length
            pushLocal (varName, KTInt, lengthTableLocalVariables, True)
            ktype <- createNewArray lengthArray 0 lengthTableLocalVariables body
            deleteLocalByIndex lengthTableLocalVariables
            return ktype
        CallFun ('.' : nameFun) argsFun -> throwError nameFun -- TODO
        CallFun "println" [message] -> do
            pushStr "getstatic java/lang/System/out Ljava/io/PrintStream;"
            ktype <- translatorExpression message
            case ktype of 
                (KTArray KTChar) -> do 
                    pushStr "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
                    return KTUnit
                _ -> throwError "Wrong println argument"    
        CallFun nameFun args -> do
            translatorFunArgs args
            mainClass <- lift $ lift $ ask
            case find ((== nameFun) . (name :: Fun -> String)) (methods mainClass) of
                Just (Fun {..}) -> do --TODO get mainClassName
                    pushStr $ "invokestatic jsmClasses/Main/" ++ (toLower `map` nameFun) ++ "(" ++ translateArgs args ++ ")" ++ kTypeToBaseType returnType
                    return returnType
                Nothing -> throwError $ "Fun " ++ nameFun ++ " not found"
        _ -> throwError "Unsupported operator"        

createNewArray :: Integer -> Integer -> Int -> [FunPrimitive] -> SuperState KType
createNewArray length index indexArgLambda body
    | length == index = return KTUnknown
    | 0 == index = do
        pushStr $ "iconst_0"
        pushStr $ "istore " ++ show indexArgLambda
        ktype <- manyTranslatorFunPrimitive body
        case ktype of
            KTInt -> do
                pushStr $ "istore " ++ show indexArgLambda
                pushStr $ "ldc " ++ show length
                pushStr $ "newarray int"
                pushStr $ "dup"
                pushStr $ "iconst_0"
                pushStr $ "iload " ++ show indexArgLambda
                pushStr $ "iastore"
            _ -> undefined -- TODO
        createNewArray length (index + 1) indexArgLambda body
        return $ KTArray ktype
    | otherwise = do
        pushStr $ "dup"
        pushStr $ "ldc " ++ show index
        pushStr $ "dup"
        pushStr $ "istore " ++ show indexArgLambda
        ktype <- manyTranslatorFunPrimitive body
        case ktype of
            KTInt -> pushStr $ "iastore"
            _ -> undefined -- TODO
        createNewArray length (index + 1) indexArgLambda body

getKTypeFromFields :: KType -> [Expr] -> KType
getKTypeFromFields ktype [] = ktype
getKTypeFromFields (KTUserType nameUserType) ((Var nameField) : fields) = undefined -- TODO,
-- because here we cannot get type of field in user's type
getKTypeFromFields (KTArray ktypeArrayElem) (index : fields) = getKTypeFromFields ktypeArrayElem fields

translatorFunArgs :: [Expr] -> SuperState ()
translatorFunArgs [] = return ()
translatorFunArgs (arg : args) = do
    translatorExpression arg
    translatorFunArgs args

translatorGetFields :: [Expr] -> SuperState KType
translatorGetFields [] = return KTUnit
translatorGetFields ((Var nameField) : fields) = do
    pushStr $ "getfield " ++ nameField
    translatorGetFields fields
translatorGetFields (index : fields) = do
    translatorExpression index
    pushStr $ "aaload"
    translatorGetFields fields

        {-And e1 e2 -> do 
            currentLines <- lift get
            pushStr $ genUnqID "Label" (length currentLines)
            return KTUnknown-}
translatorFunPrimitive :: FunPrimitive -> SuperState KType
translatorFunPrimitive = 
    \case 
        ValInit name ktype -> do 
            lengthTableLocalVariables <- gets length
            pushLocal (name, ktype, lengthTableLocalVariables, False)
            return KTUnit
        VarInit name ktype -> do 
            lengthTableLocalVariables <- gets length
            pushLocal (name, ktype, lengthTableLocalVariables, True)
            return KTUnit    
        Expression expr -> do
            translatorExpression expr 
        _ -> throwError $ "Unsupported statement"

manyTranslatorFunPrimitive :: [FunPrimitive] -> SuperState KType
manyTranslatorFunPrimitive [] = return KTUnit
manyTranslatorFunPrimitive [last] = translatorFunPrimitive last
manyTranslatorFunPrimitive (f:fs) = do 
    ktype <- translatorFunPrimitive f
    manyTranslatorFunPrimitive fs 

_maxStack = 1024
_maxLocals = 1024    

kTypeToBaseType :: KType -> String
kTypeToBaseType = 
    \case 
        KTInt -> "I"
        KTDouble -> "D"
        KTLong -> "J"
        KTUnit -> "V"
        KTAny -> "Ljava/lang/Object;"
        KTArray KTChar ->"Ljava/lang/String;"
        KTArray t -> "[" ++ (kTypeToBaseType t)
        KTUserType t -> "L" ++ t ++ ";"
        KTUnknown -> "ERROR"

translateArgs :: [Variable] -> String
translateArgs [] = []
translateArgs ((Variable _ _ ktype):args) = kTypeToBaseType ktype ++ (translateArgs args)

localStoreArgs :: [Variable] -> SuperState ()
localStoreArgs [] = return ()
localStoreArgs ((Variable {..}):args) = do 
        lengthTableLocalVariables <- gets length
        pushLocal (varName, varType, lengthTableLocalVariables, varMutable)
        localStoreArgs args

translatorMethod :: Fun -> SuperState ()
translatorMethod (Fun {..})= do
    pushStr $ ".method public static " ++ (toLower `map` name) ++ "(" ++ translateArgs args ++ ")" ++ (kTypeToBaseType returnType)
    pushStr $ ".limit stack " ++ show _maxStack
    pushStr $ ".limit locals " ++ show _maxLocals
    localStoreArgs args
    ktype <- manyTranslatorFunPrimitive body
    case ktype of 
        KTInt -> pushStr "ireturn"
        KTLong -> pushStr "lreturn"
        KTDouble -> pushStr "dreturn"
        KTUnit -> pushStr "return"
        _ -> pushStr "areturn"
    put [] -- delete local args
    pushStr $ ".end method"

--TODO:Add fields, nested Classes
translatorClass :: String -> Class -> SuperState ()
translatorClass givenName (Class {..}) = do --another name, or it will be always Main, for testing purposes
    pushStr $ ".class public jsmClasses/" ++ givenName
    pushStr ".super java/lang/Object"
    let execAbleFuns = filter (\(Fun {..}) -> name /= "Any") methods
    helper execAbleFuns where
        helper :: [Fun] -> SuperState ()
        helper [] = return ()
        helper [f] = translatorMethod f 
        helper (f:fs) = do
            translatorMethod f
            helper fs    
{-
type ConstantPool = [Constant]

data JVMClass = JVMClass {
        constantPool :: ConstantPool,
        thisClass :: Int, -- index from constantPool
        superClass :: Int, -- index from constantPool
        flags :: [JVMFlag],
        interfaces :: [JVMInterface],
        fields :: [JVMField],
        methods :: [JVMMethod],
        attributes :: [JVMAttribute]
    }

data JVMMethod = JVMMethod {
        thisMethod :: Int, -- index from constantPool
        flags :: [JVMFlag],
        code :: [JVMInstruction]
    }

data JVMInterface --TODO

data JVMAttribute -- TODO

type JVMField = Int -- index from constantPool

data JVMFlag -- TODO

translatorProgram :: Class -> [JVMClass]
translatorProgram mainClass = runReader (translatorClass' mainClass) ""

translatorClass' :: Class -> Reader String [JVMClass] -- in Reader: parentClassName -> list of all inner classes of this class
translatorClass' thisClass@(Class {..}) = do
    fullClassName <- asks (\parentName -> if parentName == "" then name else parentName ++ "$" ++ name)
    let innerJVMClasses = concat $ (\cl -> runReader (translatorClass' cl) fullClassName) <$> classes
    let thisJVMClass = runReader (evalStateT (translatorClass thisClass) []) ""
    return $ thisJVMClass : innerJVMClasses

-} 
