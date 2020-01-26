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

type LocalVariable = (String, KType, Int, Bool) -- (name, type, index, canModify)

type TableLocalVariables = [LocalVariable]

genUnqID :: String -> Int -> String 
genUnqID prefix lineNum = prefix ++ show lineNum

pushStr :: String -> StateT TableLocalVariables (State [String]) ()
pushStr newStr = lift $ modify (newStr : )

updateVariable :: String -> (LocalVariable -> LocalVariable) -> StateT TableLocalVariables (State [String]) ()
updateVariable nameVar funUpdating = modify (helper) where
    helper [] = []
    helper (var@(name, _, _, _) : vars) | name == nameVar = (funUpdating var) : vars
    helper (var : vars) = var : (helper vars)

pushLocal :: (String, KType, Int, Bool) -> StateT TableLocalVariables (State [String]) ()
pushLocal newL = modify (newL : )

translatorExpression :: Expr -> StateT TableLocalVariables (State [String]) KType
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
            pushStr $ "ldc " ++ (getChar <$> str)
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
                Nothing -> do 
                    pushStr "ERROR"
                    return KTUnknown
        CallFun ".get" ((Var varName) : fields) -> do
            tableLocalVariables <- get
            case find ((== varName) . sel1) tableLocalVariables of
                Just (name, ktype, index, canModify) -> do
                    pushStr $ case ktype of
                        KTUserType nameUserType -> "aload " ++ show index
                        KTArray ktypeArrayElem -> "aload " ++ show index
                        _ -> "ERROR"
                    translatorGetFields $ init fields
                    case last fields of
                        Var nameLastField ->
                            pushStr $ "getfield " ++ nameLastField
                        indexInArray -> do
                            translatorExpression indexInArray
                            pushStr $ case getKTypeFromFields ktype fields of
                                KTInt -> "iaload"
                                KTLong -> "laload"
                                KTDouble -> "daload"
                                _ -> "aaload"
                    return ktype
                Nothing -> do 
                    pushStr "ERROR"
                    return KTUnknown
        Add e1 e2 -> do
            ktype1 <- translatorExpression e1
            ktype2 <- translatorExpression e2
            case (ktype1, ktype2) of
                (KTInt, KTInt) -> do
                    pushStr "iadd"
                    return KTInt
                (KTLong, KTInt) -> do
                    pushStr "i2l"
                    pushStr "ladd"
                    return KTLong
                (KTInt, KTLong) -> do
                    lengthTableLocalVariables <- gets length
                    pushStr $ "lstore " ++ show lengthTableLocalVariables
                    pushStr "i2l"
                    pushStr $ "lload " ++ show lengthTableLocalVariables
                    pushStr "ladd"
                    return KTLong
                (KTLong, KTLong) -> do
                    pushStr "ladd"
                    return KTLong --if_icmpeq
                (KTDouble, KTDouble) -> do
                    pushStr "dadd"
                    return KTLong
        Sub e1 e2 -> do
            ktype1 <- translatorExpression e1
            ktype2 <- translatorExpression e2
            case (ktype1, ktype2) of
                (KTInt, KTInt) -> do
                    pushStr "isub"
                    return KTInt
                (KTLong, KTInt) -> do
                    pushStr "i2l"
                    pushStr "lsub"
                    return KTLong
                (KTInt, KTLong) -> do
                    lengthTableLocalVariables <- gets length
                    pushStr $ "lstore " ++ show lengthTableLocalVariables
                    pushStr "i2l"
                    pushStr $ "lload " ++ show lengthTableLocalVariables
                    pushStr "lsub"
                    return KTLong
                (KTLong, KTLong) -> do
                    pushStr "lsub"
                    return KTLong
                (KTDouble, KTDouble) -> do
                    pushStr "dsub"
                    return KTLong
        Mul e1 e2 -> do
            ktype1 <- translatorExpression e1
            ktype2 <- translatorExpression e2
            case (ktype1, ktype2) of
                (KTInt, KTInt) -> do
                    pushStr "imul"
                    return KTInt
                (KTLong, KTInt) -> do
                    pushStr "i2l"
                    pushStr "lmul"
                    return KTLong
                (KTInt, KTLong) -> do
                    lengthTableLocalVariables <- gets length
                    pushStr $ "lstore " ++ show lengthTableLocalVariables
                    pushStr "i2l"
                    pushStr $ "lload " ++ show lengthTableLocalVariables
                    pushStr "lmul"
                    return KTLong 
                (KTDouble, KTDouble) -> do
                    pushStr "dmul"
                    return KTLong
        Div e1 e2 -> do
            ktype1 <- translatorExpression e1
            ktype2 <- translatorExpression e2
            case (ktype1, ktype2) of
                (KTInt, KTInt) -> do
                    pushStr "idiv"
                    return KTInt
                (KTLong, KTInt) -> do
                    pushStr "i2l"
                    pushStr "ldiv"
                    return KTLong
                (KTInt, KTLong) -> do
                    lengthTableLocalVariables <- gets length
                    pushStr $ "lstore " ++ show lengthTableLocalVariables
                    pushStr "i2l"
                    pushStr $ "lload " ++ show lengthTableLocalVariables
                    pushStr "ldiv"
                    return KTLong
                (KTLong, KTLong) -> do
                    pushStr "ldiv"
                    return KTLong
                (KTDouble, KTDouble) -> do
                    pushStr "ddiv"
                    return KTLong
        Mod e1 e2 -> do
            ktype1 <- translatorExpression e1
            ktype2 <- translatorExpression e2
            case (ktype1, ktype2) of
                (KTInt, KTInt) -> do
                    pushStr "irem"
                    return KTInt
                (KTLong, KTInt) -> do
                    pushStr "i2l"
                    pushStr "lrem"
                    return KTLong
                (KTInt, KTLong) -> do
                    lengthTableLocalVariables <- gets length
                    pushStr $ "lstore " ++ show lengthTableLocalVariables
                    pushStr "i2l"
                    pushStr $ "lload " ++ show lengthTableLocalVariables
                    pushStr "lrem"
                    return KTLong
                (KTLong, KTLong) -> do
                    pushStr "lrem"
                    return KTLong
                (KTDouble, KTDouble) -> do
                    pushStr "drem"
                    return KTLong
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
                        updateVariable name (\(name', _, index', canModify') -> (name', ktypeRes, index', canModify'))
                        return KTUnit
                    | canModify == False ->  do
                        pushStr $ "ERROR: variable " ++ varName ++ " cannot be modified"
                        return KTUnit
                    | otherwise -> do 
                        ktypeRes <- translatorExpression rvalue
                        pushStr $ case ktype of
                            KTInt -> "istore " ++ show index
                            KTLong -> "lstore " ++ show index
                            KTDouble -> "dstore " ++ show index
                            _ -> "astore " ++ show index
                        updateVariable name (\(name', _, index', canModify') -> (name', ktype, index', canModify'))
                        return KTUnit        
                _ -> do 
                    pushStr $ "ERROR: variable " ++ varName ++ " was not found"
                    return KTUnknown
        CallFun ".set" (rvalue : (Var varName) : fields) -> do
            tableLocalVariables <- get
            case find ((== varName) . sel1) tableLocalVariables of
                Just (name, ktype, index, canModify)
                    | canModify == True || ktype == KTUnknown -> do
                        case ktype of
                            KTUserType nameUserType -> pushStr $ "aload " ++ show index
                            KTArray ktypeArrayElem -> pushStr $ "aload " ++ show index
                            _ -> pushStr "ERROR"
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
                        return KTUnit
                _ -> do 
                    pushStr "ERROR"
                    return KTUnknown

getKTypeFromFields :: KType -> [Expr] -> KType
getKTypeFromFields ktype [] = ktype
getKTypeFromFields (KTUserType nameUserType) ((Var nameField) : fields) = undefined -- TODO,
-- because here we cannot get type of field in user's type
getKTypeFromFields (KTArray ktypeArrayElem) (index : fields) = getKTypeFromFields ktypeArrayElem fields

translatorGetFields :: [Expr] -> StateT TableLocalVariables (State [String]) KType
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
translatorFunPrimitive :: FunPrimitive -> StateT TableLocalVariables (State [String]) KType
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
        _ -> do
            pushStr "ERROR"
            return KTUnit

manyTranslatorFunPrimitive :: [FunPrimitive] -> StateT TableLocalVariables (State [String]) KType
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
        KTAny -> "Ljava/lang/Object"
        KTArray KTChar ->"Ljava/lang/String"
        KTArray t -> "[" ++ (kTypeToBaseType t)
        KTUserType t -> "L" ++ t
        KTUnknown -> "ERROR"

translateArgs :: [Variable] -> String
translateArgs [] = []
translateArgs ((Variable _ _ ktype):args) = kTypeToBaseType ktype ++ ";" ++ (translateArgs args) 

translatorMethod :: Fun -> StateT TableLocalVariables (State [String]) ()
translatorMethod (Fun {..})= do
    pushStr $ ".method public static " ++ (toLower `map` name) ++ "(" ++ translateArgs args ++ ")" ++ (kTypeToBaseType returnType)
    pushStr $ ".limit stack " ++ show _maxStack
    pushStr $ ".limit locals " ++ show _maxLocals
    ktype <- manyTranslatorFunPrimitive body
    case ktype of 
        KTInt -> pushStr "ireturn"
        KTLong -> pushStr "lreturn"
        KTDouble -> pushStr "dreturn"
        KTUnit -> pushStr "return"
        _ -> pushStr "areturn"
    pushStr $ ".end method"  

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

translatorClass :: Class -> StateT ConstantPool (Reader String) JVMClass
translatorClass (Class {..}) = do
    pool <- get
    return JVMClass {
            constantPool = pool,
            thisClass = undefined,
            superClass = undefined,
            flags = [],
            interfaces = [],
            fields = undefined,
            methods = undefined,
            attributes = undefined
        }

-} 
translateHelloWorld :: String -> IO()
translateHelloWorld name = do 
    helloWorldj <- readFile "test/hello_world.j"
    writeFile (name ++ ".j") helloWorldj
    return ()

runTranslateExpression :: Expr -> TableLocalVariables -> [String]
runTranslateExpression expr table = reverse $ execState (evalStateT (translatorExpression expr) table) []

