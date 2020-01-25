{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Translator where
import Ast
import Control.Monad.Reader
import Control.Monad.State.Lazy
import System.IO
import Data.Tuple.Utils
import Data.List

type TableLocalVariables = [(String, KType, Int)] -- (name, type, index)

pushStr :: String -> StateT TableLocalVariables (State [String]) KType
pushStr newStr = lift $ modify (newStr : )

translatorExpression :: Expr -> StateT TableLocalVariables (State [String]) KType
translatorExpression = \case
    Val (KDByte val) -> do
        pushStr $ "bipush " ++ show val
        return KTByte
    Val (KDShort val) -> do
        pushStr $ "sipush " ++ show val
        return KTShort
    Val (KDInt val) -> do
        pushStr $ "ldc " ++ show val
        return KTInt
    Val (KDLong val) -> do
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
        case find ((== varName) . fst3) tableLocalVariables of
            Just var -> do
                let ktype = snd3 var
                pushStr $ case ktype of
                    KTByte -> "iload " ++ show (trd3 var)
                    KTShort -> "iload " ++ show (trd3 var)
                    KTInt -> "iload " ++ show (trd3 var)
                    KTLong -> "lload " ++ show (trd3 var)
                    KTDouble -> "dload " ++ show (trd3 var)
                    _ -> "aload " ++ show (trd3 var)
                return ktype
            Nothing -> undefined
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
                return KTLong
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
            (KTLong, KTLong) -> do
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

translateHelloWorld :: String -> IO()
translateHelloWorld name = do 
    helloWorldj <- readFile "test/hello_world.j"
    writeFile (name ++ ".j") helloWorldj
    return ()
-}
