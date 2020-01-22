{-# LANGUAGE DuplicateRecordFields #-}
module Translator where
type Byte = Int -- single byte number
type Short = Int -- double byte number
type Long = Integer -- eight byte number

data Instruction = 
      Aaload
    | Aastore
    | Aconst_null
    | Aload {index :: Byte}
    | Aload_0
    | Aload_1
    | Aload_2
    | Aload_3
    | Anewarray {index :: Short}
    | Areturn
    | Arraylength
    | Astore {index :: Byte}
    | Astore_0
    | Astore_1
    | Astore_2
    | Astore_3
    | Athrow
    | Baload
    | Bastore
    | Bipush {byte :: Byte}
    | Caload
    | Castore
    | Checkcast {index :: Short}
    | D2f
    | D2i
    | D2l
    | Dadd
    | Daload
    | Dastore
    | Dcmpg
    | Dcmpl
    | Dconst_0
    | Dconst_1
    | Ddiv
    | Dload {index :: Byte}
    | Dload_0
    | Dload_1
    | Dload_2
    | Dload_3
    | Dmul
    | Dneg
    | Drem
    | Dreturn
    | Dstore {index :: Byte}
    | Dstore_0
    | Dstore_1
    | Dstore_2
    | Dstore_3
    | Dsub
    | Dup
    | Dup_x1
    | Dup_x2
    | Dup2
    | Dup2_x1
    | Dup2_x2
    | F2d
    | F2i
    | F2l
    | Fadd
    | Faload
    | Fastore
    | Fcmpg
    | Fcmpl
    | Fconst_0
    | Fconst_1
    | Fconst_2
    | Fdiv
    | Fload {index :: Byte}
    | Fload_0
    | Fload_1
    | Fload_2
    | Fload_3
    | Fmul
    | Fneg
    | Frem
    | Freturn
    | Fstore {index :: Byte}
    | Fstore_0
    | Fstore_1
    | Fstore_2
    | Fstore_3
    | Fsub
    | Getfield {index :: Short}
    | Getstatic {index :: Short}
    | Goto {branch :: Short}
    | Goto_w {branch :: Int} -- 4(!) bytes
    | I2b
    | I2c
    | I2d
    | I2f
    | I2l
    | I2s
    | Iadd
    | Iaload
    | Iand
    | Iastore
    | Iconst_m1 -- (-1)
    | Iconst_0
    | Iconst_1
    | Iconst_2
    | Iconst_3
    | Iconst_4
    | Iconst_5
    | Idiv
    | If_acmpeq {branch :: Short}
    | If_acmpne {branch :: Short}
    | If_icmpeq {branch :: Short}
    | If_icmpne {branch :: Short}
    | If_icmplt {branch :: Short}
    | If_icmpge {branch :: Short}
    | If_icmpgt {branch :: Short}
    | If_icmple {branch :: Short}
    | Ifeq {branch :: Short}
    | Ifne {branch :: Short}
    | Iflt {branch :: Short}
    | Ifge {branch :: Short}
    | Ifgt {branch :: Short}
    | Ifle {branch :: Short}
    | Ifnonnull {branch :: Short}
    | Ifnull {branch :: Short}
    | Iinc {index :: Byte, const :: Byte}
    | Iload {index :: Byte}
    | Iload_0
    | Iload_1
    | Iload_2
    | Iload_3
    | Imul
    | Ineg
    | Instanceof {index :: Short}
    | Invokedynamic {index :: Short} -- + 0 0
    | Invokeinterface {index :: Short, count :: Byte} -- + 0
    | Invokespecial {index :: Short}
    | Invokestatic {index :: Short}
    | Invokevirtual {index :: Short}
    | Ior
    | Irem
    | Ireturn
    | Ishl
    | Ishr
    | Istore {index :: Byte}
    | Istore_0
    | Istore_1
    | Istore_2
    | Istore_3
    | Isub
    | Iushr
    | Ixor
    | Jsr {branch :: Short}
    | Jsr_w {branch :: Int} -- 4(!) bytes
    | L2d
    | L2f
    | L2i
    | Ladd
    | Laload
    | Land
    | Lastore
    | Lcmp
    | Lconst_0
    | Lconst_1
    | Ldc {index :: Byte}
    | Ldc_w {index :: Short}
    | Ldc2_w {index :: Short}
    | Ldiv
    | Lload {index :: Byte}
    | Lload_0
    | Lload_1
    | Lload_2
    | Lload_3
    | Lmul
    | Lneg
    | Lookupswitch -- TODO
    | Lor
    | Lrem
    | Lreturn
    | Lshl
    | Lshr
    | Lstore {index :: Byte}
    | Lstore_0
    | Lstore_1
    | Lstore_2
    | Lstore_3
    | Lsub
    | Lushr
    | Lxor
    | Monitorenter
    | Monitorexit
    | Multianewarray {index :: Short, dimensions :: Byte}
    | New {index :: Short}
    | Newarray {atype :: Byte}
    | Nop
    | Pop
    | Pop2
    | Putfield {index :: Short}
    | Putstatic {index :: Short}
    | Ret {index :: Byte}
    | Return
    | Saload
    | Sastore
    | Sipush {short :: Short}
    | Swap
    | Tableswitch -- TODO
    | Wide -- TODO

data Constant =
      CInteger Int
    | CLong Long
    | CFloat Float
    | CDouble Double
    | CUtf8 String
    | CString String
    {-| CNameAndType CUtf8 CUtf8
    | CClass CUtf8
    | CFieldref CClass CNameAndType
    | CMethodref CClass CNameAndType
    | CInterfaceMethodref CClass CNameAndType-}

type ConstantPool = [Constant]

data Class = Class {
        constantPool :: ConstantPool,
        thisClass :: Int, -- index from constantPool
        superClass :: Int, -- index from constantPool
        flags :: [Flag],
        interfaces :: [Interface],
        fields :: [Field],
        methods :: [Method],
        attributes :: [Attribute]
    }

data Method = Method {
        thisMethod :: Int, -- index from constantPool
        flags :: [Flag],
        code :: [Instruction]
    }

data Interface --TODO

data Attribute -- TODO

data Field = Int -- index from constantPool

data Flag -- TODO
