.class public jsmClasses/Main
.super java/lang/Object
.method public static fact(I)I
.limit stack 1024
.limit locals 1024
iload 0
ldc 0
if_icmpne else5
iconst_1
goto fi5
else5:
iconst_0
fi5:
ifeq else13
ldc 1
goto fi13
else13:
iload 0
iload 0
ldc 1
isub
invokestatic jsmClasses/Main/fact(I)I
imul
fi13:
ireturn
.end method
.method public static main([Ljava/lang/String;)V
.limit stack 1024
.limit locals 1024
ldc 5
invokestatic jsmClasses/Main/fact(I)I
istore 2
iload 2
ldc 120
if_icmpne else32
iconst_1
goto fi32
else32:
iconst_0
fi32:
ifeq else40
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Wow!"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
goto fi40
else40:
fi40:
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "..."
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
return
.end method
