.class public jsmClasses/Main
.super java/lang/Object
.method public static f([I)V
.limit stack 1024
.limit locals 1024
aload 0
ldc 0
ldc "asdf"
aastore
return
.end method
.method public static main([Ljava/lang/String;)V
.limit stack 1024
.limit locals 1024
iconst_0
istore 2
iload 2
ldc 2
iadd
istore 2
ldc 10
newarray int
dup
iconst_0
iload 2
iastore
dup
ldc 1
dup
istore 2
iload 2
ldc 2
iadd
iastore
dup
ldc 2
dup
istore 2
iload 2
ldc 2
iadd
iastore
dup
ldc 3
dup
istore 2
iload 2
ldc 2
iadd
iastore
dup
ldc 4
dup
istore 2
iload 2
ldc 2
iadd
iastore
dup
ldc 5
dup
istore 2
iload 2
ldc 2
iadd
iastore
dup
ldc 6
dup
istore 2
iload 2
ldc 2
iadd
iastore
dup
ldc 7
dup
istore 2
iload 2
ldc 2
iadd
iastore
dup
ldc 8
dup
istore 2
iload 2
ldc 2
iadd
iastore
dup
ldc 9
dup
istore 2
iload 2
ldc 2
iadd
iastore
astore 1
aload 1
ldc 1
iaload
ldc 3
if_icmpne else99
iconst_1
goto fi99
else99:
iconst_0
fi99:
ifeq else109
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "!!!"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
goto fi109
else109:
fi109:
aload 1
invokestatic jsmClasses/Main/f([I)V
aload 1
ldc 0
iaload
ldc 5
if_icmpne else118
iconst_1
goto fi118
else118:
iconst_0
fi118:
ifeq else128
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "!!!!!"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
goto fi128
else128:
fi128:
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "..."
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
return
.end method
