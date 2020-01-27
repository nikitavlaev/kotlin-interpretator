.class public jsmClasses/Main
.super java/lang/Object
.method public static f([Ljava/lang/Object;)V
.limit stack 1024
.limit locals 1024
aload 0
ldc 0
ldc_w 3.14
dastore
return
.end method
.method public static main()V
.limit stack 1024
.limit locals 1024
iconst_0
istore 1
iload 1
ldc 2
iadd
istore 1
ldc 10
newarray int
dup
iconst_0
iload 1
iastore
dup
ldc 1
dup
istore 1
iload 1
ldc 2
iadd
iastore
dup
ldc 2
dup
istore 1
iload 1
ldc 2
iadd
iastore
dup
ldc 3
dup
istore 1
iload 1
ldc 2
iadd
iastore
dup
ldc 4
dup
istore 1
iload 1
ldc 2
iadd
iastore
dup
ldc 5
dup
istore 1
iload 1
ldc 2
iadd
iastore
dup
ldc 6
dup
istore 1
iload 1
ldc 2
iadd
iastore
dup
ldc 7
dup
istore 1
iload 1
ldc 2
iadd
iastore
dup
ldc 8
dup
istore 1
iload 1
ldc 2
iadd
iastore
dup
ldc 9
dup
istore 1
iload 1
ldc 2
iadd
iastore
astore 0
aload 0
invokestatic jsmClasses/Main/f([Ljava/lang/Object;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "..."
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
return
.end method
