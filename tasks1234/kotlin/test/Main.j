.class public jsmClasses/Main
.super java/lang/Object
.method public static f([Ljava/lang/Object;)V
.limit stack 1024
.limit locals 1024
aload 0
ldc 0
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
return
.end method
.method public static main([Ljava/lang/String;)V
.limit stack 1024
.limit locals 1024
iconst_0
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
astore 2
ldc 10
anewarray java/lang/Object
dup
iconst_0
aload 2
aastore
dup
ldc 1
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
dup
ldc 2
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
dup
ldc 3
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
dup
ldc 4
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
dup
ldc 5
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
dup
ldc 6
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
dup
ldc 7
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
dup
ldc 8
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
dup
ldc 9
dup
istore 2
new java/lang/Object
dup
invokespecial java/lang/Object."<init>":()V
aastore
astore 1
aload 1
ldc 1
aaload
ldc 3
if_acmpne else101
iconst_1
goto fi101
else101:
iconst_0
fi101:
ifeq else111
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "!!!"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
goto fi111
else111:
fi111:
aload 1
invokestatic jsmClasses/Main/f([Ljava/lang/Object;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "..."
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
return
.end method
