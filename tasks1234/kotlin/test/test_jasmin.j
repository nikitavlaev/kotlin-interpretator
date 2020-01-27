.class public jsmClasses/test_jasmin
.super java/lang/Object
.method public static foo()V
.limit stack 1024
.limit locals 1024
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Wow!"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit stack 1024
.limit locals 1024
invokestatic Main/Foo()V
return
.end method
