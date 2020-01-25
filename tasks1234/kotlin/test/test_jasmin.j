; --- Copyright Jonathan Meyer 1996. All rights reserved. -----------------
; File:      jasmin/examples/HelloWorld.j
; Author:    Jonathan Meyer, 10 July 1996
; Purpose:   Prints out "Hello World!"
; -------------------------------------------------------------------------


.class public jsmClasses/test_jasmin
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
	.limit stack 2
	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Hello world wow!"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	return	
.end method
