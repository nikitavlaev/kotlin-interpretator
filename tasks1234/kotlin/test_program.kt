fun Foo(val a: Int, var b: String):Int
{
    a+b;
    throw MyException;
    return a;
}

fun Bar(val a: Int, var b: String):String
{
    a=b + a;
    return a;
}