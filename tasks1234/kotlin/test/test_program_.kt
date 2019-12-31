fun Main() {
    val ans = (readLine())!!
    if (ans[0] == 'y')
    {
        var a : Byte? = 6
        print("a = ")
        println(a)
        while (a > 0) {
            a = a - 1
            print("a = ")
            println(a)
        }
    }
    else
    {
        var a
        a = 4
        println("Your answer = " + ans)
    }
}

