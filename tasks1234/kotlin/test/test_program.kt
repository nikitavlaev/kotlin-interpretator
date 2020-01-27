fun f(arr:Array<Any>) {
    arr[0] = 3.14;
}

fun Main() {
    var a = Array(10) {i -> i + 2}
    if (a[1] == 3)
        println("!!!")
    f(a)
    println("...")
}

