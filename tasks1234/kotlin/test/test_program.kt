fun f(arr:Array<Any>) {
    arr[0] = Object();
}

fun Main() {
    var a = Array(10) {i -> Object()}
    if (a[1] == 3)
        println("!!!")
    f(a)
    println("...")
}

