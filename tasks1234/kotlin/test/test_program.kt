fun f(arr:Array<Int>) {
    arr[0] = 1
}

fun Main() {
    var a = Array(10) {i -> 1 }
    f(a)
    println(a)
}

