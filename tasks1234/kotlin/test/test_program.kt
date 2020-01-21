fun f(arr:Array<Object>) {
    arr[0] = Object()
}

fun Main() {
    var a = Array(10) {i -> Object()}
    f(a)
    println(a[0])
}

