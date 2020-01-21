fun f(arr:Array<Any>) {
    arr[0] = "asdf"
}

fun Main() {
    var a = Array(10) {i -> i + 2}
    f(a)
    println(a[0])
}

