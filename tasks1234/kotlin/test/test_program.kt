fun f(arr:Array<Object>) : Array<Object> {
    arr[0] = "asdf"
    arr
}

fun Main() {
    var a = Array(10) {i -> Object()}
    a = f(a)
    println(a[0])
}

