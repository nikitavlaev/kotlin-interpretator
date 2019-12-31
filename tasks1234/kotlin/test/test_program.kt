var fib: Array<Int>

fun Main() {
    fib = 0..10
    var i = 2
    while (i < 11) {
        fib[i] = fib[i - 2] + fib[i - 1]
        i = i + 1
    }
    println(fib)
}
