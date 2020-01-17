var fib: Array<Int> /*= Array(4,{i -> i*i})*/

class MyClass (var name: String, var age: Int) {
    init {
        println("Constructor launched")
    }
    fun Bar() { /*add this */
        println("Test passed")
    }
}

fun Main() {
    val a = MyClass("name", 10)
    a.age = 11
    println(a.age)
    a.Bar()
   /* fib = 0..10
    var i = 2
    while (i < 11) {
        fib[i] = fib[i - 2] + fib[i - 1]
        i = i + 1
    }
    println(fib)*/
}
