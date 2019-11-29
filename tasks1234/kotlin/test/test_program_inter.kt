fun say_hello() {
    println("What is your name?")
    val name = readLine()
    print("Hello, ")
    println(name)
}

fun Main() {
    println("Start...")
    say_hello()
}
