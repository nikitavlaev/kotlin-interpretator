fun fact(n : Int) : Int {
  if (n == 0) {
    1
  }
  else {
    n * fact(n - 1)
  }
}

fun Main(arr: Array<String>) {
  val a = fact(5)
  if (a == 120) {
    println("Wow!")
  }
  println("...")
}  