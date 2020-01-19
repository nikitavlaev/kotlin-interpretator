class Person (val name: String) {
    
    val favoriteHat : Person.Hat
    var age : Int

    class Hat {}
    
    init {
        println(name + " was born!")
        this.age = 0
    }

    fun buyNewFavoriteHat() {
        println(this.name + " bought a new favorite hat.")
        this.favoriteHat = this.Hat()
    }

    fun afterOneYear() {
        println("Happy birthday, " + this.name + "!")
        this.age = this.age + 1
    }
}

fun Main() {
    val a = Person("Bob")
    a.age = 11
    println(a.age)
    a.buyNewFavoriteHat()
    a.afterOneYear()
    print(a.name + " is ")
    print(a.age)
    println(" years ago.")
    a.afterOneYear()
    print(a.name + " is ")
    print(a.age)
    println(" years ago.")
}

