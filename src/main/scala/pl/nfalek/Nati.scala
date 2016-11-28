package pl.nfalek

object Nati {
  def main(args: Array[String]) {
    println("hau")
    println(List(1,2,3))
    println("hau")
    println(countCents(65544))
    println(rolDice())
    println(list)
    println(countPeopleByGender(list))
  }

  // 2. Napisz kalkulator z groszy na ilosc calkowitych zlotowek np.
  //  countCents(325) == "3 zlote i 25 groszy"

def countCents(a: Int): String = {
  a / 100 + " zlote " + a % 100 + " grosze"
}

  //symulator rzutu kostka

def rolDice(): Int = {
  (Math.random()*5 + 1).toInt

}

  //zad 2 obiektowe

  case class Person(name: String, gender: String)

  val bey = Person("Beyonce", "f")
  val mad = Person("Madonna", "f")
  val bow = Person("Bowie", "m")

  val list = List(bey,mad,bow)

  case class GenderSummary(woman: Int, man: Int)

  def countPeopleByGender(people: List[Person]): GenderSummary = {
    val womanCount = people.filter(p => p.gender == "f").size
    val manCount = people.filter(p => p.gender == "m").size
    GenderSummary(womanCount, manCount)
  }
}