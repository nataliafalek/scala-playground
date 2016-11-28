//val a = 325
//val b = a%100
//val lista = List(List(2, 4), List(2,2))
//val list2 = lista.transpose
//println(list2)


val randomm = scala.util.Random
val randomm_number = (randomm.nextInt(3-1) + 1)*2



val macierz = List(List(0,0,0,2),List(0,0,0,2))
val a = List(0,0,0,2).zipWithIndex
val b = List(0,0,0,2).zipWithIndex
val newList = List(a, b).zipWithIndex.map { case (rowWithIndex, rowIndex) =>
  rowWithIndex.filter( { case (value, idx) => value == 0 })
    .map { case (value, idx) => (rowIndex, idx)}
}.flatten

val r = scala.util.Random

val random_place = r.nextInt(newList.size)

val list2 = newList(random_place)
val random_column = list2._1
val random_row = list2._2



println(macierz(random_column)(random_row))

macierz.updated(random_column, macierz(random_column).updated(random_row, randomm_number))



List(List.fill(4)(0), List.fill(4)(0), List.fill(4)(0), List.fill(4)(0))
