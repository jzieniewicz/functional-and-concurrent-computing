//Justyna Zieniewicz

//zadanie 1
val suma: List[Double] => Double =
  xs =>
    if(xs == Nil) 0.0
    else xs.head + suma(xs.tail)

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6


//zadanie 2
def ends[A](xs: List[A]): (A, A) =
  if(xs == Nil) throw new NoSuchElementException("empty list")
  else if(xs.tail == Nil) (xs.head, xs.head)
  else (xs.head, ends(xs.tail)._2)


ends(List(1,2, 3, 5)) == (1,5)
ends(List(1)) == (1,1)
try {
  ends(Nil)
} catch {
  case e: NoSuchElementException => true
}



//zadanie 3
val posortowana: List[Int] => Boolean =
  xs =>
    if(xs == Nil || xs.tail == Nil) true
    else xs.head <= xs.tail.head && posortowana(xs.tail)

posortowana(List(1, 3, 3, 5, 6, 7))
posortowana(Nil)
posortowana(List(1))
!posortowana(List(3, 2, 1))



//zadanie 4
val glue: (List[String], String) => String =
  (xs, ys) =>
    if (xs == Nil) ""
    else if (xs.tail == Nil) xs.head
    else s"${xs.head}$ys${glue(xs.tail, ys)}"


glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
glue(Nil, "-") == ""
glue(List("To"), "&") == "To"


