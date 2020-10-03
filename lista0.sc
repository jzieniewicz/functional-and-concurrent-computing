//Justyna Zieniewicz
//zadanie 1

def last[A](xs: List[A]): A =
  if (xs == Nil)
    throw new NoSuchElementException("last of empty list")
  else if (xs.tail != Nil)
    last(xs.tail)
  else xs.head



last(List(3, 6.0, 7.5))
last(List("x"))
last(Nil)