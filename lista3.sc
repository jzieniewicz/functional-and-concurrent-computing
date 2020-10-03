//Justyna Zieniewicz

//zadanie 1
//a)
def existsA[A] (xs: List[A]) (p: A => Boolean): Boolean =
  xs match {
    case h::t => p(h) || existsA(t)(p)
    case Nil => false
  }

existsA (List(5,1,2,3)) (_ == 2)
!existsA (List(5,1,3)) (_ == 2)
!existsA (Nil) (_ == 2)

//b)
def existsB[A] (xs: List[A]) (p: A => Boolean): Boolean =
  xs.foldLeft(false)((i, x) => i || p(x))

existsB (List(5,1,2,3)) (_ == 2)
!existsB (List(5,1,3)) (_ == 2)
!existsB (Nil) (_ == 2)

//c)
def existsC[A] (xs: List[A]) (p: A => Boolean): Boolean =
  xs.foldRight(false)((x, i) => i || p(x))

existsC (List(5,1,2,3)) (_ == 2)
!existsC (List(5,1,3)) (_ == 2)
!existsC (Nil) (_ == 2)

//zadanie 2
def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  xs.foldRight(List[A]())((x, i) => if(p(x)) x::i else i)

filter (List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7, 7, 8, 4, 6, 9)
filter(List(1, 1, 1, 1))(_ > 1) == Nil
filter(List(Nil))(_ == 1) == List()

//zadanie 3
//a)
def remove1A[A](xs: List[A])(p: A => Boolean): List[A] =
  xs match {
    case h::t => if(p(h)) t else h::remove1A(t)(p)
    case Nil => Nil
  }

remove1A(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1A(Nil)(_ == 2) == Nil
remove1A(List(1,2,3,2,5)) (_ == 6) == List(1,2,3,2,5)

//b)
def remove1B[A](xs: List[A])(p: A => Boolean): List[A] = {
  def removeInter(xs: List[A], result: List[A]): List[A] =
    xs match {
      case h::t => if(p(h)) t.reverse_:::(result) else h::removeInter(t, result)
      case Nil => Nil
    }
  removeInter(xs, Nil)
}

remove1B(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1B(Nil)(_ == 2) == Nil
remove1B(List(1,2,3,2,5)) (_ == 6) == List(1,2,3,2,5)



//zadanie 4
def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) = {
  def splitAtInter(xs: List[A], n: Int, result: List[A]): (List[A], List[A]) =
    xs match {
      case h::t => if(n == 0) (result.reverse, xs) else splitAtInter(t, n-1, h::result)
      case Nil => (result.reverse, Nil)
    }
  splitAtInter(xs, n, Nil)
}

splitAt (List('a','b','c','d','e')) (2) == (List('a', 'b'), List('c', 'd', 'e'))
splitAt (List('a','b','c','d','e')) (10) == (List('a', 'b', 'c', 'd', 'e'), List())
splitAt (List())(1) == (List(), List())





