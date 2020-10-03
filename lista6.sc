//Justyna Zieniewicz

//zadanie 1
def whileLoop(condition: => Boolean)(statement: => Unit): Unit =
  if (condition) {
    statement
    whileLoop(condition)(statement)
  }

var count = 0
whileLoop(count < 5) {
  println(count)
  count += 1
}


//zadanie 2
def lrepeat[A] (k: Int) (stream: LazyList[A]): (LazyList [A]) = {
  def lrepeat2(n: Int)(s: LazyList[A]): LazyList[A] =
    s match {
      case h #:: t => if (n > 0) h #:: lrepeat2(n - 1)(s) else lrepeat(k)(t)
      case _ => LazyList[A]()
    }

  lrepeat2(k)(stream)
}

(lrepeat(3) (LazyList('a','b','c','d'))).toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
(lrepeat (3) (LazyList.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
lrepeat(2)(LazyList.empty).toList == List()

//zadanie 3
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

//a)
def lBreadth[A](ltree: lBT[A]): LazyList [A] = {
  def lBreadth2(queue: List[lBT[A]]): LazyList[A] = queue match {
    case LNode(e, l, r) :: t => e #:: lBreadth2(t ::: List(l(), r()))
    case LEmpty :: t => lBreadth2(t)
    case Nil => LazyList[A]()
  }

  lBreadth2(List(ltree))
}



//b)
def lTree(n: Int): lBT[Int] = {
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))
}


lBreadth(lTree(1)).take(10).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
lBreadth(lTree(2)).take(7).toList == List(2, 4, 5, 8, 9, 10, 11)
lBreadth(LEmpty).toList == Nil