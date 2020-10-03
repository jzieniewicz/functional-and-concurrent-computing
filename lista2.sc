//Justyna Zieniewicz

//zadanie 1
def take[A](n: Int, xs:List[A]): List[A] =
  xs match {
    case h::t => if(n>0) h :: take(n-1, t) else Nil
    case Nil => Nil
  }

take(2, List(1,2,3,5,6)) == List(1,2)
take(-2, List(1,2,3,5,6)) == Nil
take(8, List(1,2,3,5,6)) == List(1,2,3,5,6)
take(2, List()) == Nil


//zadanie 2
def drop[A](n: Int, xs: List[A]): List[A] =
  xs match {
    case h::t => if(n>0) drop(n-1, t) else xs
    case Nil => Nil
  }

drop(2, List(1,2,3,5,6)) == List(3,5,6)
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(8, List(1,2,3,5,6)) == Nil
drop(2, List()) == Nil

//zadanie 3
def reverse[A](xs: List[A]): List[A] = {
  def reverseInter(xs: List[A], ys: List[A]): List[A] =
    xs match {
      case h::t  => reverseInter(t, h::ys)
      case Nil => ys
    }
  reverseInter(xs, Nil)
}

reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(List()) == Nil
reverse(List("Ala")) == List("Ala")

//zadanie 4
val replicate: List[Int] => List[Int] =
  xs => {
    def replicateInter(x: Int, i: Int, tail: List[Int]): List[Int] =
      if (i > 0) x::replicateInter(x, i-1, tail)
      else replicate(tail)

    xs match {
      case h::t => replicateInter(h, h, t)
      case Nil => Nil
    }
  }

replicate (List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(List()) == Nil
replicate(List(0, -1, -2)) == Nil

//zadanie 5
def root3(a: Double): Double = {
    val precision = 10e-15
    def root3Inter(xi: Double): Double =
      if (Math.abs(xi*xi*xi - a) <= precision*Math.abs(a)) xi
      else root3Inter(xi + ((a / (xi*xi)) - xi)/3)

  root3Inter(if (a > 1) a/3 else a)
  }

root3(8.0) == 2.0
root3(-8.0) == -2.0
root3(0.0) == 0.0

