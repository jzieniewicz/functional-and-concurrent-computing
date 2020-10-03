//Justyna Zieniewicz


//zadananie 1
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]
val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)

def  sumBT(bt: BT[Int]): Int =
  bt match {
    case Node(elem, left, right) => elem + sumBT(left) + sumBT(right)
    case Empty => 0
  }

sumBT(t) == 6
sumBT(Empty) == 0
sumBT(Node(1, Empty, Node(1, Empty, Empty))) == 2

//zadanie 2
def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B =
  bt match {
    case Node(elem, left, right) => f(elem)(foldBT(f)(acc)(left), foldBT(f)(acc)(right))
    case Empty => acc
  }

//zadanie 3
//a)
def sumBTfold(bt: BT[Int]): Int =
  foldBT((elem: Int) => (left: Int, right: Int) => elem + left + right)(0)(bt)

sumBTfold(t) == 6
sumBTfold(Empty) == 0
sumBTfold(Node(1, Empty, Node(1, Empty, Empty))) == 2

//b)
def inorderBTfold[A](bt: BT[A]): List[A] =
  foldBT((elem: A) => (left: List[A], right: List[A]) => left ::: elem :: right)(Nil)(bt)

inorderBTfold(t) == List(2, 3, 1)
inorderBTfold(Empty) == Nil
inorderBTfold(Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))) == List(2, 1, 3)

//zadanie 4
def mapBT[A, B](f: A => B)(tree: BT[A]): BT[B] =
  foldBT[A, BT[B]](elem => Node(f(elem), _, _): BT[B])(Empty)(tree)

mapBT((v: Int) => 2 * v)(t: BT[Int]) == Node(2,Node(4,Empty,Node(6,Empty,Empty)),Empty)
mapBT(2 * (_: Int))(Empty) == Empty
mapBT(4 + (_: Int))(Node(1, Node(2, Empty, Empty), Node(-3, Empty, Empty))) ==
  Node(5, Node(6, Empty, Empty), Node(1, Empty, Empty))

//zadanie 5
sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => List(5)
    case 4 => List(0, 2)
    case 5 => List(3)
    case n => throw new Exception(s"Graph g: node $n doesn't exist")
  })

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
  def search(visited: List[A])(toVisit: List[A]): Boolean =
    toVisit match {
      case h :: t => if (visited contains h) search(visited)(t) else h == to || search(h :: visited)(t ::: (g succ h))
      case Nil => false
    }
  search(Nil)(List(from))
}

pathExists(g)(4, 1)
!pathExists(g)(0, 4)
!pathExists(g)(3, 0)
val g2 = Graph((i: Int) =>
  i match {
    case 0 => List(1, 2, 3)
    case n => throw new NoSuchElementException(s"Graph g2: node $n doesn't exist")
  })
pathExists(g2)(0, 1)
pathExists(g2)(0, 0)





