//Justyna Zieniewicz

//zadanie 1
class MyQueue[+T] private (private val out: List[T], private val in: List[T]) {

  def this() = this(Nil, Nil)

  //enqueue : Elem * Queue -> Queue
  def enqueue[S >: T](elem: S):MyQueue[S] = out match {
     case Nil => new MyQueue[S](elem::out, in)
     case _ => new MyQueue[S](out, elem::in)
  }

  //first : Queue -> Elem
  def first: T = out match {
     case Nil => throw new NoSuchElementException("Empty queue")
     case h::_ => h
  }

  //firstOption: Queue -> Option[Elem]
  def firstOption(): Option[T] = out match {
    case h :: _ => Some(h)
    case Nil => None
  }

  //dequeue : Queue -> Queue
  def dequeue(): MyQueue[T] = out match {
    case _ :: Nil => if (in == Nil) MyQueue.empty else new MyQueue[T](in.reverse, Nil)
    case _ :: t => new MyQueue[T](t, in)
    case Nil => this
  }

  //isEmpty : Queue -> bool
  def isEmpty: Boolean = out == Nil

  override def toString: String = {
    if (in == Nil && out == Nil) "empty"
    else (in ::: out.reverse) mkString ", "
  }

}

object MyQueue{

  def empty[A]: MyQueue[A] =  new MyQueue[A](Nil, Nil)

  def apply[T](xs: T*): MyQueue[T] = new MyQueue[T](xs.toList, Nil)
}


//zadanie 2
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

object Lista7{
  def main (args: Array[String]): Unit = {

    //proste testy dla kolejki (utwórz ją na cztery podane sposoby)
    val queue1 = new MyQueue
    val queue2 = MyQueue()
    val queue3 = MyQueue.empty
    val queue4 = MyQueue('a', 'b', 'c')


    println("testy dla kolejki")

    println("isEmpty (empty) == true")
    println(queue1.isEmpty)

    println("isEmpty (enqueue (x)) == false")
    println(!queue3.enqueue(1).isEmpty)

    println("first (non empty) == x")
    println(queue4.first == 'a')

    println("dequeue (empty) == empty")
    println(queue1.dequeue)

    println("dequeue (enqueue(e1,empty)) = empty")
    println(queue1.enqueue(1).dequeue)

    println("first (empty) == ERROR")
    try {
      queue1.first
    } catch {
      case e: NoSuchElementException => println(true)
      case e: Exception => println(false)
    }

    println("firstOption(empty) == None")
    println(queue1.firstOption() == None)

    println("firstOption(enqueue(1)) == Some(1)")
    println(queue1.enqueue(1).firstOption() == Some(1))

    println("first (enqueue(1)) == 1")
    println(queue1.enqueue(1).first == 1)


    //obejście wszerz drzewa pustego i dwóch drzew z wykładu 4 (t i tt)
    println("testy drzew binarnych")
    println(breadthBT(Empty) == List())
    println(breadthBT(t) == List(1, 2, 3))
    println(breadthBT(tt) == List(1, 2, 3, 4, 5, 6))

  }

  //zadanie 2
  def breadthBT[A](tree: BT[A]): List[A] = {
    def breadth(queue: MyQueue[BT[A]], list: List[A]): List[A] =
      queue.firstOption() match {
        case Some(Node(e, l, r)) => e :: breadth(queue.dequeue().enqueue(l).enqueue(r), list)
        case Some(Empty) => breadth(queue.dequeue(), list)
        case None => Nil
      }
    breadth(MyQueue(tree), Nil)
  }

  val t = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty)


  val tt = Node(1,
    Node(2,
      Node(4,
        Empty,
        Empty
      ),
      Empty
    ),
    Node(3,
      Node(5,
        Empty,
        Node(6,
          Empty,
          Empty
        )
      ),
      Empty
    )
  )


}
