//Justyna Zieniewicz

import java.util.concurrent.{ArrayBlockingQueue, Semaphore}

import scala.concurrent.ExecutionContext
import scala.util.Random

object Zad1 {
  class BoundedBuffer(N: Int) {
    private var in, out, n: Int = 0
    private val elems = new Array[Int](N)
    def put(x: Int) = this.synchronized {
      while (n >= N) {println(s"${Thread.currentThread.getName} waiting"); wait}
      elems(in) = x ; in = (in + 1) % N ; n += 1; println(s"${Thread.currentThread.getName} putting $x")
      elems.foreach(e => print(s"$e ")); println
      if (n == 1) notifyAll
    }
    def take: Int = this.synchronized {
      while (n == 0) {println(s"${Thread.currentThread.getName} waiting"); wait}
      val x = elems(out) ; elems(out) = 0 ; out = (out + 1) % N ; n -= 1
      elems.foreach(e => print(s"$e ")); println
      if (n == N-1) notifyAll
      x
    }
  }

  class Producer(name: String, buf: BoundedBuffer) extends Thread(name) {
    override def run: Unit =
      for (i <- 1 to 10) {println(s"$getName producing $i"); buf.put(i)}
  }
  class Consumer(name: String, buf: BoundedBuffer) extends Thread(name) {
    override def run: Unit =
      for (i <- 1 to 10) println(s"$getName consumed ${buf.take}")
  }

//  object prodCons {
    def main(args: Array[String]): Unit = {
      val buf: BoundedBuffer = new BoundedBuffer(5)
      new Producer("Producer", buf).start
      new Consumer("Consumer", buf).start
    }
//  }

}

//zadanie 1 a)
object Zad1a {

  class Producer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
    override def run(): Unit =
      for (i <- 1 to 10) {
        println(s"$getName producing $i")
        buf.put(i)
        println(buf)
      }
  }

  class Consumer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
    override def run(): Unit =
      for (i <- 1 to 10) println(s"$getName consumed ${buf.take}")
  }


  def main(args: Array[String]): Unit = {
    val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue(5)
    new Producer("Producer", buf).start
    new Consumer("Consumer", buf).start
  }


}



//zadanie 1 b)
//W zadaniu 1b) występuje 2 producentów i 3 konsumentów, oczekujących na dane od producentów.
//Ze względu na tę dysproporcję program się nie kończy.
// Powoduje to użycie funkcji ArrayBlockingQueue::take(), która w przypadku braku elementów w kolejce czeka tak długo aż jakiś się pojawi, co nie nastąpi, bo producentów jest mniej.

object Zad1b {

  class Producer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
    override def run(): Unit =
      for (i <- 1 to 10) {
        println(s"$getName producing $i")
        buf.put(i)
        println(buf)
      }
  }

  class Consumer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
    override def run(): Unit =
      for (i <- 1 to 10) println(s"$getName consumed ${buf.take}")
  }


   def main(args: Array[String]): Unit = {
     val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue(5)

     val producerCount = 2
     val consumerCount = 3

     for (i <- 1 to producerCount) {
       new Producer(s"Producer$i", buf).start
     }

     for (i <- 1 to consumerCount) {
       new Consumer(s"Consumer$i", buf).start
     }
   }

}

//Liczba producentów i konsumentów jest identyczna w zadaniu 1c), pomimo tego jednak program się kończy.
//Zawdzięczamy to użyciu ExecutionContext.global, wykorzystującego ForkJoinPool, który tworzy wątek demona i wątki robocze, zależne od demona.
//Po zakończeniu wątków demonów, wątki robocze, w tym przypadku odpowiedzialne za dostarczanie i konsumowanie danych z bufora, kończą się.

//zadanie 1 c
object Zad1c {


  def main(args: Array[String]): Unit = {
    val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue(5)
    val ectx = ExecutionContext.global

    val producerCount = 2
    val consumerCount = 3

    for (i <- 1 to producerCount) {
      ectx.execute(() =>
        for (j <- 1 to 10) {
          println(s"Producer$i producing $j")
          buf.put(j)
          println(buf)
        }
      )
    }

    for (i <- 1 to consumerCount) {
      ectx.execute(() => for (_ <- 1 to 10) println(s"Consumer$i consumed ${buf.take}"))
    }
    Thread.sleep(500)
  }


}


// zadanie 2
object Zad2 {

  class Table(private val seats: Int) {

    private[this] val sticks: List[Semaphore] = List.fill(seats)(new Semaphore(1))

    private[this] val doorman = new Semaphore(seats - 1)

    private[this] val random = new Random()

    def eat(id: Int): Unit = {
      println(s"Philosopher $id entered dinning room")

      val stick1 = id
      val stick2 = (id + 1) % seats

      sticks(stick1).acquire()
      sticks(stick2).acquire()

      println(s"Philosopher $id starts eating")
      val time = countTime({
        Thread.sleep(getRandomBetween)
        sticks(stick1).release()
        sticks(stick2).release()
      })
      println(s"Philosopher $id eating time: $time ms")

      doorman.release()
      println(s"Philosopher $id left dinning room")
    }

    def meditate(id: Int): Unit = {
      val time = countTime({
        Thread.sleep(getRandomBetween)
        doorman.acquire()
      })
      println(s"Philosopher $id meditation time: $time ms")
    }

    private def countTime(task: => Unit): Long = {
      val startTime = System.nanoTime
      task
      (System.nanoTime - startTime) / 1000000
    }

    private def getRandomBetween: Int = {
      val start = 1000
      val end = 4000
      start + random.nextInt((end - start) + 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val philosophers = 5
    val rounds = 10
    val table = new Table(philosophers)

    for (i <- 0 until philosophers) {
      println(s"Summoned philosopher $i")
      new Thread(() =>
        for (_ <- 1 to rounds) {
          table.meditate(i)
          table.eat(i)
        }
      ).start()
    }
  }
}