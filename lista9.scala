//Justyna Zieniewicz

//1a
//Fragmentem kodu, który jest źródłem błędu jest metoda readWriteCounter, która nie zakłada synchronizacji wątków. Brak monitora powoduje, że oba wątki mają do niej dostęp w tym samym czasie.
//Przez to dochodzi do sytuacji, w których wywłaszczenie wątku następuje zanim poprzedni dokona inkrementacji zmiennej, tym samym ten nowy wątek wciąż widzi jego starą wartość.
//Chociaż każdy wątek zwiększył wspólny licznik 200 000 razy i program się zakończył, to kiedy dochodziło do błędu z przedwczesnym wywłaszczeniem, zwiększenie wartości zmiennej nie było rejestrowane.
//Z tego powodu wynik jest zwykle niższy niż 400 000 tysięcy, a przy każdym uruchomieniu jego wartość jest różna.

object Zad1 extends App {
  var counter = 0 // counter variable
  def readWriteCounter(): Unit = {
    val incrementedCounter = counter + 1 // reading counter
    counter = incrementedCounter // writing to counter
     //counter += 1 // shorter code
  }
  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val startTime = System.nanoTime
  p.start; q.start
  p.join; q.join
  val estimatedTime = (System.nanoTime - startTime)/1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}

//1b
object Zad1b{
  var counter = 0 // counter variable

  def readWriteCounter(): Unit = this.synchronized {
    counter += 1 // shorter code
  }

  def main(args: Array[String]): Unit = {
    val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter)
    val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter)
    val startTime = System.nanoTime
    p.start; q.start
    p.join; q.join
    val estimatedTime = (System.nanoTime - startTime) / 1000000
    println(s"The value of counter = $counter")
    println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")

    println("\nCzy counter jest równy 400 000?")
    println(counter == 400000)
  }
}

//1c

import java.util.concurrent.Semaphore
object Zad1c{
  var counter = 0 // counter variable
  val semaphore = new Semaphore(1)

  def readWriteCounter(): Unit = {
    semaphore.acquire()
    counter += 1 // shorter code
    semaphore.release()
  }

  def main(args: Array[String]): Unit = {
    val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter)
    val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter)
    val startTime = System.nanoTime
    p.start;
    q.start
    p.join;
    q.join
    val estimatedTime = (System.nanoTime - startTime) / 1000000
    println(s"The value of counter = $counter")
    println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")

    println("\nCzy counter jest równy 400 000?")
    println(counter == 400000)
  }
}

//zadanie 2
object Zad2 {
  def parallel[A, B](block1: =>A, block2: =>B): (A, B) = {
    var a: Option[A] = None
    var b: Option[B] = None
    val p = new Thread(() => a = Some(block1))
    val q = new Thread(() => b = Some(block2))
    p.start()
    q.start()
    p.join()
    q.join()
    (a.get, b.get)
  }

  def main(args: Array[String]): Unit = {
    println(parallel("a" + 1, "b" + 2))
    println(parallel(Thread.currentThread.getName, Thread.currentThread.getName))
    val x = 2
    println(parallel(x * 2, x - 50))
  }


}

//Zadanie 3
//Rolą wątków demonów jest służenie innym wątkom. Program może się zakończyć nawet kiedy ich działanie jeszcze się nie skończyło.
// W naszym przypadku wątek z linijki 108 kończy swoją pracę, a pozostałe wątki (a właściwie 1 wątek, który jeszcze się nie zakończył) są wątkami demonami.
// Z tego powodu program zakończył się zanim wypisał literkę x 25 razy.
object Zad3 {
  def periodically(duration: Long, times: Int)(block: => Unit): Unit = {
    val daemon = new Thread(() =>
      for (_ <- 0 until times) {
        block
        Thread.sleep(duration)
      }
    )

    daemon.setDaemon(true)
    daemon.start()
  }

  def main(args: Array[String]): Unit = {
    periodically(1000, 5){print("y ")}
    periodically(1000, 25){print("x ")}
    Thread.sleep(10000)
    println("Done sleeping")
  }
}


