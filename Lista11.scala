import scala.concurrent.{Future, Promise}
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import scala.io.Source
//Justyna Zieniewicz

//zadanie 1
//a

object Zad1a {
  def pairFut[A, B] (fut1: Future[A], fut2: Future[B]): Future[(A, B)] = fut1 zip fut2

  def main(args: Array[String]): Unit = {
    val result = pairFut(Future {"This is fut1"}, Future {" and this is fut2"})
    println(result.isCompleted)
    println(result.value)
  }
}

//b
object Zad1b {
  def pairFut[A, B] (fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    for {
      x <- fut1
      y <- fut2
    } yield (x, y)

  def main(args: Array[String]): Unit = {
    val result = pairFut(Future {"This is fut1"}, Future {" and this is fut2"})
    println(result.isCompleted)
    Thread.sleep(3000)
    println(result.isCompleted)
    println(result.value)
  }
}

//zadanie 2
//a
object Zad2a {

  implicit class FutureOps[T](val self: Future[T]) {
    def exists(p: T => Boolean): Future[Boolean] = {
      val promise  = Promise[Boolean]
      self onComplete {
        case Success(value) => promise.success(p(value))
        case Failure(_) => promise.success(false)
      }
      promise.future
    }
  }

  def main(args: Array[String]): Unit = {
    val success = Future {1}.exists(_ > 0)
    val failure = Future {-1}.exists(_ > 0)
    val exception = Future {1 / 0}.exists(_ > 0)

    println(success.value)
    println(failure.value)
    println(exception.value)
  }
}

//b
object Zad2b {
  implicit class FutureOps[T](val self: Future[T]) {
    def exists(p: T => Boolean): Future[Boolean] = {
      self map p recover { case _ => false }
    }
  }

  def main(args: Array[String]): Unit = {
    val success = Future {1}.exists(_ > 0)
    val failure = Future {-1}.exists(_ > 0)
    val exception = Future {1 / 0}.exists(_ > 0)

    println(success.value)
    println(failure.value)
    println(exception.value)
  }
}


////zadanie 3
//// niedokończone
//
//object WordCount {
//  def main(args: Array[String]) {
//    val path = "words/"
//    val promiseOfFinalResult = Promise[Seq[(String, Int)]]
//    // Tu oblicz promiseOfFinalResult
//    promiseOfFinalResult.future onComplete {
//      case Success(result) => result foreach println
//      case Failure(t) => t.printStackTrace
//    }
//
//
//    Thread.sleep(5000)
//  }
//
//
//  // Oblicza liczbę słów w każdym pliku z sekwencji wejściowej
//  private def processFiles(fileNames: Seq[String]): Future[Seq[(String, Int)]] = {
//    Future.sequence(fileNames.map(name => processFile(name)))
//  }
//  // Wskazówka. Wykorzystaj Future.sequence(futures)
//
//  // Oblicza liczbę słów w podanym pliku i zwraca parę: (nazwa pliku, liczba słów)
//  private def processFile(fileName: String): Future[(String, Int)] = {
//    val wordCount = scala.io.Source.fromFile(fileName).getLines().flatMap(_.split(" ")).size
//    Future { (fileName, wordCount) }
//  }
//
//  // Zwraca sekwencję nazw plików (w naszym przypadku Array[String])
//  private def scanFiles(docRoot: String): Future[Seq[String]] =
//    Future { new java.io.File(docRoot).list.map(docRoot + _) }
//}
