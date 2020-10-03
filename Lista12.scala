//Justyna Zieniewicz

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.util.Random

object Guess {

  class Server(N: Integer) extends Actor {
    private val randomNumber = Random.nextInt(N + 1)
    println(s"Guess my number from the interval [0..$N]")

    override def receive = {
      case Server.M(guess) =>
        if (guess == randomNumber) sender ! Client.R("equal")
        if (guess > randomNumber) sender ! Client.R("too big")
        if (guess < randomNumber) sender ! Client.R("too small")
      case x => throw new Exception(s"Server: invalid message: $x")
    }
  }

  object Server {
    private def props = Props[Server]
    case class M(guess: Int)
  }

  class Client(private val name: String, private val server: ActorRef, private[this] var end: Int) extends Actor {
    private var start = 0
    private var guess = Random.nextInt(end + 1)
    Thread.sleep(1000)
    println(s"$name starting")
    println(s"$name trying: $guess")

    override def receive = {
      case Client.Start => server ! Server.M(guess)
      case Client.R(msg) => msg match {
        case "equal" =>
          println(s"$name: I guessed it! $guess")
          println(s"Goodbye! from $name")
          context.system.terminate
        case "too big" =>
          end = guess
          guess = (start + end) / 2
          println(s"$name. Response: too big. I'm trying: $guess")
          sender ! Server.M(guess)
        case "too small" =>
          start = guess
          guess = (start + end) / 2
          println(s"$name. Response: too small. I'm trying: $guess")
          sender ! Server.M(guess)
      }
      case x => throw new Exception(s"Client: invalid message: $x")
    }
  }

  object Client {
    def props = Props(classOf[Client], ourSystem.actorOf(Props(classOf[Server], upperBound)))
    case class R(response: String)
    case object Start
  }

  val upperBound = 100
  val ourSystem = ActorSystem("MySystem")
  val server: ActorRef = ourSystem.actorOf(Props(classOf[Server], upperBound))

  def main(args: Array[String]): Unit = {

    val clientCount = 2
    val clientArray = new Array[ActorRef](clientCount)

    for (i <- 0 until clientCount)
      clientArray(i) = ourSystem.actorOf(Props(classOf[Client], s"Client$i", server, upperBound))

    clientArray.foreach(_ ! Client.Start)
  }
}