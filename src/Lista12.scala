//Hubert Kościelski
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.util.Random

//Zadanie 1
class Server(val n: Int) extends Actor {
	val r = new Random
	val randomNumber: Int = r.nextInt(n+1)
	println("[info] Running Guess")
	println(s"Guess my number from the interval [0..$n]")
	println(s"Debug ---- number to guess = $randomNumber")

	override def receive: Receive = {
		case Server.M(guessNumber) =>
			if (guessNumber == randomNumber) sender ! Client.R("equal")
			else if (guessNumber > randomNumber) sender ! Client.R("too_big")
			else sender ! Client.R("too_small")
	}
}

object Server {
	case class M(guessNumber: Int)
}

//Zadanie 2
class Client(name: String, server: ActorRef, private[this] var upperBound: Int) extends Actor {
	private[this] var guessedNumber = 0
	private[this] var lowerBound = 0

	override def receive: Receive = {
		case Client.Start =>
			println(s"$name starting")
			val r = new Random
			guessedNumber = r.nextInt(upperBound + 1) + lowerBound
			println(s"$name trying: $guessedNumber")
			Thread.sleep(1000)
			server ! Server.M(guessedNumber)
		case Client.R(response) =>
			response match {
				case "too_big" =>
					upperBound = guessedNumber
					guessedNumber = (lowerBound + upperBound) / 2
					println(s"$name. Response: too big. I'm trying: $guessedNumber")
					Thread.sleep(500)
					server ! Server.M(guessedNumber)
				case "too_small" =>
					lowerBound = guessedNumber
					guessedNumber = (lowerBound + upperBound) / 2
					println(s"$name. Response: too small. I'm trying: $guessedNumber")
					Thread.sleep(500)
					server ! Server.M(guessedNumber)
				case "equal" =>
					println(s"$name. I guessed it! $guessedNumber")
					println(s"Goodbye! from $name")
					context.system.terminate
			}
	}
}

object Client {
	case class R(response: String)
	case object Start
}

//Zadanie 3
object Lista12 extends App {
	val ourSystem = ActorSystem("MySystem")
	val MAX_NUMBER_TO_GUESS = 200
	val CLIENTS_NUMBER = 2

	val server = ourSystem.actorOf(Props(new Server(MAX_NUMBER_TO_GUESS)))

	for (i <- 0 until CLIENTS_NUMBER)
		ourSystem.actorOf(Props(new Client(s"Client${i+1}", server, MAX_NUMBER_TO_GUESS))) ! Client.Start
}
