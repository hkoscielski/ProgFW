//Hubert Kościelski
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, Semaphore, ThreadLocalRandom}
import scala.concurrent.ExecutionContext

class Producer(name: String, buf: BlockingQueue[Int]) extends Thread(name) {
	override def run: Unit =
		for (i <- 1 to 10) { println(s"$getName producing $i"); buf.put(i) }
}

class Consumer(name: String, buf: BlockingQueue[Int]) extends Thread(name) {
	override def run =
		for (_ <- 1 to 10) println(s"$getName consumed ${buf.take}")
}

object L10_Zad1a extends App {
	val buf: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)

	new Producer("Producer", buf).start
	new Consumer("Consumer", buf).start
}

object L10_Zad1b extends App {
	val buf: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
	val producers = 2
	val consumers = 3

	for (i <- 1 to producers) new Producer(s"Producer$i", buf).start
	for (i <- 1 to consumers) new Consumer(s"Consumer$i", buf).start
}

object L10_Zad1c extends App {
	val buf: BlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
	val executor = ExecutionContext.global
	val producers = 2
	val consumers = 3

	for (i <- 1 to producers) executor.execute(() => for (j <- 1 to 10) { println(s"Producer$i producing $j"); buf.put(j) })
	for (i <- 1 to consumers) executor.execute(() => for (_ <- 1 to 10) { println(s"Consumer$i consumed ${buf.take}") })
	Thread.sleep(1000)
}

class Philosopher (seatNumber: Int, val leftChopstick: Chopstick, val rightChopstick: Chopstick, doorman: Semaphore) extends Thread {

	override def run(): Unit = {
		goToDiningRoom
	}

	private def goToDiningRoom: Unit = {
		println(s"Philosopher $seatNumber wants to enter the dining room")
		while (true) {
			doorman.acquire
			println(s"Philosopher $seatNumber take your seat at the table")
			leftChopstick.take
			println(s"Philosopher $seatNumber take left chopstick")
			rightChopstick.take
			println(s"Philosopher $seatNumber take right chopstick")
			println(s"Philosopher $seatNumber start to eat")
			val eatTime = ThreadLocalRandom.current.nextInt(5000)
			Thread.sleep(eatTime)
			println(s"Philosopher $seatNumber ate in $eatTime ms")
			leftChopstick.putBack
			println(s"Philosopher $seatNumber put back left chopstick")
			rightChopstick.putBack
			println(s"Philosopher $seatNumber put back right chopstick")
			goToMeditate
			doorman.release
		}
	}

	private def goToMeditate = {
		val meditationTime = ThreadLocalRandom.current.nextInt(3000)
		Thread.sleep(meditationTime)
		println(s"Philosopher $seatNumber meditated in $meditationTime ms")
		goToDiningRoom
	}
}

class Chopstick {
	private[this] val semaphore = new Semaphore(1)

	def take = semaphore.acquire
	def putBack = semaphore.release
}

object L10_Zad2 extends App {
	val philosophersCount = 5

	val doorman = new Semaphore(philosophersCount - 1)
	val chopsticks = new Array[Chopstick](philosophersCount)
	val philosophers = new Array[Philosopher](philosophersCount)

	for (i <- 0 until philosophersCount) chopsticks(i) = new Chopstick
	for (i <- 0 until philosophersCount) {
		philosophers(i) = new Philosopher(i, chopsticks(i), chopsticks((i + 1) % philosophers.length), doorman)
		philosophers(i).start
	}
}
