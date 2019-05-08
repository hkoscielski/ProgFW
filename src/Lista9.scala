//Hubert Kościelski
import java.util.concurrent.Semaphore

object Zad1 extends App {
	var counter = 0 // counter variable

	def readWriteCounter(): Unit = {
		val incrementedCounter = counter + 1 // reading counter
		counter = incrementedCounter // writing to counter
		// counter += 1 // shorter code
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

object Zad1b extends App {
	var counter = 0 // counter variable

	def readWriteCounter(): Unit = this.synchronized {
		val incrementedCounter = counter + 1 // reading counter
		counter = incrementedCounter // writing to counter
		// counter += 1 // shorter code
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

object Zad1c extends App {
	var counter = 0 // counter variable
	private val semaphore = new Semaphore(1)

	def readWriteCounter(): Unit = {
		semaphore.acquire
		val incrementedCounter = counter + 1 // reading counter
		counter = incrementedCounter // writing to counter
		// counter += 1 // shorter code
		semaphore.release
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

object Zad2 extends App {

	def parallel[A, B](block1: => A, block2: => B): (A, B) = {
//		var result1: A = null.asInstanceOf[A]
//		var result2: B = null.asInstanceOf[B]
		var result1: Option[A] = None
		var result2: Option[B] = None

		val thread1 = new Thread(() => result1 = Some(block1))
		val thread2 = new Thread(() => result2 = Some(block2))

		thread1.start; thread2.start;
		thread1.join; thread2.join;

		(result1.get, result2.get)
	}

	println(parallel("a"+1, "b"+2))
	println(parallel(Thread.currentThread.getName, Thread.currentThread.getName))
	println(parallel({ var x = 0; while (x < 200000) x += 1; x }, { var x = 0; while (x < 400000) x += 1; x }))
}

object Zad3 extends App {

	def periodically(duration: Long, times: Int)(block: => Unit): Unit = {
		val thread = new Thread(() =>
			for (_ <- 0 until times) {
				block
				Thread.sleep(duration)
			})
		thread.setDaemon(true)
		thread.start
	}

	periodically(1000, 5){print("y ")}
	periodically(1000, 25){print("x ")}
	Thread.sleep(10000)
	println("Done sleeping")
}
