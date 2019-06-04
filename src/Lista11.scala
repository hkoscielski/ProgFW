//Hubert Kościelski
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

object L11_Zad1 extends App {
	def pairFutA[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] = fut1 zip fut2

	def pairFutB[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
		for {
			x <- fut1
			y <- fut2
		} yield (x, y)

	val fut1 = Future {
		Thread.sleep(5000); 18 / 3
	}
	val fut2 = Future {
		"Napis"
	}
	val fut3 = Future {
		var i = 0; while (i < 10000) i += 1; i
	}
	val fut4 = Future {
		Thread.sleep(3000); "Kolejny napis"
	}

	val pairFut1 = pairFutA(fut1, fut2)
	val pairFut2 = pairFutB(fut3, fut4)

	println(s"pairFut1 completed: ${pairFut1.isCompleted}")
	println(s"pairFut1 value: ${pairFut1.value}")
	println(s"pairFut2 completed: ${pairFut2.isCompleted}")
	println(s"pairFut2 value: ${pairFut2.value}")

	Thread.sleep(7000)

	println(s"pairFut1 completed: ${pairFut1.isCompleted}")
	println(s"pairFut1 value: ${pairFut1.value}")
	println(s"pairFut2 completed: ${pairFut2.isCompleted}")
	println(s"pairFut2 value: ${pairFut2.value}")
}

object L11_Zad2a extends App {

	implicit class FutureOps[T](val self: Future[T]) {
		def exists(p: T => Boolean): Future[Boolean] = {
			val promise = Promise[Boolean]
			self onComplete {
				case Success(value) => promise trySuccess p(value)
				case Failure(_) => promise trySuccess false
			}
			promise.future
		}
	}

	val fut1 = Future {
		Thread.sleep(2000); 2 + 2
	} exists (v => v < 5)
	val res1 = Await.result(fut1, 3.seconds)
	println(s"Future 1 result = $res1")

	val fut2 = Future {
		Thread.sleep(1500); 18 - 16
	} exists (v => v > 5)
	val res2 = Await.result(fut2, 3.seconds)
	println(s"Future 2 result = $res2")

	val fut3 = Future {
		Thread.sleep(2500); 18 / 0
	} exists (v => v > 5)
	val res3 = Await.result(fut3, 3.seconds)
	println(s"Future 3 result = $res3")
}

object L11_Zad2b extends App {

	implicit class FutureOps[T](val self: Future[T]) {
		def exists(p: T => Boolean): Future[Boolean] = self map (x => p(x)) recover { case _ => false }
	}

	val fut1 = Future {
		Thread.sleep(2000); 2 + 2
	} exists (v => v < 5)
	val res1 = Await.result(fut1, 3.seconds)
	println(s"Future 1 result = $res1")

	val fut2 = Future {
		Thread.sleep(1500); 18 - 16
	} exists (v => v > 5)
	val res2 = Await.result(fut2, 3.seconds)
	println(s"Future 2 result = $res2")

	val fut3 = Future {
		Thread.sleep(2500); 18 / 0
	} exists (v => v > 5)
	val res3 = Await.result(fut3, 3.seconds)
	println(s"Future 3 result = $res3")
}