//Hubert Kościelski
import reflect.ClassTag

//Zadanie 1
class FullException(msg: String) extends Exception(msg)

abstract class MyQueue[E] {
	@throws[FullException]
	def enqueue(x: E): Unit
	def dequeue: Unit
	@throws[NoSuchElementException]
	def first: E
	def isEmpty: Boolean
	def isFull: Boolean
}

class QueueMut[E: ClassTag](val capacity: Int = 1000) extends MyQueue[E] {

	private[this] val queue: Array[E] = new Array[E](capacity + 1)
	private[this] var f: Int = 0
	private[this] var r: Int = 0

	override def enqueue(x: E): Unit =
		if (isFull) throw new FullException("Queue is full!")
		else {
			queue(r) = x
			r = (r + 1) % queue.length
		}

	override def dequeue: Unit = if (!isEmpty) f = (f + 1) % queue.length

	override def first: E =
		if (f == r) throw new NoSuchElementException("Queue is empty!")
		else queue(f)

	override def isEmpty: Boolean = f == r

	override def isFull: Boolean = f == (r + 1) % queue.length
}

object QueueMut {
	def apply[E: ClassTag](xs: E*): QueueMut[E] = {
		val queue = empty[E](xs.size)
		xs.foreach(queue.enqueue)
		queue
	}
	def empty[E: ClassTag](capacity: Int = 1000) : QueueMut[E] = new QueueMut(capacity)
}

object Lista8 {
	def main(args: Array[String]): Unit = {
		val smallQueue = new QueueMut[Int](3)
		println(smallQueue.isEmpty)
		try {
			smallQueue.first
		} catch {
			case e: NoSuchElementException => println(e.getMessage == "Queue is empty!")
		}
		println(!smallQueue.isFull)
		smallQueue.enqueue(1)
		println(!smallQueue.isEmpty)
		smallQueue.dequeue
		println(smallQueue.isEmpty)
		smallQueue.enqueue(1)
		smallQueue.enqueue(2)
		smallQueue.enqueue(3)
		println(smallQueue.isFull)
		try {
			smallQueue.enqueue(4)
		} catch {
			case e: FullException => println(e.getMessage == "Queue is full!")
		}
		smallQueue.dequeue
		smallQueue.enqueue(5)
		println(smallQueue.first == 2)

		val queue2 = QueueMut[String]("A", "B", "C")
		println(queue2.isFull)
		println(queue2.capacity == 3)
	}
}
