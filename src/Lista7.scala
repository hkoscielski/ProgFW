//Hubert Kościelski

//Zadanie 1
class MyQueue[+A] private (private[this] val out: List[A], private[this] val in: List[A]) {

	def this() = this(Nil, Nil)

	def dequeue =
		out match {
			case _ :: Nil => new MyQueue(in.reverse, Nil)
			case _ :: t => new MyQueue(t, in)
			case Nil => this
		}

	def enqueue[B >: A](elem: B) =
		out match {
			case Nil => new MyQueue(elem :: Nil, Nil)
			case _ => new MyQueue(out, elem :: in)
		}

	def first =
		out match {
			case h::_ => h
			case _ => throw new NoSuchElementException("Queue is empty!")
		}

	def firstOption: Option[A] =
		out match {
			case h::_ => Some(h)
			case _ => None
		}

	def isEmpty = out == Nil
}

object MyQueue {
	def apply[A](in: A*) = new MyQueue[A](in.toList, Nil)
	def empty[A] = new MyQueue[A](Nil, Nil)
}

//Zadanie 2
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

object Tree {
	def breadthBT[A](tree: BT[A]): List[A] = {
		def breadthBTInternal(q: MyQueue[BT[A]]): List[A] =
			q.firstOption match {
				case Some(Node(elem, left, right)) => elem :: breadthBTInternal(q.dequeue.enqueue(left).enqueue(right))
				case Some(Empty) => breadthBTInternal(q.dequeue)
				case None => Nil
			}

		breadthBTInternal(MyQueue(tree))
	}
}

object Lista7 {
	def main(args: Array[String]): Unit = {
		val queue = new MyQueue[Int]
		println(queue.isEmpty)
		try {
			queue.first
		} catch {
			case e: NoSuchElementException => println(e.getMessage == "Queue is empty!")
		}
		println(queue.firstOption.isEmpty)
		println(queue.dequeue == queue)

		val queue2 = MyQueue.empty
		println(queue2.isEmpty)
		try {
			queue2.first
		} catch {
			case e: NoSuchElementException => println(e.getMessage == "Queue is empty!")
		}
		println(queue2.firstOption.isEmpty)
		println(queue2.dequeue == queue2)
		println(!queue2.enqueue(1).isEmpty)
		println(queue2.dequeue.isEmpty)

		val queue3 = MyQueue(1, 2, 3)
		println(!queue3.isEmpty)
		println(queue3.first == 1)
		println(queue3.firstOption.contains(1))
		println(queue3.dequeue.firstOption.contains(2))

		val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
		val tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty))

		println(Tree.breadthBT(t) == List(1, 2, 3))
		println(Tree.breadthBT(tt) == List(1, 2, 3, 4, 5, 6))
		println(Tree.breadthBT(Empty) == Nil)
	}
}
