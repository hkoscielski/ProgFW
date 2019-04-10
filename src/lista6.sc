//Hubert Kościelski

//Zadanie 1
def whileLoop(condition: => Boolean)(expr: => Unit): Unit =
	if (condition) {
		expr
		whileLoop(condition)(expr)
	}

var count = 0
whileLoop (count < 5) {
	println(count)
	count += 1
}

//Zadanie 2
def lrepeat[A](k: Int)(stream: Stream[A]): Stream[A] = {
	def lrepeatInternal(i: Int, stream: Stream[A]): Stream[A] =
		stream match {
			case h #:: t =>
				if (i > 0) h #:: lrepeatInternal(i-1, stream)
				else lrepeat(k)(t)
			case Stream.Empty => Stream.Empty
		}

	lrepeatInternal(k, stream)
}

(lrepeat(3)(Stream.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
lrepeat(4)(Stream(1, 2, 3)).toList == List(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)
lrepeat(0)(Stream(1, 2, 3)).toList == Nil
lrepeat(-4)(Stream(1, 2, 3)).toList == Nil
lrepeat(4)(Stream.Empty).toList == Nil

//Zadanie 3
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

//3a
def lBreadth[A](ltree: lBT[A]): Stream[A] = {
	def lBreadthInternal(xs: List[lBT[A]]): Stream[A] =
		xs match {
			case LNode(elem, left, right) :: t => Stream.cons(elem, lBreadthInternal(t ::: left() :: right() :: Nil))
			case LEmpty :: t => lBreadthInternal(t)
			case Nil => Stream.Empty
		}

	lBreadthInternal(List(ltree))
}

//3b
def lTree(n: Int): lBT[Int] =
	LNode(n, () => lTree(2*n), () => lTree(2*n+1))

val notEmptyTree = LNode(1, ()=>LNode(2, ()=>LEmpty, ()=>LNode(3, ()=>LEmpty, ()=>LEmpty)), ()=>LEmpty)
val emptyTree = LEmpty

lBreadth(emptyTree).take(10).toList == Nil
lBreadth(notEmptyTree).take(10).toList == List(1, 2, 3)
lBreadth(lTree(1)).take(10).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)