//Hubert Kościelski
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]
val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
val tEmpty = Empty
val tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty))

//Zadanie 1
def sumBT(bt: BT[Int]): Int = {
	bt match {
		case Node(elem, tl, tr) => elem + sumBT(tl) + sumBT(tr)
		case Empty => 0
	}
}

sumBT(t) == 6
sumBT(tt) == 21
sumBT(tEmpty) == 0

//Zadanie 2
def foldBT[A,B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B =
	bt match {
		case Node(elem, tl, tr) => f(elem)(foldBT(f)(acc)(tl), foldBT(f)(acc)(tr))
		case Empty => acc
	}

//Zadanie 3
//a
def sumBTfold(bt: BT[Int]): Int =
	foldBT[Int, Int](elem => (leftAcc, rightAcc) => elem + leftAcc + rightAcc)(0)(bt)

sumBTfold(t) == 6
sumBTfold(tt) == 21
sumBTfold(tEmpty) == 0

//b
def inorderBTfold[A](bt: BT[A]): List[A] =
	foldBT[A, List[A]](elem => (leftAcc, rightAcc) => leftAcc ::: elem :: rightAcc)(Nil)(bt)

inorderBTfold(t) == List(2, 3, 1)
inorderBTfold(tt) == List(4, 2, 1, 5, 6, 3)
inorderBTfold(tEmpty) == Nil

//Zadanie 4
def mapBT[A,B](f: A => B)(tree: BT[A]): BT[B] =
	foldBT[A, BT[B]](elem => (leftAcc, rightAcc) => Node(f(elem), leftAcc, rightAcc))(Empty)(tree)

mapBT[Int, Int](elem => elem * elem)(t) == Node(1, Node(4, Empty, Node(9, Empty, Empty)), Empty)
mapBT[Int, Int](elem => elem * elem)(tt) == Node(1,Node(4,Node(16,Empty,Empty),Empty),Node(9,Node(25,Empty,Node(36,Empty,Empty)),Empty))
mapBT[Int, Int](elem => elem * elem)(tEmpty) == Empty

//Zadanie 5
sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]
val g = Graph((i: Int) =>
	i match {
		case 0 => List(3)
		case 1 => List(0,2,4)
		case 2 => List(1)
		case 3 => Nil
		case 4 => List(0,2)
		case n => throw	new NoSuchElementException("Graph g: node " + n	+ " doesn't exist")
	})

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
	def pathExistsInternal(visited: List[A], toVisit: List[A]): Boolean =
		toVisit match {
			case h::t => h == to || (
				if (visited contains h) pathExistsInternal(visited, t)
				else pathExistsInternal(h :: visited, t ::: (g succ h))
			)
			case Nil => false
		}

	pathExistsInternal(Nil, g succ from)
}

pathExists(g)(4,1)
pathExists(g)(2,2)
pathExists(g)(1,0)
pathExists(g)(0,3)
!pathExists(g)(0,0)
!pathExists(g)(0,1)
!pathExists(g)(0,2)
!pathExists(g)(0,4)
!pathExists(g)(1,10)