//Hubert Kościelski
import scala.annotation.tailrec

//Zadanie 1
def existsA[A](xs: List[A])(p: A => Boolean): Boolean =
	xs match {
		case h::t => p(h) || existsA(t)(p)
		case Nil => false
	}

existsA(List(5,1,2,3))(_ == 2)
existsA(List(5,1,2,3))(_ == 2)
!existsA(List(5,1,2,3))(_ == 10)
!existsA(List("Ala","ma","kota"))(_ == "kot")
!existsA(Nil)(_ == 2)

def existsB[A](xs: List[A])(p: A => Boolean): Boolean =
	//xs.foldLeft(false)((acc, elem) => acc || p(elem))
	xs.foldLeft(false)(_ || p(_))

existsB(List(5,1,2,3))(_ == 2)
existsB(List(5,1,2,3))(_ == 2)
!existsB(List(5,1,2,3))(_ == 10)
!existsB(List("Ala","ma","kota"))(_ == "kot")
!existsB(Nil)(_ == 2)

def existsC[A](xs: List[A])(p: A => Boolean): Boolean =
	//xs.foldRight(false)((elem, acc) => acc || p(elem))
	xs.foldRight(false)(p(_) || _)

existsC(List(5,1,2,3))(_ == 2)
existsC(List(5,1,2,3))(_ == 2)
!existsC(List(5,1,2,3))(_ == 10)
!existsC(List("Ala","ma","kota"))(_ == "kot")
!existsC(Nil)(_ == 2)

//Zadanie 2
def filter[A](xs: List[A])(p: A => Boolean): List[A] =
	xs.foldRight(List[A]())((elem, acc) => if (p(elem)) elem::filter(acc)(p) else filter(acc)(p))

filter(List(2,7,1,3,7,8,4,1,6,9))(_ > 3) == List(7,7,8,4,6,9)
filter(List(2,1,3))(_ > 3) == Nil
filter(List("Ala","ma","kota"))(_ == "kota") == List("kota")
filter(Nil: List[Int])(_ > 3) == Nil

//Zadanie 3
def remove1A[A](xs: List[A])(p: A => Boolean): List[A] =
	xs match {
		case h::t => if (p(h)) t else h::remove1A(t)(p)
		case Nil => Nil
	}

remove1A(List(1,2,3,2,5)) (_ == 2) == List(1,3,2,5)
remove1A(List(1,2,3,2,5)) (_ == 10) == List(1,2,3,2,5)
remove1A(List("Ala","ma","kota","Ala")) (_ == "Ala") == List("ma","kota","Ala")
remove1A(Nil)(_ == 2) == Nil

def remove1B[A](xs: List[A])(p: A => Boolean): List[A] = {
	@tailrec
	def remove1Internal(xs: List[A], acc: List[A]): List[A] =
		xs match {
			case h::t => if (p(h)) t.reverse_:::(acc) else remove1Internal(t, h::acc)
			case Nil => acc.reverse
		}

	remove1Internal(xs, Nil)
}

remove1B(List(1))(_ == 1) == Nil
remove1B(List(1,2,3,2,5))(_ == 2) == List(1,3,2,5)
remove1B(List(1,2,3,2,5))(_ == 10) == List(1,2,3,2,5)
remove1B(List(1,2,3,2))(_ == 10) == List(1,2,3,2)
remove1B(List("Ala","ma","kota","Ala"))(_ == "Ala") == List("ma","kota","Ala")
remove1B(Nil)(_ == 2) == Nil

//Zadanie 4
def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) = {
	def splitAtInternal(n: Int, ls: List[A], rs: List[A]): (List[A], List[A]) =
		rs match {
			case h::t => if (n > 0) splitAtInternal(n-1, h::ls, t) else (ls.reverse, rs)
			case Nil => (ls.reverse, rs)
		}

	splitAtInternal(n, Nil, xs)
}

splitAt(List('a','b','c','d','e'))(2) == (List('a','b'), List('c','d','e'))
splitAt(List(1,2,3,4,5))(2) == (List(1,2), List(3,4,5))
splitAt(List('a','b','c','d','e'))(0) == (Nil, List('a','b','c','d','e'))
splitAt(List('a','b','c','d','e'))(-1) == (Nil, List('a','b','c','d','e'))
splitAt(List('a','b','c','d','e'))(5) == (List('a','b','c','d','e'), Nil)
splitAt(List('a','b','c','d','e'))(6) == (List('a','b','c','d','e'), Nil)
splitAt(Nil)(6) == (Nil, Nil)