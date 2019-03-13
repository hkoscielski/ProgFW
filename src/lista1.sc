//Hubert Kościelski
import java.util.NoSuchElementException

//Zadanie 1
def suma(xs: List[Double]): Double =
	if (xs == Nil) 0.0
	else xs.head + suma(xs.tail)

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6

//Zadanie 2
def ends[A](xs: List[A]): (A,A) =
	if (xs == Nil) throw new NoSuchElementException("Empty list")
	else if (xs.tail == Nil) (xs.head, xs.head)
	else (xs.head, ends(xs.tail)._2)

ends(List(1,2,3,5)) == (1,5)
ends(List(1)) == (1,1)
ends(Nil)

//Zadanie 3
def posortowana(xs: List[Int]): Boolean =
	xs == Nil || xs.tail == Nil ||
		xs.head <= xs.tail.head && posortowana(xs.tail)

posortowana(Nil)
posortowana(List(1))
posortowana(List(1,1,1,1,1,1))
posortowana(List(1,3,3,5,6,7))
!posortowana(List(7,6,5,3,3,1))

//Zadanie 4
val glue: (List[String], String) => String = (xs, sep) =>
	if (xs == Nil) sep
	else if (xs.tail == Nil) xs.head
	else xs.head + sep + glue(xs.tail, sep)

glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
glue(List("To"), "-") == "To"
glue(Nil, "-") == "-"