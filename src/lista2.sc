//Hubert Kościelski
import scala.annotation.tailrec
import scala.math.abs

//Zadanie 1
def take[A](n: Int, xs: List[A]): List[A] =
	xs match {
		case h::t => if (n > 0) h::take(n-1, t) else Nil
		case Nil => Nil
	}

take(2, Nil) == Nil
take(0, List(1,2,3,5,6)) == Nil
take(-2, List(1,2,3,5,6)) == Nil
take(2, List(1,2,3,5,6)) == List(1,2)
take(6, List(1,2,3,5,6)) == List(1,2,3,5,6)

//Zadanie 2
def drop[A](n: Int, xs: List[A]): List[A] =
	xs match {
		case _::t => if (n > 0) drop(n-1, t) else xs
		case Nil => Nil
	}

drop(2, Nil) == Nil
drop(8, List(1,2,3,5,6)) == Nil
drop(0, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(2, List(1,2,3,5,6)) == List(3,5,6)

//Zadanie 3
def reverse[A](xs: List[A]): List[A] = {
	@tailrec
	def reverse(xs: List[A], acc: List[A]): List[A] =
		xs match {
			case h::t => reverse(t, h::acc)
			case Nil => acc
		}
	reverse(xs, Nil)
}


reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(Nil) == Nil
reverse(List("Ala")) == List("Ala")
reverse(List("Ala", "Ala", "Ala")) == List("Ala", "Ala", "Ala")

//Zadanie 4
def replicate(xs: List[Int]): List[Int] = {
	def replicateInternal(n: Int, elem: Int, xs: List[Int]): List[Int] =
		xs match {
			case _ if n > 0 => elem::replicateInternal(n-1, elem, xs)
			case h::t => replicateInternal(h, h, t)
			case Nil =>	Nil
		}
	replicateInternal(0, 0, xs)
}

replicate(List(1,0,4,-2,3)) == List(1,4,4,4,4,3,3,3)
replicate(Nil) == Nil
replicate(List(0,-2,-3)) == Nil

//Zadanie 5
def root3(a: Double): Double = {
	@tailrec
	def root3internal(x: Double): Double =
		if (abs(x*x*x - a) > 10e-15 * abs(a)) root3internal(x + (a / (x*x) - x)/3)
		else x

	root3internal(if (a < 1) a/3 else a)
}

root3(-8.0) == -2.0
root3(8.0) == 2.0