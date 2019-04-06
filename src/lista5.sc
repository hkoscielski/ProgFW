//Hubert Kościelski

//Zadanie 1
class Pair[A, B](var fst: A, var snd: B) {
	override def toString: String = "(" + fst + ", " + snd + ")"
}

val pair = new Pair[Int, String](1, "Napis")
pair.fst
pair.snd
pair.fst = 2
pair.snd = "Inny napis"
pair.toString

//Zadanie 2
class BankAccount(initialBalance : Double) {
	private var balance = initialBalance
	def checkBalance = balance
	def deposit(amount : Double) = { balance += amount; balance}
	def withdraw(amount : Double) = { balance -= amount; balance}
}

//2a
class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
	override def deposit(amount: Double): Double = super.deposit(amount - 1)
	override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
}

val checkingAccount = new CheckingAccount(10)
checkingAccount.deposit(5) == 14
checkingAccount.withdraw(5) == 8
checkingAccount.withdraw(4) == 3

//2b
class SavingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
	private[this] var transactionsCount: Int = 0
	private[this] val freeTransactionsLimit: Int = 3
	private[this] val interestRate: Double = 0.0002

	override def deposit(amount: Double): Double = {
		transactionsCount += 1
		super.deposit(if (transactionsCount <= freeTransactionsLimit) amount else amount - 1)
	}

	override def withdraw(amount: Double): Double = {
		transactionsCount += 1
		super.withdraw(if (transactionsCount <= freeTransactionsLimit) amount else amount + 1)
	}

	def earnMonthlyInterest: Double = {
		transactionsCount = 0
		super.deposit(super.checkBalance * interestRate)
	}
}

val savingAccount = new SavingAccount(10000)
savingAccount.deposit(1) == 10001
savingAccount.withdraw(1) == 10000
savingAccount.deposit(2) == 10002
savingAccount.withdraw(1) == 10000
savingAccount.earnMonthlyInterest == 10002
savingAccount.withdraw(1) == 10001

//Zadanie 3
//3a
abstract class Zwierz(val imie: String = "bez imienia") {
	def dajGlos: String
	def rodzaj: String = getClass.getName

	override def toString: String = rodzaj + " " + imie + " daje glos " + dajGlos
}

//3b
class Pies(imie: String = "bez imienia") extends Zwierz(imie) {
	override def dajGlos = "hau hau"
}

class Kot(imie: String = "bez imienia") extends Zwierz(imie) {
	override def dajGlos = "miau miau"
}

class Krowa(imie: String = "bez imienia") extends Zwierz(imie) {
	override def dajGlos = "mu mu"
}

object TestZwierza {
	def main(args: Array[String]): Unit = {
		val zwierzeta: Vector[Zwierz] = Vector(new Pies("Azor"), new Kot("Mruczek"), new Krowa)

		for (z <- zwierzeta) println(z)
	}
}
TestZwierza.main(Array())