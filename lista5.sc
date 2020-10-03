//Justyna Zieniewicz


//zadanie 1
class MyPair[A, B](var fst: A, var snd: B) {
  override def toString: String = s"($fst, $snd)"
}

val pair: MyPair[Int, Int] = new MyPair(1, 2)
pair.toString
pair.fst == 1
pair.snd == 2
pair.fst = -1
pair.snd = -2
pair.toString == "(-1, -2)"
var pair2: MyPair[String, String] = new MyPair("Moja", "Para")
pair2.toString == "(Moja, Para)"
pair2.fst == "Moja"
pair2.snd == "Para"


//zadanie 2
class BankAccount(initialBalance : Double) {
  private[this] var balance = initialBalance
  def checkBalance = balance
  def deposit(amount : Double) = { balance += amount; balance}
  def withdraw(amount : Double) = { balance -= amount; balance}
}

//a)
class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance){
  override def deposit(amount: Double) = super.deposit(amount - 1)
  override def withdraw(amount: Double) = super.withdraw(amount + 1)
}

val myAccount = new CheckingAccount(5)
myAccount.deposit(10) == 14
myAccount.checkBalance == 14
myAccount.withdraw(4) == 9
myAccount.checkBalance == 9


//b)
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance){
  private[this] var transactions: Int = 0
  override def deposit(amount: Double) = {
    transactions += 1
    super.deposit(if (transactions > 3) amount - 1 else amount)
  }
  override def withdraw(amount: Double) = {
    transactions += 1
    super.withdraw(if (transactions > 3) amount + 1 else amount)
  }
  def earnMonthlyInterest() = {
    super.deposit(super.checkBalance * 0.01)
    transactions = 0
  }
}

val savingsAccount = new SavingsAccount(10)
savingsAccount.deposit(5) == 15 //transactions == 1
savingsAccount.withdraw(5) == 10 //transactions ==2
savingsAccount.deposit(5) == 15 //transactions == 3
savingsAccount.withdraw(5) == 9 //transactions ==4
savingsAccount.checkBalance == 9
savingsAccount.earnMonthlyInterest()
savingsAccount.checkBalance == 9.09

//zadanie 3
//a)
abstract class Zwierz(imie: String = "bez imienia"){
  private[this] val imieZwierza: String = imie
  def rodzaj: String
  def dajGlos(): String
  override def toString: String = s"$rodzaj $imieZwierza daje glos $dajGlos!"
}

val pies = new Zwierz("Kruczek") {
  override def rodzaj: String = "pies"
  override def dajGlos: String = "Hau, hau"
}
pies.toString == "pies Kruczek daje glos Hau, hau!"

val krowa = new Zwierz() {
  override def rodzaj: String = "krowa"
  override def dajGlos(): String = "Muuu"
}
krowa.toString == "krowa bez imienia daje glos Muuu!"

//b)
class Malpa(imie: String = "bezimienna") extends Zwierz(imie){
  override def rodzaj: String = "malpa"
  override def dajGlos(): String = "Uaaa"
}

val malpa = new Malpa()
malpa.toString == "malpa bezimienna daje glos Uaaa!"

class Kot(imie: String = "bezimienny") extends Zwierz(imie){
  override def rodzaj: String = "kot"
  override def dajGlos(): String = "Miau, miau"
}

val kot = new Kot("Dominik")
kot.toString == "kot Dominik daje glos Miau, miau!"

//c)
object TestZwierza {
  def main(args: Array[String]): Unit = {
    val vector = Vector(
      new Kot("Maciek"),
      new Malpa(),
      new Kot(),
      new Malpa("Justyna"))

    for (zwierz <- vector) println(zwierz.toString())
  }
}

TestZwierza.main(Array())