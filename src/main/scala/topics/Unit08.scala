package topics

import lib.logic.L3.*
import lib.logic.L3Parser.*
import lib.parser.Parser.*
import lib.parser.ParserLib.*

object Unit08 {
  /*
   * Exercise 81
  */
  val p81a: Parser[Char] = char('{') >> getc >>= (c => char('}') >> result(c))
  val p81b: Parser[Char] = for _ <- char('{'); c <- getc; _ <- char('}') yield c
  val p81c: Parser[String] = for _ <- char('<'); s <- alpha.some; _ <- char('>') yield s.mkString
  val p81d: Parser[String] =
    for _ <- char('<')
        tag <- alpha.some
        _ <- char('>')
        body <- sat(_!='<').many
        _ <- string("</")
        _ <- string(tag.mkString)
        _ <- char('>')
    yield body.mkString

  @main def exercise81(): Unit =
    println(p81a.parse("{a}"))
    println(p81b.parse("{b}"))
    println(p81c.parse("<HTML>"))
    println(p81d.parse("<TD>An item</TD>"))
    println(p81d.parse("<TD></TD>"))
    println(p81d.parse("<TD>Mismatched</TR>"))




  /*
   * Exercise 82
  */

  def bit: Parser[Int] = (sat(_ == '0') >> result(0)) <+> (sat(_ == '1') >> result(1))
  def bits: Parser[Seq[Int]] = bit.some.sp
  def binary: Parser[Int] = bits.map(_.reduceLeft(_*2+_))

  @main def exercise82(): Unit =
    println(binary.parse(""))
    println(binary.parse("0"))
    println(binary.parse("1"))
    println(binary.parse("11111111"))
    println(binary.parse("1111111111111111"))
    println(binary.parse("111111111111111111111111"))
    println(binary.parse("11111111111111111111111111111111"))
    println(binary.parse("10000000000000000000000000000000"))
    println(binary.parse("01111111111111111111111111111111"))

  /*
   * Exercise 83
  */
  @main def exercise83(): Unit =
    val f: Formula = formula.parse("A & B").get
    val env: Map[TruthVar, TruthVal] = Map(A -> U, B -> F)
    println(f)
    println(f.showTruthTable)
    println(s"$f.evaluate($env) = ${f.evaluate(env)}")

    val f1: Formula = formula.parse("A & B <-> B & A").get
    val env1: Map[TruthVar, TruthVal] = Map(A -> F, B -> T)
    println(f1)
    println(f1.showTruthTable)
    println(s"$f1.evaluate($env1) = ${f1.evaluate(env1)}")

    val f2: Formula = formula.parse("A & (B + C) <-> A & B + A & C").get
    val env2: Map[TruthVar, TruthVal] = Map(A -> U, B -> T, C -> F)
    println(f2)
    println(f2.showTruthTable)
    println(s"$f2.evaluate($env2) = ${f2.evaluate(env2)}")

    val f3: Formula = formula.parse("A -> (~A -> A)").get
    val env3: Map[TruthVar, TruthVal] = Map()
    println(f3)
    println(f3.showTruthTable)
    println(s"$f3.evaluate($env3) = ${f3.evaluate(env3)}")

    val f4: Formula = formula.parse("A + ~A").get
    val env4: Map[TruthVar, TruthVal] = Map(A -> U)
    println(f4)
    println(f4.showTruthTable)
    println(s"$f4.evaluate($env4) = ${f4.evaluate(env4)}")

    val f5: Formula = formula.parse("~L~A -> U").get
    val env5: Map[TruthVar, TruthVal] = Map(A -> T)
    println(f5)
    println(f5.showTruthTable)
    println(s"$f5.evaluate($env5) = ${f5.evaluate(env5)}")

    val f6: Formula = formula.parse("A -> B <-> ~B -> ~A").get
    val env6: Map[TruthVar, TruthVal] = Map(A -> F)
    println(f6)
    println(f6.showTruthTable)
    println(s"$f6.evaluate($env6) = ${f6.evaluate(env6)}")




  /*
   * Exercise 84
  */

  case class Money(dollars: Int, cents: Int) {

    override def toString: String = f"$$$dollars.$cents%02d"

    def +(that: Money): Money =
      val c = this.cents + that.cents
      Money(this.dollars + that.dollars + c/100, c%100)

    def -(that: Money): Money =
      val c = this.dollars*100 + this.cents - (that.dollars*100 + that.cents)
      Money(c / 100, c % 100)

    def *(n: Int): Money =
      val result = (dollars*100+cents)*n
      Money(result / 100, result % 100)
  }

  def money: Parser[Money] =
    for _ <- char('$').sp
       d <- nat.sp
       _ <- char('.').sp
       c <- nat.sp
    yield Money(d, c)

  def sums: Parser[Money] = money.sp.someWith(char('+').sp).map(_.reduce(_+_))

  def term: Parser[(Money, Int)] =
      (for m <- money.sp; _ <- char('*').sp; n <- nat.sp yield (m,n)) <+> money.sp.map((_, 1))

  def sumz: Parser[Money] =
    term.sp.someWith(char('+').sp).map(_.map(_*_).reduce(_+_))

  def addOrSub: Parser[Money => Money] =
    val plus: (Money, Money) => Money = _+_
    val minus: (Money, Money) => Money = _-_
    for
      op <- (char('+').sp >> result(plus)) <|> (char('-').sp >> result(minus))
      m <- money
    yield
      op(_, m)

  def sumzz: Parser[Money] =
    for
      m <- money
      as <- addOrSub.many
    yield
      as.foldLeft(m)((v, f) => f(v))


  @main def exercise84(): Unit =
    println(money.parse("$6.50").get + money.parse("$3.50").get)
    println(sums.parse("$1.10 + $2.22 + $3.33 + $4.44").get)
    println(sumz.parse("$1.10 * 2 + $2.22 + $3.33 * 3 + $4.44").get)
    println(sumz.parse("$1.10 * 10").get)
    println(sumzz.parse("$1.10").get)
    println(sumzz.parse("$1.10 - $0.10").get)
    println(sumzz.parse("$1.10 - $0.10 - $0.60").get)
    println(sumzz.parse("$1.10 - $0.10 - $0.60 - $0.20 + $10.00").get)


}
