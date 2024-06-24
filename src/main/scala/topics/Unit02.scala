package topics

import scala.annotation.tailrec

object Unit02 {
  /* Exercise 21 code */
  def sumTo(n: Int): Int =
    if n == 0 then
      0
    else
      n + sumTo(n - 1)

  def sumToTR(n: Int): Int =
    @tailrec
    def loop(k: Int, s: Int): Int =
      if k == 0 then
        s
      else
        loop(k - 1, s + k)

    loop(n, 0)

  @main def exerise21(): Unit =
    var n = 100000
    //println(sumTo(n))
    println(sumToTR(n))

  /* Exercise 22 code */
  /*
  * Return the largest number that divides exactly (i.e. with no remainder)
  * into both j and k.
  */
  def highestCommonFactor(j: Int, k: Int): Int =
    var x = j // a local mutable copy of j because j is immutable
    var y = k // a local mutable copy of k because k is immutable
    while x != y do
      if x > y then x = x - y
      if y > x then y = y - x
    x

  @tailrec
  def highestCommonFactorTR(j: Int, k: Int): Int =
    if j == k then
      j
    else if j > k then
      highestCommonFactorTR(j - k, k)
    else
      highestCommonFactorTR(j, k - j)

  @main def exerise22(): Unit =
    println(s"The hcf of 1 and 1 is ${highestCommonFactor(1, 1)}")
    println(s"The hcf of 3 and 9 is ${highestCommonFactor(3, 9)}")
    println(s"The hcf of 22 and 8 is ${highestCommonFactor(22, 8)}")
    println(s"The hcf of 36 and 84 is ${highestCommonFactor(36, 84)}")
    println(s"The hcf of 52 and 39 is ${highestCommonFactor(52, 39)}")
    println(s"The hcf of 3 and 9 is ${highestCommonFactorTR(3, 9)}")
    println(s"The hcf of 22 and 8 is ${highestCommonFactorTR(22, 8)}")
    println(s"The hcf of 36 and 84 is ${highestCommonFactorTR(36, 84)}")
    println(s"The hcf of 52 and 39 is ${highestCommonFactorTR(52, 39)}")

  /* Exercise 23 code */
  /*
  * N.B. This exercise uses a mutable data structure to perform an in situ sorting
  * algorithm. The exercise is primarily about converting a while loop into a recursive
  * method. When writing in a purely functional style using mutable data structures is
  * not recommended. In the sample solutions you see another version of the algorithm
  * that operates upon an immutable List data structure. You should compare all of the
  * sample answers to gain a fuller insight.
  */

  def findIndexOfSmallest(xs: scala.collection.mutable.ListBuffer[Int], from: Int): Int =
    val l = xs.length
    if from >= l then
      -1
    else
      var s = from
      var i = from + 1
      while i < l do
        if xs(i) < xs(s) then s = i
        i = i + 1
      s

  def selectionSort(xs: scala.collection.mutable.ListBuffer[Int]): Unit =
    if xs.nonEmpty then
      val l = xs.length
      var p = 0
      while p < l do
        val s = findIndexOfSmallest(xs, p)
        val t = xs(s)
        xs(s) = xs(p)
        xs(p) = t
        p = p + 1

  def findIndexOfSmallestTR(xs: scala.collection.mutable.ListBuffer[Int], from: Int): Int =
    val l = xs.length
    if from >= l then
      -1
    else
      @tailrec
      def loop(i: Int, s: Int): Int =
        if i < l then
          if xs(i) < xs(s) then
            loop(i + 1, i)
          else
            loop(i + 1, s)
        else
          s

      loop(from + 1, from)

  def selectionSortTR(xs: scala.collection.mutable.ListBuffer[Int]): Unit =
    if xs.nonEmpty then
      val l = xs.length

      @tailrec
      def loop(p: Int): Unit =
        if p < l then
          val s = findIndexOfSmallestTR(xs, p)
          val t = xs(s)
          xs(s) = xs(p)
          xs(p) = t
          loop(p + 1)

      loop(0)

  /*
  * For comparison, here is a realisation of the selection sort algorithm written
  * for an immutable list.
  */
  def indexOfSmallest(xs: List[Int]): Int = xs.zipWithIndex.minBy(_._1)._2

  def selectSort(xs: List[Int]): List[Int] =
    if xs.length < 2 then
      xs
    else
      val (l, x :: r) = xs.splitAt(indexOfSmallest(xs))
      x :: selectSort(l ::: r)

  @main def exerise23(): Unit =
    import scala.collection.mutable.ListBuffer
    var nums: ListBuffer[Int] = ListBuffer(8, 4, 0, 7, 3, 5, 1, 9, 6, 2)
    selectionSortTR(nums)
    println(nums)

    /* Experiment to demonstrate the version using immutable List */
    println(selectSort(List()))
    println(selectSort(List(1)))
    println(selectSort(List(1, 2)))
    println(selectSort(List(8, 4, 0, 7, 3, 5, 1, 9, 6, 2)))




  /* Exercise 24 code */

  def factorial(n: Long): Long =
    if n == 0 then
      1
    else
      n * factorial(n - 1)

  def factorial2(n: BigInt): BigInt =
    if n == 0 then
      1
    else
      n * factorial2(n - 1)

  @main def exerise24(): Unit =
    val n = 22
    (0 until n).foreach(n => println(factorial(n)))
    (0 until 1000).foreach(n => println(factorial2(n)))




  /* Exercise 25 code */

  /*
   * Study the Fibonacci method below and read the notes that accompany it. If you are unfamiliar
   * with the Fibonacci sequence you may wish to visit https://en.wikipedia.org/wiki/Fibonacci_number
   */
  def fibonacci(n: BigInt): BigInt =
    @tailrec
    def loop(a: BigInt, b: BigInt, n: BigInt): BigInt =
      if (n == 0) a + b
      else loop(b, a + b, n - 1)

    if (n == 0) 0
    else if (n == 1) 1
    else loop(0, 1, n - 2)

  /*
   * There are some important points to note about this method definition:
   * 1. Notice the use of the type BigInt. This type represents unbounded integers which cannot overflow.
   *    They are built on top of Java's BigInteger type but look a lot prettier in Scala. Because the
   *    Fibonacci sequence generates very big numbers very quickly it is useful to use the BigInt data
   *    type in this instance.
   * 2. The method fibonacci is NOT recursive. It deals with the initial cases of the zero-th and first
   *    Fibonacci numnbers. However, for the nth Fibonacci number (where n>1), a call is made to an
   *    auxialliary method called loop.
   * 3. The auxilliary method uses recursion and takes three parameters. The first two parameters carry
   *    the previous two values in the sequence. This is why, when it is called initally from fibonacci,
   *    the initial values of a and b are 0 and 1 respectively. The third parameter is a counter
   *    representing which Fibonacci number is required. Initially it is the value originally requested.
   *    However, with each recursive call it is reduced by one until it reaches the base value of zero.
   * 4. The reason for coding the method using an auxilliary is because we do not want to clutter up
   *    the main fibonacci method with parameters carrying the previous two values. This is really
   *    internal information that the client does not want to see. Therefore, the main method only
   *    takes as input the index, n, that the client wishes to provide. The auxilliary method then does
   *    the bulk of the work.
   * Let us trace an evaluation to see how it works:
   *
   *    fibonacci(8)
   *  = loop(0,1,8)
   *  = loop(1,1,7)
   *  = loop(1,2,6)
   *  = loop(2,3,5)
   *  = loop(3,5,4)
   *  = loop(5,8,3)
   *  = loop(8,13,2)
   *  = 8+13
   *  = 21
   */

  def fibonacci2(n: BigInt): BigInt =
    if n < 2 then
      n
    else
      var prev1 = 0
      var prev2 = 1
      var curr = 1
      var i = 2
      while i < n do
        prev1 = prev2
        prev2 = curr
        curr = prev1 + prev2
        i = i + 1
      curr

  @main def exerise25(): Unit =
    (0 until 20).foreach(n => print(s" ${fibonacci(n)}"))
    println()
    (0 until 20).foreach(n => print(s" ${fibonacci2(n)}"))
    println()




  /* Exercise 26 code */
  /*
  * N.B. In these sample solutions we have added some methods to the abstract class Num. These
  * provide symbolic, infix names for the methods defined elsewhere. They make it easier to
  * express arithmetic expressions because numbers can be combined using the infix operators
  * instead. For example:  one + two * three. It is easier to read and write than its
  * translation:  add(one, times(two, three)), and also supports the standard operator
  * precendences as well.
  */

  sealed abstract class Num:
    def +(that: Num): Num = add(this)(that)

    def *(that: Num): Num = mul(this)(that)

    def -(that: Num): Num = sub(this)(that)

    def **(that: Num): Num = pwr(this)(that)

    def >(that: Num): Boolean = gt(this)(that)

  case object Zero extends Num

  case class Succ(n: Num) extends Num

  /*
   * We define some useful Num instances:
   */
  val zero: Num = Zero
  val one: Num = Succ(Zero)
  val two: Num = Succ(Succ(Zero))
  val three: Num = Succ(Succ(Succ(Zero)))
  val ten: Num = Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))))))

  /*
   * Clearly, writing out instances for each whole number is unwieldy, not to mention impossible.
   * However, we can write a conversion function from Int => Num. This makes it much easier to
   * create arbitrary Nums.
   */
  def toNum(i: Int): Num =
    if (i < 0)
      throw new ArithmeticException("Trying to convert a -ve Int to a Num")
    else if (i == 0)
      Zero
    else
      Succ(toNum(i - 1))

  /*
   * And it is possible to go the other way: to convert a Num into an Int. This is easy to
   * achieve with pattern matching.
   */
  def toInt(n: Num): Int = n match {
    case Zero => 0
    case Succ(n) => 1 + toInt(n)
  }

  /*
   * One of the simplest operations on a Num is to add one: i.e. an increment function. To increase a
   * Num by one it is necessary to add and extra Succ constructor around it:
   */
  def incr(n: Num): Num = Succ(n)

  /*
   * How can we add two Num values together?  Clearly, if we want to add
   * Succ(Succ(Zero)) and Succ(Succ(Succ(Zero))) we should get Succ(Succ(Succ(Succ(Succ(Zero))))).
   * That is, 2+3=5. The following curried method achieves this by using recursion on the structure
   * of the first number.
   */
  def add(m: Num)(n: Num): Num = m match {
    case Zero => n
    case Succ(k) => Succ(add(k)(n))
  }

  /*
   * Let us trace this using our example. See how the first argument (m) successively reduces in
   * size until it reaches the base case - Zero. At this point the recursion ends and the answer
   * is delivered.
   *   add (Succ(Succ(Zero))) (Succ(Succ(Succ(Zero))))
   * = Succ( add (Succ(Zero)) (Succ(Succ(Succ(Zero)))) )
   * = Succ(Succ( add (Zero) (Succ(Succ(Succ(Zero)))) ))
   * = Succ(Succ(Succ(Succ(Succ(Zero)))))
   */

  def mul(m: Num)(n: Num): Num = m match
    case Zero => Zero
    case Succ(k) => add(mul(k)(n))(n)

  def decr(n: Num): Num = n match
    case Zero => throw new IllegalArgumentException()
    case Succ(m) => m

  @tailrec
  def sub(m: Num)(n: Num): Num = (m, n) match {
    case (Zero, _) => Zero
    case (_, Zero) => m
    case (Succ(p), Succ(q)) => sub(p)(q)
  }

  def pwr(n: Num)(p: Num): Num = (n, p) match {
    case (_, Zero) => Succ(Zero)
    case (Zero, _) => Zero
    case (_, Succ(k)) => mul(pwr(n)(k))(n)
  }

  @tailrec
  def gt(m: Num)(n: Num): Boolean = (m, n) match
    case (Zero, _) => false
    case (Succ(_), Zero) => true
    case (Succ(p), Succ(q)) => gt(p)(q)

  @main def exercise26(): Unit =
    List(zero, one, two, three, ten).foreach(println)
    (0 to 20).foreach(toNum andThen println)
    println(add(three)(ten))
    println(three + ten) // read the comment above the class Num
    println(List(one, two, three) map add(two))
    println(List(one, two, three) map (_ + two)) // read the comment above the class Num
    println(pwr(two)(three))
    println(one + (two + three) == (one + two) + three)
}
/*
To show that add(m)(add(n)(p)) == add(add(m)(n))(p)
Proof is by induction on m:

Case m = Zero
add(Zero)(add(n)(p))        Left hand side
= add(n)(p)                 Definition of add, case 0
= add(add(zero)(n))(p)      Definition of add, case 0
= Right hand side
which establishes the case.

Case m = Succ(k)
add(Succ(k))(add(n)(p))     Left hand side
= Succ(add(k)(add(n)(p)))   Definition of add, case 1
= Succ(add(add(k)(n))(p))   By induction
= add(Succ(add(k)(n)))(p)   Definition of add, case 1 (right to left)
= add(add(Succ(k))(n))(p)   Definition of add, case 1 (right to left)
= Right hand side
which establishes the case, and completes the proof.

Equivalently,
m + (n + p) == (m + n) + p

*/