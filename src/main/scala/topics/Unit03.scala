package topics

import scala.annotation.tailrec

object Unit03 {

  val wordList: List[String] = List("The", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog")
  val charList: List[Char] = ('a' to 'z').toList
  val aardvark: List[Char] = "aardvark".toList
  val intList: List[Int] = (0 to 9).toList

  /*
   * Exercise 31
   * Using only head and tail extract the following words from wordList:
   * (a) The
   * (b) quick
   * (c) dog
   * Then extract the following words from wordList using only init and last:
   * (d) The
   * (e) lazy
   * (f) dog
   */

  @main
  def Exercise31(): Unit =
    val a = wordList.head
    val b = wordList.tail.head
    val c = wordList.tail.tail.tail.tail.tail.tail.tail.tail.head
    val d = wordList.init.init.init.init.init.init.init.init.last
    val e = wordList.init.last
    val f = wordList.last
    println(a)
    println(b)
    println(c)
    println(d)
    println(e)
    println(f)

  /*
   * Exercise 32
   * Using charList, intList, and wordList, respectively, and at most the operators
   * take, drop, and ::: build the following lists:
   * (a) List(x, y, z, a, b, c, d)
   * (b) List(9, 0)
   * (c) List(over, the, lazy, dog, jumped, The, quick, brown, fox)
   * Use the method splitAt to divide the intList in each of the following ways:
   * (d) (List(0, 1, 2, 3), List(4, 5, 6, 7, 8, 9))
   * (e) (List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(9))
   * (f) (List(), List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
   */

  @main
  def Exercise32(): Unit =
    val a = charList.drop(23) ::: charList.take(3)
    val b = intList.drop(9) ::: intList.take(1)
    val c1 = wordList.drop(5) ::: wordList.drop(4).take(1) ::: wordList.take(4)
    // Scala also has the slice operation that you can use to extract a subsequence
    val c2 = wordList.drop(5) ::: wordList.slice(4, 5) ::: wordList.take(4)
    val d = intList.splitAt(4)
    val e = intList.splitAt(9)
    val f = intList.splitAt(0)
    println(a)
    println(b)
    println(c1)
    println(c2)
    println(d)
    println(e)
    println(f)


  /*
   * Exercise 33
   * Construct appropriate (function) arguments ... for map such that (e.g.) intList.map(...) generates
   * each of the following lists:
   * (a) List(0, 2, 4, 6, 8, 10, 12, 14, 16, 18)
   * (b) List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
   * (c) List(0, 1, 2, 3, 4, 5, 5, 5, 5, 5)
   * (d) List(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
   * (e) List(a, b, c, d, e, f, g, h, i, j)
   * (f) List(The, quick, brown, fox, jumped, over, the, lazy, dog, The)
   * (g) List(false, false, true, true, false, true, false, false, true)
   * (h) List(THE, QUICK, BROWN, FOX, JUMPED, OVER, THE, LAZY, DOG)
   * (i) List(List(T, h, e), List(q, u, i, c, k), List(b, r, o, w, n), List(f, o, x),
   *          List(j, u, m, p, e, d), List(o, v, e, r), List(t, h, e), List(l, a, z, y),
   *          List(d, o, g))
   */

  @main
  def Exercise33(): Unit =
    val a = intList.map(_ * 2) // or can write as (k => x*2)
    val b = intList.map(_ * 2 + 1) // etc.
    val c = intList.map(math.min(_, 5))
    val d = intList.map(9 - _)
    val e = intList.map(charList(_))
    val f = intList.map(k => wordList(k % 9))
    val g = wordList.map(_.contains('o'))
    val h = wordList.map(_.toUpperCase)
    val i = wordList.map(_.toList)
    println(a)
    println(b)
    println(c)
    println(d)
    println(e)
    println(f)
    println(g)
    println(h)
    println(i)


  /*
   * Exercise 34
   * Construct appropriate (function) arguments ... and use with filter, takeWhile, or
   * dropWhile, and the relevant list such that (e.g.) intList.filter(...), wordList.takeWhile(...),
   * etc., generates each of the following lists:
   * (a) List(a, a)
   * (b) List(r, d, v, a, r, k)
   * (c) List(a, a, a)
   * (d) List(0, 3, 6, 9)
   * (e) List(a, e, i, o, u)
   * (f) List(The, fox, the, dog)
   * (g) List(The, quick, brown, fox)
   * (h) List(the, lazy, dog)
   * (i) List(x, y, z)
   * (j) List(0, 2, 3, 4, 6, 8, 9)
   */

  @main
  def Exercise34(): Unit =
    val a = aardvark.takeWhile(_ == 'a')
    val b = aardvark.dropWhile(_ == 'a')
    val c = aardvark.filter(_ == 'a')
    val d = intList.filter(_ % 3 == 0)
    val e = charList.filter("aeiou".contains(_))
    val f = wordList.filter(_.length == 3)
    val g = wordList.takeWhile(!_.contains('j'))
    val h = wordList.dropWhile(_ != "the")
    val i = charList.dropWhile(_ < 'x')
    val j = intList.filter(k => k % 2 == 0 || k % 3 == 0)
    println(a)
    println(b)
    println(c)
    println(d)
    println(e)
    println(f)
    println(g)
    println(h)
    println(i)
    println(j)


  /*
   * Exercise 35
   * Write a method, swapAround, that takes a list and returns it with the first half
   * swapped with the second half.  Thus, e.g.,
   *
   * swapAround(List("The", "horse", "the", "cart")) = List(the, cart, the, horse)
   *
   * Try your method out with a few lists of different element types to show it
   * in action.
   */

  def swapAround[A](xs: List[A]): List[A] =
    val (ys, zs) = xs.splitAt(xs.length / 2)
    zs ::: ys

  @main
  def Exercise35(): Unit =
    println(swapAround(List("The", "horse", "the", "cart")))
    println(swapAround(List(1, 2, 3, 4, 5)))
    println(swapAround(List.empty))


  /*
   * Exercise 36
   * Write a method, groupInto, that takes a list and splits it into packets of a given
   * size. Here are some examples:
   *
   * groupInto(4, "abcdefghijkl".toList) =
   *     List(List(a, b, c, d), List(e, f, g, h), List(i, j, k, l))
   *
   * groupInto(3, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) =
   *    List(List(0, 1, 2), List(3, 4, 5), List(6, 7, 8), List(9))
   *
   * groupInto(1, wordList) =
   *    List(List(The), List(quick), List(brown), List(fox), List(jumped), List(over),
   *         List(the), List(lazy), List(dog))
   *
   * groupInto(8, aardvark)) =
   *     List(List(a, a, r, d, v, a, r, k))
   *
   * The final list in the result may have fewer elements if there are insufficient
   * elements to make up the packet size.
   */

  def groupInto[A](size: Int, xs: List[A]): List[List[A]] =
    val blocks = (xs.indices by size).toList
    blocks.map(k => xs.drop(k)).map(_.take(size))

  @main
  def Exercise36(): Unit =
    println(groupInto(4, "abcdefghijkl".toList))
    println(groupInto(3, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)))
    println(groupInto(1, wordList))
    println(groupInto(8, aardvark))



  /*
   * Exercise 37
   * This exercise builds upon your previous answer and assumes that you have developed
   * a working version of the groupInto method.
   *
   * (a) Write a method, packetReverse, that splits a list into packets, reverses each
   *     packet, and flattens the result. For example:
   *
   *     packetReverse(3, intList) = List( 2, 1, 0, 5, 4, 3, 8, 7, 6, 9)
   *     packetReverse(2, wordList) = List(quick, The, fox, brown, over, jumped, `lazy`, the, dog)
   *     packetReverse(8, aardvark) = List(k, r, a, v, d, r, a, a)
   *
   *     You should avoid inserting a reverse method into a copy of your groupInto method.
   *     Rather, you should combine your groupInto method, with reverse and flatMap to achieve
   *     the result.
   *
   * (b) Write a method, tabulate, that takes a list of numbers and formats it as an HTML
   *     table with the given width. Thus, e.g.,
   *
   *     tabulate(2, List(1, 2, 3, 4) =
   *         <table><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></table>
   *
   *     tabulate(List(1, List(1, 2)) =
   *         <table><tr><td>1</td></tr><tr><td>2</td></tr></table>
   *
   *     This can be solved using groupInto and map methods (although we do not want to restrict
   *     you to these if you wish to explore alternatives.) However, you will find the following
   *     method useful, too:  mkString.  The standard Scala method mkString compresses a list of
   *     strings into a single string.  E.g. List("abc", "def", "gh").mkString = "abcdefgh". (In
   *     fact, mkString is much more general than this - but we leave you to look it up if you
   *     want to learn more about it.)
   */

  def packetReverse[A](size: Int, xs: List[A]): List[A] =
    groupInto(size, xs).flatMap(_.reverse)

  def tabulate(width: Int, xs: List[Int]): String =
    val elems = xs.map(k => s"<td>$k</td>") // each number is wrapped: e.g. 1 as "<td>1</td>"
    val rows = groupInto(width, elems)
      .map(_.mkString)
      .map(r => "<tr>" + r + "</tr>") // a list of rows, each like "<tr><td>...</td></tr>"
    "<table>" + rows.mkString + "</table>" // compress rows into a string and add table tags

  @main
  def Exercise37(): Unit =
    println(packetReverse(4, "abcdefghijkl".toList))
    println(packetReverse(3, intList))
    println(packetReverse(2, wordList))
    println(packetReverse(8, aardvark))
    println(tabulate(2, List(1, 2, 3, 4)))
    println(tabulate(1, List(1, 2)))


  /*
  * Exercise 38
  * Tony Hoare's famous quicksort algorithm can be written recursively. See below. It uses
  * the head of the list as the pivot element which does not work well for nearly or fully
  * ordered data (O(n^2)). But for reasonably random data this version will have the n(log n)
  * performance normally associated with a good implementation of quicksort. The version
  * below is notable for the simplicity of its expression in a functional style.
  *
  * The function random(k) generates a list of k random characters drawn from the digits
  * 0-9 and the upper and lower case letters.
  *
  * Your task is to write the merge sort function that works by splitting the list into
  * two halves, recursively sorting each half, and then merging the resulting ordered
  * lists back together. Demonstrate your function in action.
  *
  */

  def qsort(xs: List[Char]): List[Char] = xs match
    case List() => List()
    case y :: ys =>
      val (less, more) = ys partition (_ <= y)
      qsort(less) ::: y :: qsort(more)

  /**
   * Generates a list of k random alphanumeric characters
   */
  def random(k: Int): List[Char] = scala.util.Random.alphanumeric.take(k).toList

  /*
   * merge assumes that each of its arguments are in ascending order. The method
   * merges the two lists into one which is also in ascending order.
   */
  def merge(as: List[Char], bs: List[Char]): List[Char] = (as, bs) match
    case (List(), ys) => ys
    case (xs, List()) => xs
    case (x :: xs, y :: ys) =>
      if (x < y)
        x :: merge(xs, y :: ys)
      else
        y :: merge(x :: xs, ys)

  def msort(xs: List[Char]): List[Char] =
    if xs.length <= 1 then xs
    else
      val (ys, zs) = xs.splitAt(xs.length / 2)
      merge(msort(ys), msort(zs))

  @main
  def Exercise38(): Unit =
    val rs = random(100)
    println(rs.mkString)
    println(qsort(rs).mkString)
    println(msort(rs).mkString)

  /*
  * Exercise 39
  */

  extension (xs: List[Int])
    def *(n: Int): List[Int] = xs map (_ * n)
    def +(n: Int): List[Int] = xs map (_ + n)
    def allEven: Boolean = xs forall(_ % 2 == 0)
    def group(n: Int): List[List[Int]] = groupInto(n, xs)

  @main
  def Exercise39(): Unit =
    println(intList * 2)
    println(intList + 100)
    println(List(1, 2, 3).allEven)
    println((List(1, 2, 3) * 2).allEven)
    println(intList group 3)

}
