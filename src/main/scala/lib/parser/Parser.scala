package lib.parser
/*
The parsers in this library have been adapted (to Scala 3) from their Haskell
counterparts which appear in
Bird, R., Thinking Functionally with Haskell, Cambridge University Press, 2015,
Ch 11.

Monadic parsers are described (using Haskell) in the following paper by Graham
Hutton:
https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

A video explanation of monadic parsers is provided by Graham Hutton available at
https://www.youtube.com/watch?v=dDtZLm7HIJs
*/

object Parser {

  /*
   * As Graham Hutton shows in the above video, a parser is a mapping from
   * strings to a list of (result, string) pairs.
   *
   * Parser[A] = String => Seq[(A, String)]
   *
   * Thus the Parser type is represented as a function which maps an input
   * string to a list of possible parses (this is why there is a list of
   * results). Each result is a tuple: the thing parsed and possibly transformed
   * into something useful (an Int, for example); and the remainder of the
   * input string.
   */

  type Parser[A] = String => Seq[(A, String)]

  /**
   * A parser that always fails.
   *
   * @return A parser that recognises nothing
   */
  def fail[A]: Parser[A] = s => Seq.empty

  /**
   * A parser that always succeeds.
   *
   * @param x The value with which the parser succeeds
   * @return A parser that succeeds with x
   */
  def result[A](x: A): Parser[A] = s => Seq((x, s))

  /**
   * A parser that returns an empty sequence of results. The
   * parser consumes no input.
   *
   * @return A parser that succeeds with an empty sequence
   *         (of successful parses)
   */
  def none[A]: Parser[Seq[A]] = result(Seq.empty)

  /**
   * A parser that consumes and returns the character at the head of
   * the input string. If the string is empty than the parse fails;
   * otherwise the head of the string is consumed and returned.
   *
   * @return A parser that removes a single character from the
   *         head of the input string
   */
  def getc: Parser[Char] = s =>
    if s.isEmpty then Seq() else Seq((s.head, s.tail))

  /*
   * The type Parser[A] is a synonym for
   * String => Seq[(A, String)]
   * This is, in turn, a shorthand for the type
   * Function1[String, Seq[(A, String)]]
   * It is possible to extend this type, thus adding methods that
   * apply to instances of Parser[A]
   */

  extension[A] (p: Parser[A]) {
    /**
     * Monadic bind: Sequencing one parser after another.
     * The result from this parser, p,  is passed to the function
     * f, which returns a new parser. It is usual for the
     * subsequent parser to make use of the returned value
     * from this parser, p. In this way, parsers may be placed
     * in sequence and the intermediate results "threaded"
     * through.
     */
    def >>=[B](f: A => Parser[B]): Parser[B] = s =>
      for (x, s1) <- p(s); (y, s2) <- f(x)(s1) yield (y, s2)
      //p(s).flatMap { case (x, s1) => f(x)(s1).map { case (y, s2) => (y, s2) } }

    /**
     * A synonym for >>= in which the result of this parser,
     * p, is ignored. This is used to sequence two parsers in
     * situations where this parser must be satisfied, but its
     * result is not required by the subsequent parser.
     */
    def >>[B](q: => Parser[B]): Parser[B] = p >>= (_ => q)

    /**
     * A synonym for >>= (monadic bind)
     */
    def flatMap[B](f: A => Parser[B]): Parser[B] = p >>= f

    /**
     * The result of this parser, p, is transformed by applying
     * the function g to the result.
     */
    def map[B](g: A => B): Parser[B] = s =>
      for (x, s1) <- p(s) yield (g(x), s1)

    /**
     * This filter checks that the parsed value(s) from this
     * parser, p, satisfies the given condition.  If all fail
     * the test then the for...yield comprehension produces an
     * empty sequence of parses. For all values that pass the
     * test, these are included in the sequence of successful
     * parses that is returned.
     */
    def withFilter(g: A => Boolean): Parser[A] = s =>
      for (x, s1) <- p(s) if g(x) yield (x, s1)

    /**
     * If this parser, p, succeeds with a sequence of successful
     * parses then return the first successful parse. The use of
     * Option is required in case there was no successful parse
     * possible, in which case None is returned.
     */
    def parse(s: String): Option[A] =
      val rs = p(s)
      if rs.nonEmpty then Some(rs.head._1) else None

    /**
     * Either this parser, p, must be satisfied or the alternative
     * parser, q, must be satisfied. If the grammar is ambiguous
     * then each successful parse is returned. This operator can
     * cause the number of parses to grow exponentially.
     */
    def <+>(q: => Parser[A]): Parser[A] = s => p(s) ++ q(s)

    /**
     * Either this parser, p, must be satisfied or the alternative
     * parser, q, must be satisfied. If the grammar is ambiguous
     * then only the first successful parse is returned.
     */
    def <|>(q: => Parser[A]): Parser[A] = s =>
          val ps = p(s)
          if ps.nonEmpty then ps else q(s)

    /**
     * Satisfy zero or more instances of this parser, p, in sequence.
     */
    def many: Parser[Seq[A]] = p.some <|> none

    /**
     * Satisfy this parser, p, at least once. Then follow this by
     * any number (including zero) of further iterations of p.
     */
    def some: Parser[Seq[A]] =
      for x <- p; xs <- p.many yield x +: xs
  }
}
