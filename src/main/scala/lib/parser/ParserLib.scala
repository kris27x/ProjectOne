package lib.parser
/*
The parsers in this library have been adapted (to Scala 3) from their Haskell
counterparts which appear in
Bird, R., Thinking Functionally with Haskell, Cambridge University Press, 2015,
Ch 11.

Further useful parsers have also been included.
*/

object ParserLib {
  import Parser.*

  /**
   * A parser that requires the first character to satisfy p. If
   * it does the character is consumed and returned. Otherwise the
   * parser fails.
   *
   * @param p The condition that the head character must satisfy
   * @return A parser that requires that the next character
   *         satisfies a condition, p
   */
  def sat(p: Char => Boolean): Parser[Char] = getc.withFilter(p)

  /**
   * A parser that requires a specific character at the head of the
   * input string. Once matched, the character is thrown away.
   *
   * @param c The required character
   * @return A parser that requires c as the next character
   */
  def char(c: Char): Parser[Unit] = sat(_ == c) >> result(())

  /**
   * A parser that requires a lower case character at the head of the
   * input string. The matched character is returned as the result of
   * the successful parse.
   *
   * @return A parser that requires a lower case letter
   */
  def lower: Parser[Char] = sat(_.isLower)

  /**
   * A parser that requires an upper case character at the head of the
   * input string. The matched character is returned as the result of
   * the successful parse.
   *
   * @return A parser that requires an upper case letter
   */
  def upper: Parser[Char] = sat(_.isUpper)

  /**
   * A parser that requires a letter at the head of the input string.
   * The matched character is returned as the result of the successful
   * parse.
   *
   * @return A parser that requires any letter
   */
  def alpha: Parser[Char] = sat(_.isLetter)

  /**
   * A parser that requires a digit at the head of the input string.
   * The matched character is returned as the result of the successful
   * parse.
   *
   * @return A parser that requires a digit
   */
  def digit: Parser[Char] = sat(_.isDigit)

  /**
   * A parser that requires an alphanumeric character at the head of
   * the input string. The matched character is returned as the result
   * of the successful parse.
   *
   * @return A parser that requires any alphanumeric character
   */
  def alphanum: Parser[Char] = sat(_.isLetterOrDigit)

  /**
   * A parser that requires a specific string as a prefix to the input
   * string. The matched string is thrown away.
   *
   * @param s The required string
   * @return A parser that requires a specific string prefix
   */
  def string(s: String): Parser[Unit] =
    if s.isEmpty then
      result(())
    else
      char(s.head) >> string(s.tail) >> result(())

  /**
   * A parser that recognises a program identifier that starts with a
   * lower case character and is followed by any number of characters
   * and/or digits.
   *
   * @return A parser that recognises a program identifier
   */
  def ident: Parser[String] =
    for c <- lower; cs <- alphanum.many yield (c +: cs).mkString

  /**
   * A parser that recognises a natural number. In this case, the parser
   * accepts a sequence of digits which means that leading zeros are
   * allowed, but no preceding sign (+ or -) can be used.
   *
   * @return A parser that recognises a non-negative integer
   */
  def nat: Parser[Int] = for ds <- digit.some yield ds.mkString.toInt

  /**
   * A parser that recognises any integer. The integer may be preceded by
   * a + sign or a - sign.
   *
   * @return A parser that recognises a signed int value
   */
  def int: Parser[Int] =
    nat <+>
      (char('+') >> nat) <+>
      (char('-') >> nat map (-_))

  /**
   * A parser that consumes and discards any amount of contiguous whitespace
   */
  def space: Parser[Unit] = sat(_.isSpaceChar).many >> result(())

  /**
   * A parser that strips leading whitespace before attempting to satisfy
   * the parser p
   *
   * @param p The parser that is applied once leading whitespace is removed
   * @return A parser that strips leading whitespace then acts like p
   */
  def token[A](p: Parser[A]): Parser[A] = space >> p

  extension[A] (p: Parser[A]) {
    /**
     * Removes any (possibly no) leading spaces before processing this parser, p.
     * E.g.  string("foo").sp matches "    foo"
     */
    def sp: Parser[A] = token(p)

    /*
     * Looks for a repeated pattern separated by a pattern: e.g.
     * digit.manyWith(char(',')) applied to "1,2,3" would give:
     * Seq( (Seq(1,2,3), Seq(1,2), Seq(1) )
     * This may match no instances. E.g., digit.manyWith(char(','))
     * applied to "x,2,3" would give Seq().
     */
    def manyWith[B](sep: => Parser[B]): Parser[Seq[A]] =
      p.someWith(sep) <+> none

    /*
     * Looks for a repeated pattern separated by a pattern: e.g.
     * digit.someWith(char(',')) applied to "1,2,3" would give:
     * Seq( (Seq(1,2,3), Seq(1,2), Seq(1) ).
     */
    def someWith[B](sep: => Parser[B]): Parser[Seq[A]] =
      for x <- p; xs <- (sep >> p).many yield x +: xs

    /**
     * Surrounds a parser with two other parsers that act like
     * brackets. The intention is that the surrounding (bracket)
     * parsers match relevant text but their results are thrown
     * away. Only the result from this parser, p, is returned.
     * E.g. getc('*').bracketed(char('('), char('))) would match
     * "(*)" and only '*' would be returned.
     *
     * @param open  The parser for the open bracket
     * @param close The parser for the close bracket
     * @return A parser that returns the value matched between
     *         the text matched by the open and close parsers
     */
    def bracketed[O, C](open: => Parser[O], close: => Parser[C]): Parser[A] =
      for _ <- open; x <- p; _ <- close yield x
  }
}
