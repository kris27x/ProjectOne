package lib.logic
object L3Parser {
  import lib.parser.Parser.*
  import lib.parser.ParserLib.*
  import lib.logic.L3.*

  def formula: Parser[Formula] = conditional.sp.someWith(string("<->").sp) map (_.reduceLeft(Eqv.apply))

  def conditional: Parser[Formula] = disjunction.sp.someWith(string("->").sp) map (_.reduceLeft(Cond.apply))

  def disjunction: Parser[Formula] = conjunction.sp.someWith(string("+").sp) map (_.reduceLeft(Or.apply))

  def conjunction: Parser[Formula] = unary.sp.someWith(string("&").sp) map (_.reduceLeft(And.apply))

  def unary: Parser[Formula] =
    (for _ <- char('~').sp; u <- unary yield Not(u)) <|>
      (for _ <- char('M').sp; u <- unary yield M(u)) <|>
      (for _ <- char('L').sp; u <- unary yield L(u)) <|>
      (for _ <- char('I').sp; u <- unary yield I(u)) <|>
      primary

  def primary: Parser[Formula] =
    truthval <|>
      truthvar <|>
      formula.bracketed(char('(').sp, char(')').sp)

  def truthval: Parser[Formula] =
    ((char('T') >> result(T)) <|> (char('F') >> result(F)) <|> (char('U') >> result(U))).sp

  def truthvar: Parser[Formula] =
    ((char('A') >> result(A)) <|> (char('B') >> result(B)) <|> (char('C') >> result(C)) <|>
    (char('D') >> result(D)) <|> (char('E') >> result(E))).sp

}
