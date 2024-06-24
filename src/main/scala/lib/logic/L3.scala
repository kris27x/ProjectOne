package lib.logic
import lib.picture.Picture
import lib.picture.Picture.*

object L3 {
  sealed abstract class Formula {
    override def toString: String = this match
      case T => "T"
      case F => "F"
      case U => "U"
      case A => "A"
      case B => "B"
      case C => "C"
      case D => "D"
      case E => "E"
      case M(q) => s"M$q"
      case L(q) => s"L$q"
      case I(q) => s"I$q"
      case Not(q) => s"~$q"
      case And(q, r) => s"($q & $r)"
      case Or(q, r) => s"($q + $r)"
      case Cond(q, r) => s"($q -> $r)"
      case Eqv(q, r) => s"($q <-> $r)"

    def map(f: (TruthVal | TruthVar) => Formula): Formula = this match
      case v: (TruthVal | TruthVar) => f(v)
      case M(q) => M(q.map(f))
      case L(q) => L(q.map(f))
      case I(q) => I(q.map(f))
      case Not(q) => Not(q.map(f))
      case And(q, r) => And(q.map(f), r.map(f))
      case Or(q, r) => Or(q.map(f), r.map(f))
      case Cond(q, r) => Cond(q.map(f), r.map(f))
      case Eqv(q, r) => Eqv(q.map(f), r.map(f))

    def getVars: Seq[TruthVar] = (this match
      case _: TruthVal => Vector()
      case v: TruthVar => Vector(v)
      case M(q) => q.getVars
      case L(q) => q.getVars
      case I(q) => q.getVars
      case Not(q) => q.getVars
      case And(q, r) => q.getVars ++ r.getVars
      case Or(q, r) => q.getVars ++ r.getVars
      case Cond(q, r) => q.getVars ++ r.getVars
      case Eqv(q, r) => q.getVars ++ r.getVars
      ).distinct

    def evaluate(env: Map[TruthVar, TruthVal]): TruthVal = this match
      case t: TruthVal => t
      case v: TruthVar => env.getOrElse(v, U)
      /*
       * Implements Łukasiewicz's Ł3 logic
         Conditional
         ------------
         A  B  A -> B
         ------------
         T  T    T
         T  U    U
         T  F    F
         U  T    T
         U  U    T
         U  F    U
         F  T    T
         F  U    T
         F  F    T

         ~A     = A -> F
         A + B  = (A -> B) -> B
         A & B  = ~(~A + ~B)
         A <-> B = (A -> B) & (B -> A)

        Additionally (Tarski)
        MA = ~A -> A      It is unknown that
        LA = ~M~A         It is necessary that
        IA = MA & ~LA     It is possible that
       */
      case Cond(q, r) => (q.evaluate(env), r.evaluate(env)) match
        case (F, _) => T
        case (U, F) => U
        case (U, U) | (U, T) => T
        case (T, y) => y
      case M(q) => Cond(Not(q), q).evaluate(env)
      case L(q) => Not(M(Not(q))).evaluate(env)
      case I(q) => And(M(q), Not(L(q))).evaluate(env)
      case Not(q) => Cond(q, F).evaluate(env)
      case Eqv(q, r) => And(Cond(q, r), Cond(r, q)).evaluate(env)
      case And(q, r) => Not(Or(Not(q), Not(r))).evaluate(env)
      case Or(q, r) => Cond(Cond(q, r), r).evaluate(env)

    def generateTruthTable: (Seq[TruthVar], Seq[Seq[TruthVal]], Seq[TruthVal]) =
      val vars: Seq[TruthVar] = this.getVars
      val combinations: Seq[Seq[TruthVal]] = L3.truthValCombinations(vars.length)
      val results = for row <- combinations yield this.evaluate((vars zip row).toMap)
      (vars, combinations, results)

    def isTautology: Boolean = this.generateTruthTable._3 forall (_ == T)

    def isSatisfiable: Boolean = this.generateTruthTable._3 contains T

    def showTruthTable: Picture =
      val (vars, combis, results) = this.generateTruthTable
      val header: Seq[Picture] =
        (for v <- vars yield Picture(v.toString)) :+ Picture(this.toString)
      val rows: Seq[Seq[TruthVal]] = (combis zip results) map (_ :+ _)
      val rowPictures: Seq[Seq[Picture]] = rows map (_ map (v => Picture(v.toString)))
      (header.map(_.borderLR()) +: rowPictures).formatAsTable(CTR)
  }

  sealed abstract class TruthVal extends Formula
  sealed abstract class TruthVar extends Formula

  case object T extends TruthVal
  case object U extends TruthVal
  case object F extends TruthVal
  case object A extends TruthVar
  case object B extends TruthVar
  case object C extends TruthVar
  case object D extends TruthVar
  case object E extends TruthVar

  case class M(p: Formula) extends Formula // (Tarski) It is possible that ...
  case class L(p: Formula) extends Formula // (Tarski) It is necessary that ...
  case class I(p: Formula) extends Formula // (Tarski) It is unknown that ...
  case class Not(p: Formula) extends Formula // (Łukasiewicz) It is not that ...
  case class And(p: Formula, q: Formula) extends Formula // (Łukasiewicz) p + q
  case class Or(p: Formula, q: Formula) extends Formula // (Łukasiewicz) p & q
  case class Cond(p: Formula, q: Formula) extends Formula // (Łukasiewicz) p -> q
  case class Eqv(p: Formula, q: Formula) extends Formula // (Łukasiewicz) p <-> q

  object L3 {
    def truthValCombinations(numVars: Int): Seq[Seq[TruthVal]] =
      if numVars <= 0 then Seq()
      else if numVars == 1 then Seq(Seq(T), Seq(F), Seq(U))
      else
        val previousTable = truthValCombinations(numVars - 1)
        for
          v <- List(T, F, U)
          row <- previousTable
        yield
          row :+ v
  }
}
