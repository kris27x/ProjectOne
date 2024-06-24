package topics

import lib.logic.L3.*

object Unit07 {

  /*
   * Exercise 71
  */

  @main def exercise71(): Unit =
    val f: Formula = Cond(A, B)
    val env: Map[TruthVar, TruthVal] = Map(A -> U, B -> F)
    println(f)
    println(f.showTruthTable)
    println(s"$f.evaluate($env) = ${f.evaluate(env)}")

    val f1: Formula = Eqv(And(A, B), And(B, A))
    val env1: Map[TruthVar, TruthVal] = Map(A -> F, B -> T)
    println(f1)
    println(f1.showTruthTable)
    println(s"$f1.evaluate($env1) = ${f1.evaluate(env1)}")

    val f2: Formula = Eqv(And(A, Or(B, C)), Or(And(A, B), And(A, C)))
    val env2: Map[TruthVar, TruthVal] = Map(A -> U, B -> T, C -> F)
    println(f2)
    println(f2.showTruthTable)
    println(s"$f2.evaluate($env2) = ${f2.evaluate(env2)}")

    val f3: Formula = Cond(A, Cond(Not(A), A))
    val env3: Map[TruthVar, TruthVal] = Map()
    println(f3)
    println(f3.showTruthTable)
    println(s"$f3.evaluate($env3) = ${f3.evaluate(env3)}")

    val f4: Formula = Or(A, Not(A))
    val env4: Map[TruthVar, TruthVal] = Map(A -> U)
    println(f4)
    println(f4.showTruthTable)
    println(s"$f4.evaluate($env4) = ${f4.evaluate(env4)}")

    val f5: Formula = Cond(Not(L(Not(A))), U)
    val env5: Map[TruthVar, TruthVal] = Map(A -> T)
    println(f5)
    println(f5.showTruthTable)
    println(s"$f5.evaluate($env5) = ${f5.evaluate(env5)}")

    val f6: Formula = Eqv(Cond(A, B), Cond(Not(B), Not(A)))
    val env6: Map[TruthVar, TruthVal] = Map(A -> F)
    println(f6)
    println(f6.showTruthTable)
    println(s"$f6.evaluate($env6) = ${f6.evaluate(env6)}")

  /*
   * Exercise 72
  */
  @main def exercise72(): Unit =
    val f: Formula = Cond(A, Eqv(B, And(C, Or(D, Not(Cond(E, F))))))
    println(f)
    val g: Formula = f map { case A | B | C => T; case v => v }
    println(g)

  /*
   * Exercise 73
  */
  def equiv(f1: Formula, f2: Formula): Boolean = Eqv(f1, f2).isTautology

  @main def exercise73(): Unit =
    println(equiv(A, Not(Not(A))))
    println(equiv(And(A, B), Cond(A, B)))

    val f: Formula = Eqv(Or(And(A, B), C), Cond(A, Cond(B, C)))
    val (names, combos, results) = f.generateTruthTable
    val vals = for (c, r) <- combos zip results if r == T yield c
    println(s"Values that satisfy $f are")
    println(names.map(_.toString).mkString)
    println("-" * names.length)
    vals foreach { (r: Seq[TruthVal]) =>
      println(r.map(_.toString).mkString)
    }
}
