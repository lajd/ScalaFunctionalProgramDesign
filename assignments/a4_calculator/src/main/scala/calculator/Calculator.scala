package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
      namedExpressions.map((k, v) => (k, Signal {eval(v(), namedExpressions)}))

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    expr match {
      case Expr.Literal(v) =>  v
      // Recursively substitute references for their expressions
      // using fold, indicating that substitution order is irrelevant
      // For each iteration, we remove the variable just used,
      // ensuring thqt there are no circular dependencies
      case Expr.Ref(name) =>
        val expr = references.get(name).fold(
          Expr.Literal(Double.NaN)) (
          s => s()
        )
        eval(expr, references - name)
      case Expr.Plus(a, b) =>  eval(a, references) + eval(b, references)
      case Expr.Minus(a, b) => eval(a, references)  - eval(b, references)
      case Expr.Divide(a, b) =>  eval(a, references) / eval(b, references)
      case Expr.Times(a, b) => eval(a, references) * eval(b, references)
    }