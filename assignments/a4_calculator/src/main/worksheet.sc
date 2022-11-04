import calculator._

val someSig = Signal { 5 }

val someOtherSig = Signal {
  someSig() + 100
}

assert(someOtherSig.currentValue == 105)

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)


def getVal(expr: Expr, exprMap: Map[String, Signal[Expr]]): Double =
  expr match {
    case Expr.Literal(v) =>  v
    case Expr.Ref(name) =>
      val expr = exprMap.get(name).fold(
        Expr.Literal(Double.NaN) )(
        s => s.currentValue  // We use fold here as order is irrelevant
      )
      getVal(expr, exprMap - name)  // We recursively get values, removing the variable from the next call
    case Expr.Plus(a, b) =>  getVal(a, exprMap) + getVal(b, exprMap)
    case Expr.Minus(a, b) => getVal(a, exprMap)  - getVal(b, exprMap)
    case Expr.Divide(a, b) =>  getVal(a, exprMap) / getVal(b, exprMap)
    case Expr.Times(a, b) => getVal(a, exprMap) * getVal(b, exprMap)
  }


val e1 = Expr.Literal(5.5)

val e2 = Expr.Literal(10.5)

val e3 = Expr.Plus(e1, e2)

val e4 = Expr.Plus(Expr.Ref("e1"), e2) // e1 + e2 = 5.5 + 10.5

val e5 = Expr.Literal(99.9)

val e6 = Expr.Plus(Expr.Ref("e1"), e1)

val a = Expr.Plus(Expr.Ref("b"), Expr.Literal(1))
val b = Expr.Times(Expr.Ref("a"), Expr.Literal(2))

val exprMap = Map[String, Signal[Expr]](
  "e1" ->  Signal {e1},
  "a" ->  Signal {a},
  "b" -> Signal {b}
)

val c6 = getVal(e6, exprMap)
val c3 = getVal(e3, exprMap)
val c4 = getVal(e4, exprMap)

getVal(a, exprMap)
