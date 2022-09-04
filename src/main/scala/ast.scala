enum Ty {
  case Float
  case Int
  case Unit
  case Arrow(from: Ty, to: Ty)

  override def toString(): String = this match {
    case Float         => "float"
    case Int           => "int"
    case Unit          => "unit"
    case Arrow(t1, t2) => s"($t1 -> $t2)"
  }
}

type Name = String

sealed trait Value

enum Stlc {
  case Lam(x: Name, t: Ty, body: Stlc) extends Stlc with Value
  case SInt(n: Int) extends Stlc with Value
  case SFloat(n: Double) extends Stlc with Value
  case SUnit extends Stlc with Value

  case Var(x: Name)
  case App(e1: Stlc, s2: Stlc)
  case Plus(e1: Stlc, e2: Stlc)

  override def toString(): String = this match {
    case Lam(x, t, body) => s"\\$x: $t => $body"
    case SInt(n)         => n.toString
    case SFloat(n)       => n.toString
    case SUnit           => "()"
    case Var(x)          => x
    case App(e1, e2)     => s"($e1 $e2)"
    case Plus(e1, e2)    => s"($e1 + $e2)"
  }

}
