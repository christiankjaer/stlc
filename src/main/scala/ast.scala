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

final case class SourceLocation(start: Int, end: Int) {
  def join(that: SourceLocation) =
    SourceLocation(start min that.start, end max that.end)
}

// This way of tagging the syntax is a bit nasty.
// I need it to track source positions.
enum Stlc[A] {
  case Lam(x: Name, t: Ty, body: Stlc[A], a: A) extends Stlc[A] with Value
  case SInt(n: Int, a: A) extends Stlc[A] with Value
  case SFloat(n: Double, a: A) extends Stlc[A] with Value
  case SUnit[A](a: A) extends Stlc[A] with Value

  case Var(x: Name, a: A)
  case App(e1: Stlc[A], s2: Stlc[A], a: A)
  case Plus(e1: Stlc[A], e2: Stlc[A], a: A)
  case ToInt(e1: Stlc[A], a: A)
  case ToFloat(e1: Stlc[A], a: A)

  override def toString(): String = this match {
    case Lam(x, t, body, _) => s"(\\$x: $t => $body)"
    case SInt(n, _)         => n.toString
    case SFloat(n, _)       => n.toString
    case SUnit(_)           => "()"
    case Var(x, _)          => x
    case App(e1, e2, _)     => s"($e1 $e2)"
    case Plus(e1, e2, _)    => s"($e1 + $e2)"
    case ToInt(e, _)        => s"int[$e]"
    case ToFloat(e, _)      => s"float[$e]"
  }

  // Every piece of surface syntax should have an A
  def get: A = this match {
    case Lam(_, _, _, a) => a
    case SInt(_, a)      => a
    case SFloat(_, a)    => a
    case SUnit(a)        => a
    case Var(_, a)       => a
    case App(_, _, a)    => a
    case Plus(_, _, a)   => a
    case ToInt(_, a)     => a
    case ToFloat(_, a)   => a
  }
}

type LStlc = Stlc[SourceLocation]
