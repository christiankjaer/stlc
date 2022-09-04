def step(term: Stlc): Stlc = {
  import Stlc.*
  term match {

    // Actual work
    case Plus(SInt(n1), SInt(n2))     => SInt(n1 + n2)
    case Plus(SFloat(n1), SFloat(n2)) => SFloat(n1 + n2)
    case App(Lam(x, t, e), v: Value)  => subst(x, e, v)

    // Evaluation contexts
    case Plus(v1: Value, e2) => Plus(v1, step(e2))
    case Plus(e1, e2)        => Plus(step(e1), e2)
    case App(v1: Value, e2)  => App(v1, step(e2))
    case App(e1, e2)         => App(step(e1), e2)

    // Values or stuck terms
    case _ => term
  }

}
def subst(x: Name, e: Stlc, v: Stlc & Value): Stlc = e match {

  case e: (Stlc.SFloat | Stlc.SInt | Stlc.SUnit.type) => e

  case Stlc.Var(y) if x == y => v
  case e: Stlc.Var           => e

  case Stlc.Plus(e1, e2) => Stlc.Plus(subst(x, e1, v), subst(x, e2, v))
  case Stlc.App(e1, e2)  => Stlc.App(subst(x, e1, v), subst(x, e2, v))
  case Stlc.Lam(y, t, body) =>
    if x == y then e else Stlc.Lam(y, t, subst(x, body, v))
}
