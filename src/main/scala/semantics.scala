def step(term: Stlc): Option[Stlc] = {
  import Stlc.*
  term match {

    // Actual work
    case Plus(SInt(n1), SInt(n2))     => Some(SInt(n1 + n2))
    case Plus(SFloat(n1), SFloat(n2)) => Some(SFloat(n1 + n2))
    case App(Lam(x, t, e), v: Value)  => Some(subst(x, e, v))

    // Evaluation contexts
    case Plus(v1: Value, e2) => step(e2).map(Plus(v1, _))
    case Plus(e1, e2)        => step(e1).map(Plus(_, e2))
    case App(v1: Value, e2)  => step(e2).map(App(v1, _))
    case App(e1, e2)         => step(e1).map(App(_, e2))

    // Values or stuck terms
    case _ => None
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
