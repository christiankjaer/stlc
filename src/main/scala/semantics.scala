def step[A](term: Stlc[A]): Option[Stlc[A]] = {
  import Stlc.*
  term match {

    // Actual work
    case Plus(SInt(n1, loc), SInt(n2, _), _)     => Some(SInt(n1 + n2, loc))
    case Plus(SFloat(n1, loc), SFloat(n2, _), _) => Some(SFloat(n1 + n2, loc))
    case App(Lam(x, t, e, _), v: Value, _)       => Some(subst(x, e, v))

    // Evaluation contexts
    case Plus(v1: Value, e2, loc) => step(e2).map(Plus(v1, _, loc))
    case Plus(e1, e2, loc)        => step(e1).map(Plus(_, e2, loc))
    case App(v1: Value, e2, loc)  => step(e2).map(App(v1, _, loc))
    case App(e1, e2, loc)         => step(e1).map(App(_, e2, loc))

    // Values or stuck terms don't step
    case _ => None
  }

}
def subst[A](x: Name, e: Stlc[A], v: Stlc[A] & Value): Stlc[A] = e match {

  // Not variables
  case e: (Stlc.SFloat[A] | Stlc.SInt[A] | Stlc.SUnit[A]) => e

  // Variables
  case Stlc.Var(y, _) if x == y => v
  case e: Stlc.Var[A]           => e

  // Things we might need to substitute in
  case Stlc.Plus(e1, e2, loc) =>
    Stlc.Plus(subst(x, e1, v), subst(x, e2, v), loc)
  case Stlc.App(e1, e2, loc) => Stlc.App(subst(x, e1, v), subst(x, e2, v), loc)
  case Stlc.Lam(y, t, body, loc) =>
    if x == y then e else Stlc.Lam(y, t, subst(x, body, v), loc)
}
