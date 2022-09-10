def stepCBV[A](term: Stlc[A]): Option[Stlc[A]] = {
  import Stlc.*
  term match {

    // Actual work
    case Plus(SInt(n1, loc), SInt(n2, _), _)     => Some(SInt(n1 + n2, loc))
    case Plus(SFloat(n1, loc), SFloat(n2, _), _) => Some(SFloat(n1 + n2, loc))
    case App(Lam(x, t, e, _), v: Value, _)       => Some(substValue(x, e, v))

    case ToFloat(SInt(n, loc1), _) => Some(SFloat(n.toDouble, loc1))
    case ToInt(SFloat(m, loc1), _) => Some(SInt(m.floor.toInt, loc1))

    // Builtins
    case Var("float", loc) =>
      Some(Lam("x", Ty.Int, ToFloat(Var("x", loc), loc), loc))
    case Var("int", loc) =>
      Some(Lam("x", Ty.Float, ToInt(Var("x", loc), loc), loc))

    // Evaluation contexts
    case Plus(v1: Value, e2, loc) => stepCBV(e2).map(Plus(v1, _, loc))
    case Plus(e1, e2, loc)        => stepCBV(e1).map(Plus(_, e2, loc))
    case App(v1: Value, e2, loc)  => stepCBV(e2).map(App(v1, _, loc))
    case App(e1, e2, loc)         => stepCBV(e1).map(App(_, e2, loc))
    case ToFloat(e, loc)          => stepCBV(e).map(ToFloat(_, loc))
    case ToInt(e, loc)            => stepCBV(e).map(ToInt(_, loc))

    // Values or stuck terms don't step
    case _ => None
  }

}

def stepCBN[A](term: Stlc[A]): Option[Stlc[A]] = {
  import Stlc.*
  term match {

    // Actual work
    case Plus(SInt(n1, loc), SInt(n2, _), _)     => Some(SInt(n1 + n2, loc))
    case Plus(SFloat(n1, loc), SFloat(n2, _), _) => Some(SFloat(n1 + n2, loc))
    case App(Lam(x, t, body, _), e, _)           => Some(substTerm(x, body, e))

    case ToFloat(SInt(n, loc1), _) => Some(SFloat(n.toDouble, loc1))
    case ToInt(SFloat(m, loc1), _) => Some(SInt(m.floor.toInt, loc1))

    // Builtins
    case Var("float", loc) =>
      Some(Lam("x", Ty.Int, ToFloat(Var("x", loc), loc), loc))
    case Var("int", loc) =>
      Some(Lam("x", Ty.Float, ToInt(Var("x", loc), loc), loc))

    // Evaluation contexts
    case Plus(v1: Value, e2, loc) => stepCBN(e2).map(Plus(v1, _, loc))
    case Plus(e1, e2, loc)        => stepCBN(e1).map(Plus(_, e2, loc))
    case App(e1, e2, loc)         => stepCBN(e1).map(App(_, e2, loc))
    case ToFloat(e, loc)          => stepCBN(e).map(ToFloat(_, loc))
    case ToInt(e, loc)            => stepCBN(e).map(ToInt(_, loc))

    // Values or stuck terms don't step
    case _ => None
  }

}

def fvs[A](e: Stlc[A]): Set[Name] = {
  import Stlc.*

  e match {
    case Var(x, _)       => Set(x)
    case Plus(e1, e2, _) => fvs(e1) union fvs(e2)
    case App(e1, e2, _)  => fvs(e1) union fvs(e2)
    case ToFloat(e, _)   => fvs(e)
    case ToInt(e, _)     => fvs(e)
    case Lam(x, t, e, _) => fvs(e) - x
    case _               => Set.empty
  }
}

// `v` is a value and does not have free variables. It should therefore
// always be fine to substitute in.
def substValue[A](x: Name, e: Stlc[A], v: Stlc[A] & Value): Stlc[A] = e match {

  // Not variables
  case e: (Stlc.SFloat[A] | Stlc.SInt[A] | Stlc.SUnit[A]) => e

  // Variables
  case Stlc.Var(y, _) if x == y => v
  case e: Stlc.Var[A]           => e

  // Builtins
  case Stlc.ToFloat(e, loc) => Stlc.ToFloat(substValue(x, e, v), loc)
  case Stlc.ToInt(e, loc)   => Stlc.ToInt(substValue(x, e, v), loc)

  // Things we might need to substitute in
  case Stlc.Plus(e1, e2, loc) =>
    Stlc.Plus(substValue(x, e1, v), substValue(x, e2, v), loc)
  case Stlc.App(e1, e2, loc) =>
    Stlc.App(substValue(x, e1, v), substValue(x, e2, v), loc)
  case Stlc.Lam(y, _, _, _) if x == y => e
  case Stlc.Lam(y, t, body, loc) => Stlc.Lam(y, t, substValue(x, body, v), loc)
}

// Some arbitrary mapping of numbers to strings
def intToString(x: Int): String =
  def charList(n: Int): List[Char] =
    if n < 26 then List((n % 26 + 'a').toChar)
    else (n % 26 + 'a').toChar :: charList(n / 26)
  charList(x).mkString

// Infinite list of them
val lexi: LazyList[Name] = LazyList.from(0).map(intToString)

// Find the first fresh for names
def freshFor(names: Set[Name]): Name =
  lexi.find(n => !names.contains(n)).getOrElse(names.mkString)

// The general case where the term we substitute in is not necessarily a
// value, and it might be a term with free variables.
def substTerm[A](x: Name, e: Stlc[A], term: Stlc[A]): Stlc[A] = e match {

  // Not variables
  case e: (Stlc.SFloat[A] | Stlc.SInt[A] | Stlc.SUnit[A]) => e

  // Variables
  case Stlc.Var(y, _) if x == y => term
  case e: Stlc.Var[A]           => e

  // Builtins
  case Stlc.ToFloat(e, loc) => Stlc.ToFloat(substTerm(x, e, term), loc)
  case Stlc.ToInt(e, loc)   => Stlc.ToInt(substTerm(x, e, term), loc)

  // Things we might need to substitute in
  case Stlc.Plus(e1, e2, loc) =>
    Stlc.Plus(substTerm(x, e1, term), substTerm(x, e2, term), loc)
  case Stlc.App(e1, e2, loc) =>
    Stlc.App(substTerm(x, e1, term), substTerm(x, e2, term), loc)

  case Stlc.Lam(y, _, _, _) if x == y => e

  case Stlc.Lam(y, t, body, loc) if !fvs(term).contains(y) =>
    Stlc.Lam(y, t, substTerm(x, body, term), loc)

  // Case where the term we are subsituting is open and contains the binder
  // Example: [y/x](\x. x + y) => \a. x + a
  case Stlc.Lam(y, t, body, loc) => {
    val fresh = freshFor(fvs(term) ++ fvs(body) + x + y)
    Stlc.Lam(
      fresh,
      t,
      substTerm(x, substTerm(y, body, Stlc.Var(fresh, loc)), term),
      loc
    )

  }
}
