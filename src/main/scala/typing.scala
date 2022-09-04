// Sort of a trivial bidirectional typing thing.

type TypeError = String

// Will check if the type of a term is in `ts`
def check(term: Stlc, ctx: Map[Name, Ty], ts: Set[Ty]): Either[TypeError, Ty] =
  infer(term, ctx).flatMap(t =>
    if ts(t) then Right(t)
    else Left(s"found type $t, expected one of ${ts.mkString(", ")}")
  )

// Will yield the type of `term`
def infer(term: Stlc, ctx: Map[Name, Ty]): Either[TypeError, Ty] = term match {
  case Stlc.SInt(_)   => Right(Ty.Int)
  case Stlc.SUnit     => Right(Ty.Unit)
  case Stlc.SFloat(_) => Right(Ty.Float)
  case Stlc.Var(x)    => ctx.get(x).toRight(s"$x not defined")
  case Stlc.Plus(e1, e2) =>
    for {
      t <- check(e1, ctx, Set(Ty.Int, Ty.Float))
      _ <- check(e2, ctx, Set(t))
    } yield t
  case Stlc.App(e1, e2) =>
    infer(e1, ctx).flatMap {
      case Ty.Arrow(t1, t2) => check(e2, ctx, Set(t1)).map(_ => t2)
      case t                => Left(s"Expected function type, got $t")
    }
  case Stlc.Lam(x, tx, body) =>
    infer(body, ctx + (x -> tx)).map(Ty.Arrow(tx, _))
}
