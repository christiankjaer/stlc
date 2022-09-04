// Sort of a trivial bidirectional typing thing.

final case class TypeError(message: String, loc: SourceLocation)

// Will check if the type of a term is in `ts`
def check(term: LStlc, ctx: Map[Name, Ty], ts: Set[Ty]): Either[TypeError, Ty] =
  infer(term, ctx).flatMap(t =>
    if ts(t) then Right(t)
    else
      Left(
        TypeError(
          s"found term of type $t, expected one of ${ts.mkString(" or ")}",
          term.get
        )
      )
  )

// Will yield the type of `term`
def infer(term: LStlc, ctx: Map[Name, Ty]): Either[TypeError, Ty] = term match {
  case Stlc.SInt(_, _)   => Right(Ty.Int)
  case Stlc.SUnit(_)     => Right(Ty.Unit)
  case Stlc.SFloat(_, _) => Right(Ty.Float)
  case Stlc.Var(x, loc) =>
    ctx.get(x).toRight(TypeError(s"Variable $x not defined", loc))
  case Stlc.Plus(e1, e2, _) =>
    for {
      t <- check(e1, ctx, Set(Ty.Int, Ty.Float))
      _ <- check(e2, ctx, Set(t))
    } yield t
  case Stlc.App(e1, e2, loc) =>
    infer(e1, ctx).flatMap {
      case Ty.Arrow(t1, t2) => check(e2, ctx, Set(t1)).map(_ => t2)
      case t => Left(TypeError(s"Expected function, got term of type $t", loc))
    }
  case Stlc.Lam(x, tx, body, _) =>
    infer(body, ctx + (x -> tx)).map(Ty.Arrow(tx, _))
}
