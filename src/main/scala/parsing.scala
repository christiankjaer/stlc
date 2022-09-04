import cats.parse.Rfc5234.{alpha, digit}
import cats.parse.{Parser0, Parser => P, Numbers}
import cats.syntax.all.*

val whitespace: P[Unit] = P.charIn(" \t\r\n").void
val whitespaces0: Parser0[Unit] = whitespace.rep0.void

def grouped[T](p: P[T]): P[T] =
  p.between(token(P.char('(')), token(P.char(')')))

def token[T](p: P[T]): P[T] = p.surroundedBy(whitespaces0)
def locToken[T](p: P[T]): P[(T, SourceLocation)] =
  (P.index.with1 ~ p ~ P.index)
    .map { case ((start, t), end) => (t, SourceLocation(start, end)) }
    .surroundedBy(whitespaces0)

val parseType: P[Ty] = P.recursive[Ty] { pt =>

  val parseBase: P[Ty] = token(P.string("int").as(Ty.Int)) | token(
    P.string("float").as(Ty.Float)
  ) | token(P.string("unit")).as(Ty.Unit) | grouped(pt)

  // Trick for left recursion
  val parseEnd: Parser0[Ty => Ty] = (token(P.string("->")) *> pt)
    .map(t => ty => Ty.Arrow(ty, t)) | P.pure[Ty => Ty](x => x)

  // <*> in haskell. Is not good for parsers so we do this
  // manual thing
  (parseBase ~ parseEnd).map { case (s, e) => e(s) }
}

val parseTerm: P[LStlc] = P.recursive[LStlc] { pt =>

  val unit = locToken(P.string("()")).map(i => Stlc.SUnit(i._2))

  val name: P[(Name, SourceLocation)] =
    locToken((alpha ~ (alpha | digit).rep0).string)

  val frac = (P.char('.') ~ Numbers.digits).string

  val number = locToken(Numbers.signedIntString ~ frac.?).map {
    case ((p, Some(s)), loc) => Stlc.SFloat((p + s).toDouble, loc)
    case ((p, None), loc)    => Stlc.SInt(p.toInt, loc)
  }

  // Left associative hack thing
  def leftThing(p: P[LStlc], sep: Option[Parser0[Any]])(
      binop: (LStlc, LStlc, SourceLocation) => LStlc
  ): P[LStlc] =
    sep
      .map(p.repSep)
      .getOrElse(p.rep)
      .map(x =>
        x.tail.foldLeft(x.head)((x, y) => binop(x, y, x.get.join(y.get)))
      )

  val lambda =
    ((token(P.index.with1 <* P.char('\\')) ~ name) ~ (token(
      P.char(':')
    ) *> parseType) ~ (token(P.string("=>")) *> pt))
      .map { case (((loc, x), t), e) =>
        Stlc.Lam(x._1, t, e, SourceLocation(loc, e.get.end))
      }

  val term =
    grouped(pt).backtrack | lambda | unit | number | name.map(Stlc.Var.apply)

  val app = leftThing(term, None)(Stlc.App.apply)

  leftThing(app, Some(token(P.char('+'))))(Stlc.Plus.apply)
}

def parse(s: String): Either[P.Error, LStlc] = {
  (parseTerm <* whitespaces0 <* P.end)
    .parseAll(s)

}
