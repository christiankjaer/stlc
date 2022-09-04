import cats.parse.Rfc5234.{wsp, alpha, digit}
import cats.parse.{Parser0, Parser => P, Numbers}
import cats.syntax.all.*

val whitespace: P[Unit] = P.charIn(" \t\r\n").void
val whitespaces0: Parser0[Unit] = whitespace.rep0.void
def grouped[T](p: P[T]): P[T] =
  p.between(token(P.char('(')), token(P.char(')')))

def token[T](p: P[T]): P[T] = p.surroundedBy(whitespaces0)

val parseType: P[Ty] = P.recursive[Ty] { pt =>

  val parseBase: P[Ty] = token(P.string("int").as(Ty.Int)) | token(
    P.string("float").as(Ty.Float)
  ) | token(P.string("unit")).as(Ty.Unit) | grouped(pt)

  val parseEnd: Parser0[Ty => Ty] = (token(P.string("->")) *> pt)
    .map(t => ty => Ty.Arrow(ty, t)) | P.pure[Ty => Ty](x => x)

  (parseBase ~ parseEnd).map { case (s, e) => e(s) }
}

val parseTerm: P[Stlc] = P.recursive[Stlc] { pt =>

  val unit = token(P.string("()")).as(Stlc.SUnit)

  val name: P[Name] = token(alpha.rep.string)

  val frac = (P.char('.') ~ Numbers.digits).string

  val number = token(Numbers.signedIntString ~ frac.?).map {
    case (p, Some(s)) => Stlc.SFloat((p + s).toDouble)
    case (p, None)    => Stlc.SInt(p.toInt)
  }

  val lambda =
    ((token(P.char('\\')) *> name) ~ (token(P.char(':')) *> parseType) ~ (token(
      P.string("=>")
    ) *> pt)).map { case ((x, t), e) =>
      Stlc.Lam(x, t, e)
    }

  val term =
    grouped(pt).backtrack | lambda | unit | number | name.map(Stlc.Var.apply)

  val plus = term
    .repSep(token(P.char('+')))
    .map(x => x.tail.foldLeft(x.head)(Stlc.Plus.apply))

  plus.rep.map(x => x.init.foldRight(x.last)(Stlc.App.apply))
}

def parse(s: String): Either[String, Stlc] = {
  (parseTerm <* whitespaces0 <* P.end)
    .parseAll(s)
    .leftMap(e => s"Parse error: ${e.toString}")

}
