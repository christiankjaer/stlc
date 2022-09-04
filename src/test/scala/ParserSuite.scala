class ParserSuite extends munit.FunSuite {
  import Stlc.*
  test("parse type") {
    def parseTy(s: String): Option[Ty] =
      parseType.parseAll(s).toOption

    assertEquals(parseTy("int"), Some(Ty.Int))
    assertEquals(parseTy("float"), Some(Ty.Float))
    assertEquals(parseTy("unit"), Some(Ty.Unit))

    assertEquals(parseTy("(int)"), Some(Ty.Int))
    assertEquals(parseTy("float -> int"), Some(Ty.Arrow(Ty.Float, Ty.Int)))
    assertEquals(
      parseTy("float -> int -> unit"),
      Some(Ty.Arrow(Ty.Float, Ty.Arrow(Ty.Int, Ty.Unit)))
    )
    assertEquals(
      parseTy("(float -> int) -> unit"),
      Some(Ty.Arrow(Ty.Arrow(Ty.Float, Ty.Int), Ty.Unit))
    )
  }

  test("parse base") {
    assertEquals(parse("10"), Right(SInt(10)))
    assertEquals(parse("12.12"), Right(SFloat(12.12)))
    assertEquals(parse("12.0"), Right(SFloat(12.0)))
    assertEquals(parse("()"), Right(SUnit))
  }

  test("parse plus") {
    assertEquals(parse("10 + 12"), Right(Plus(SInt(10), SInt(12))))
    assertEquals(
      parse("1 + 2 + 3"),
      Right(Plus(Plus(SInt(1), SInt(2)), SInt(3)))
    )
    assertEquals(
      parse("1 + (2 + 3)"),
      Right(Plus(SInt(1), Plus(SInt(2), SInt(3))))
    )
  }

  test("parse lambda") {
    assertEquals(
      parse("\\foo:int=>foo+20"),
      Right(Lam("foo", Ty.Int, Plus(Var("foo"), SInt(20))))
    )

    assertEquals(
      parse("\\ foo : int => foo + 20"),
      Right(Lam("foo", Ty.Int, Plus(Var("foo"), SInt(20))))
    )
  }

  test("parse app") {
    assertEquals(parse("()()"), Right(App(SUnit, SUnit)))
    assertEquals(parse("()()()"), Right(App(SUnit, App(SUnit, SUnit))))
    assertEquals(parse("(()())()"), Right(App(App(SUnit, SUnit), SUnit)))
  }
}
