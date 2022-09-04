class ParserSuite extends munit.FunSuite {
  def sl(i: Int): SourceLocation = SourceLocation(i, i)
  def sl(i: Int, j: Int): SourceLocation = SourceLocation(i, j)
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
    assertEquals(parse("10"), Right(SInt(10, sl(0, 2))))
    assertEquals(parse("12.12"), Right(SFloat(12.12, sl(0, 5))))
    assertEquals(parse("12.0"), Right(SFloat(12.0, sl(0, 4))))
    assertEquals(parse("()"), Right(SUnit(sl(0, 2))))
  }

  test("parse plus") {
    assertEquals(
      parse("10 + 12"),
      Right(Plus(SInt(10, sl(0, 2)), SInt(12, sl(5, 7)), sl(0, 7)))
    )
    assertEquals(
      parse("1 + 2 + 3"),
      Right(
        Plus(
          Plus(SInt(1, sl(0, 1)), SInt(2, sl(4, 5)), sl(0, 5)),
          SInt(3, sl(8, 9)),
          sl(0, 9)
        )
      )
    )
    assertEquals(
      parse("1 + (2 + 3)"),
      Right(
        Plus(
          SInt(1, sl(0, 1)),
          Plus(SInt(2, sl(5, 6)), SInt(3, sl(9, 10)), sl(5, 10)),
          sl(0, 10)
        )
      )
    )
  }

  test("parse lambda") {
    assertEquals(
      parse("\\foo:int=>foo+20"),
      Right(
        Lam(
          "foo",
          Ty.Int,
          Plus(Var("foo", sl(10, 13)), SInt(20, sl(14, 16)), sl(10, 16)),
          sl(0, 16)
        )
      )
    )

    assertEquals(
      parse("\\ foo : int => foo + 20"),
      Right(
        Lam(
          "foo",
          Ty.Int,
          Plus(Var("foo", sl(15, 18)), SInt(20, sl(21, 23)), sl(15, 23)),
          sl(0, 23)
        )
      )
    )
  }

  test("parse app") {
    assertEquals(
      parse("()()"),
      Right(App(SUnit(sl(0, 2)), SUnit(sl(2, 4)), sl(0, 4)))
    )
    assertEquals(
      parse("()()()"),
      Right(
        App(
          App(SUnit(sl(0, 2)), SUnit(sl(2, 4)), sl(0, 4)),
          SUnit(sl(4, 6)),
          sl(0, 6)
        )
      )
    )
    assertEquals(
      parse("()(()())"),
      Right(
        App(
          SUnit(sl(0, 2)),
          App(SUnit(sl(3, 5)), SUnit(sl(5, 7)), sl(3, 7)),
          sl(0, 7)
        )
      )
    )
  }
}
