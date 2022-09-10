class StlcSuite extends munit.FunSuite {
  import Stlc.*
  val loc = SourceLocation(0,0)
  test("infer") {
    assertEquals(
      infer(Lam("x", Ty.Int, Var("x", loc), loc), Map.empty),
      Right(Ty.Arrow(Ty.Int, Ty.Int))
    )
    assert(
      infer(Lam("y", Ty.Int, Var("x", loc), loc), Map.empty).isLeft
    )
    assertEquals(
      infer(
        App(Lam("x", Ty.Int, Var("x", loc), loc), SInt(10, loc), loc),
        Map.empty
      ),
      Right(Ty.Int)
    )
    assertEquals(
      infer(Plus(SInt(10, loc), SInt(20, loc), loc), Map.empty),
      Right(Ty.Int)
    )
    assertEquals(
      infer(Plus(SFloat(10, loc), SFloat(20, loc), loc), Map.empty),
      Right(Ty.Float)
    )
    assert(
      infer(Plus(SFloat(10, loc), SInt(20, loc), loc), Map.empty).isLeft
    )
    assert(
      infer(Var("x", loc), Map.empty).isLeft
    )
    assertEquals(
      infer(Var("x", loc), Map("x" -> Ty.Int)),
      Right(Ty.Int)
    )
  }

  test("subst") {
    assertEquals(substValue("x", Var("x", ()), SInt(10, ())), SInt(10, ()))
    assertEquals(substValue("y", Var("x", ()), SInt(10, ())), Var("x", ()))
    assertEquals(
      substValue("x", Lam("x", Ty.Int, Var("x", ()), ()), SInt(10, ())),
      Lam("x", Ty.Int, Var("x", ()), ())
    )
    assertEquals(
      substValue("y", Lam("x", Ty.Int, Var("y", ()), ()), SInt(10, ())),
      Lam("x", Ty.Int, SInt(10, ()), ())
    )
  }

  test("substTerm") {
    assertEquals(substTerm("x", Var("x", ()), SInt(10, ())), SInt(10, ()))
    assertEquals(substTerm("y", Var("x", ()), SInt(10, ())), Var("x", ()))
    assertEquals(
      substTerm("x", Lam("x", Ty.Int, Var("x", ()), ()), SInt(10, ())),
      Lam("x", Ty.Int, Var("x", ()), ())
    )
    assertEquals(
      substTerm("y", Lam("x", Ty.Int, Var("y", ()), ()), SInt(10, ())),
      Lam("x", Ty.Int, SInt(10, ()), ())
    )
  }

  test("step") {
    assertEquals(step(Plus(SInt(10, ()), SInt(20, ()), ())), Some(SInt(30, ())))
    assertEquals(step(Plus(SInt(10, ()), SFloat(20, ()), ())), None)
    assertEquals(step(Plus(SInt(10, ()), SUnit(()), ())), None)

    val lam = Lam("x", Ty.Int, Var("x", ()), ())
    assertEquals(step(lam), None)

    // Apply a non-value
    assertEquals(
      step(App(lam, Plus(SInt(10, ()), SInt(20, ()), ()), ())),
      Some(App(lam, SInt(30, ()), ()))
    )
  }

  test("evaluate") {

    assert(evaluate(Plus(SInt(10, loc), SFloat(10, loc), loc), 11).isLeft)
    assertEquals(evaluate(Plus(SInt(10, loc), SInt(10, loc), loc), 11), Right(SInt(20, loc), 10))
    assertEquals(
      evaluate(App(Lam("x", Ty.Int, Var("x", loc), loc), SInt(10, loc), loc), 10),
      Right(SInt(10, loc), 9)
    )
  }
}
