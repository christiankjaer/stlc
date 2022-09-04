// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class StlcSuite extends munit.FunSuite {
  import Stlc.*
  test("infer") {
    assertEquals(
      infer(Lam("x", Ty.Int, Var("x")), Map.empty),
      Right(Ty.Arrow(Ty.Int, Ty.Int))
    )
    assert(
      infer(Lam("y", Ty.Int, Var("x")), Map.empty).isLeft
    )
    assertEquals(
      infer(App(Lam("x", Ty.Int, Var("x")), SInt(10)), Map.empty),
      Right(Ty.Int)
    )
    assertEquals(
      infer(Plus(SInt(10), SInt(20)), Map.empty),
      Right(Ty.Int)
    )
    assertEquals(
      infer(Plus(SFloat(10), SFloat(20)), Map.empty),
      Right(Ty.Float)
    )
    assert(
      infer(Plus(SFloat(10), SInt(20)), Map.empty).isLeft
    )
    assert(
      infer(Var("x"), Map.empty).isLeft
    )
    assertEquals(
      infer(Var("x"), Map("x" -> Ty.Int)),
      Right(Ty.Int)
    )
  }

  test("subst") {
    assertEquals(subst("x", Var("x"), SInt(10)), SInt(10))
    assertEquals(subst("y", Var("x"), SInt(10)), Var("x"))
    assertEquals(
      subst("x", Lam("x", Ty.Int, Var("x")), SInt(10)),
      Lam("x", Ty.Int, Var("x"))
    )
    assertEquals(
      subst("y", Lam("x", Ty.Int, Var("y")), SInt(10)),
      Lam("x", Ty.Int, SInt(10))
    )
  }

  test("step") {
    assertEquals(step(Plus(SInt(10), SInt(20))), SInt(30))
  }

  test("evaluate") {

    assert(evaluate(Plus(SInt(10), SFloat(10)), 11).isLeft)
    assertEquals(evaluate(Plus(SInt(10), SInt(10)), 11), Right(SInt(20), 10))
    assertEquals(
      evaluate(App(Lam("x", Ty.Int, Var("x")), SInt(10)), 10),
      Right(SInt(10), 9)
    )

  }
}
