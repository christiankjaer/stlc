import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.syntax.all.*
import cats.Monad
import cats.data.Writer

final case class TracedExecution(
    trace: List[Stlc],
    result: Stlc,
    remainingGas: Int
)

def trace(term: Stlc, gas: Int): Either[TypeError, TracedExecution] = {
  def go(term: Stlc, gas: Int): Writer[List[Stlc], (Stlc, Int)] =
    if term.isInstanceOf[Value] || gas <= 0 then Writer.value((term, gas))
    else Writer.tell(List(term)) >> go(step(term), gas - 1)

  infer(term, Map.empty) match {
    case Left(err) => Left(s"Type error: $err")
    case Right(_) => {
      val ((res, remainingGas), trace) = go(term, gas).listen.value
      Right(TracedExecution(trace, res, remainingGas))
    }
  }
}

def evaluate(term: Stlc, gas: Int): Either[TypeError, (Stlc, Int)] =
  trace(term, gas).map(t => (t.result, t.remainingGas))

object Main extends IOApp {

  def interp(s: String, gas: Int): String =
    parse(s).flatMap(trace(_, gas)) match {
      case Right(res) => {
        val rem = s"Remaining gas: ${res.remainingGas}"
        val tr = res.trace.map(t => s"=> $t\n").mkString
        val r = s"=> ${res.result}"
        s"$tr$r\n$rem"
      }
      case Left(e) => e
    }

  def interact(gas: Int): IO[Boolean] =
    IO.print("> ") >> IO.readLine.flatMap(s =>
      if s == ":q" then IO.pure(true)
      else IO.println(interp(s, gas)) >> IO.pure(false)
    )

  override def run(args: List[String]): IO[ExitCode] = {

    val gas = args.get(0).map(_.toInt).get

    interact(gas).iterateUntil(x => x).as(ExitCode.Success)

  }

}
