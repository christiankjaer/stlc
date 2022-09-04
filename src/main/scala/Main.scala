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
    else
      step(term) match {
        case None          => Writer.value((term, gas)) // Stuck
        case Some(newTerm) => Writer.tell(List(term)) >> go(newTerm, gas - 1)
      }

  infer(term, Map.empty) match {
    case Left(err) => Left(s"Type error: $err")
    case Right(_) => {
      val ((res, remainingGas), trace) = go(term, gas).listen.value
      Right(TracedExecution(trace, res, remainingGas))
    }
  }
}

def evaluate(term: Stlc, gas: Int): Either[TypeError, (Stlc, Int)] = {
  def go(term: Stlc, gas: Int): (Stlc, Int) =
    if term.isInstanceOf[Value] || gas <= 0 then (term, gas)
    else
      step(term) match {
        case None          => (term, gas)
        case Some(newTerm) => go(newTerm, gas - 1)
      }

  infer(term, Map.empty) match {
    case Left(err) => Left(s"Type error: $err")
    case Right(_)  => Right(go(term, gas))
  }
}

object Main extends IOApp {

  def interpTrace(term: Stlc, gas: Int): Either[TypeError, String] =
    trace(term, gas).map { res =>
      val rem = s"Remaining gas: ${res.remainingGas}"
      val tr = res.trace.map(t => s"$t \n=> ").mkString
      val r = s"${res.result}"
      s"$tr$r\n$rem"
    }

  def interp(s: String, gas: Int, traceExecution: Boolean): String =
    parse(s)
      .flatMap(term =>
        if traceExecution then interpTrace(term, gas)
        else evaluate(term, gas).map(_(0).toString)
      )
      .fold(x => x, x => x)

  def interact(gas: Int, trace: Boolean): IO[Boolean] =
    IO.print("> ") >> IO.readLine.flatMap(s =>
      if s == ":q" then IO.pure(true)
      else IO.println(interp(s, gas, trace)) >> IO.pure(false)
    )

  override def run(args: List[String]): IO[ExitCode] = {

    val gas = args.get(0).map(_.toInt).get
    val trace = args.get(1).map(_ == "trace").exists(x => x)

    interact(gas, trace).iterateUntil(x => x).as(ExitCode.Success)

  }

}
