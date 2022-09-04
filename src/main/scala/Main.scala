import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import cats.data.Writer
import cats.parse.Parser

final case class TracedExecution(
    trace: Seq[LStlc],
    result: LStlc,
    remainingGas: Int
)

def showTypeError(err: TypeError): String = {
  val pointer = Seq.fill(err.loc.start + 2)(" ").mkString + Seq
    .fill(err.loc.end - err.loc.start)("~")
    .mkString
  s"$pointer\nType error: ${err.message}"
}

def showParseErrors(err: Parser.Error): String = {
  val pointer = Seq.fill(err.failedAtOffset)("-").mkString + "--^"
  s"$pointer\nParse error"
}

def trace(term: LStlc, gas: Int): Either[String, TracedExecution] = {

  def go(term: LStlc, gas: Int): Writer[Seq[LStlc], (LStlc, Int)] =
    Writer.tell(Seq(term)) >>
      (if gas <= 0 then Writer.value((term, gas))
       else
         step(term) match {
           case None          => Writer.value((term, gas)) // Stuck or value
           case Some(newTerm) => go(newTerm, gas - 1)
         }
      )

  infer(term, Map.empty).bimap(
    showTypeError,
    { _ =>
      val ((res, remainingGas), trace) = go(term, gas).listen.value
      TracedExecution(trace, res, remainingGas)
    }
  )
}

def evaluate(term: LStlc, gas: Int): Either[String, (LStlc, Int)] = {
  def go(term: LStlc, gas: Int): (LStlc, Int) =
    if term.isInstanceOf[Value] || gas <= 0 then (term, gas)
    else
      step(term) match {
        case None          => (term, gas)
        case Some(newTerm) => go(newTerm, gas - 1)
      }

  infer(term, Map.empty).bimap(showTypeError, _ => go(term, gas))
}

object Main extends IOApp {

  def interpTrace(term: LStlc, gas: Int): Either[String, String] =
    trace(term, gas).map { res =>
      val rem = s"Used gas: ${gas - res.remainingGas}"
      val tr = res.trace.map(t => s"=> $t").mkString("\n")
      s"$tr\n$rem"
    }

  def interp(s: String, gas: Int, traceExecution: Boolean): String =
    parse(s)
      .leftMap(showParseErrors)
      .flatMap(term =>
        if traceExecution then interpTrace(term, gas)
        else evaluate(term, gas).map(_(0).toString)
      )
      .merge

  def interact(gas: Int, trace: Boolean): IO[Boolean] =
    IO.print("> ") >> IO.readLine.flatMap(s =>
      if s == ":q" then IO.pure(true)
      else IO.println(interp(s, gas, trace)) >> IO.pure(false)
    )

  override def run(args: List[String]): IO[ExitCode] = {

    val gas = args.get(0).flatMap(_.toIntOption).getOrElse(1000000)
    val trace = args.get(1).map(_ == "trace").exists(x => x)

    interact(gas, trace).iterateUntil(x => x).as(ExitCode.Success)

  }

}
