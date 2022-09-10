import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import cats.data.Writer
import cats.data.StateT
import cats.parse.Parser
import com.monovore.decline.*
import com.monovore.decline.effect.*
import cats.data.Validated

type Stepper = LStlc => Option[LStlc]

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

def trace(
    term: LStlc,
    gas: Int,
    step: Stepper
): Either[String, TracedExecution] = {

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

def evaluate(
    term: LStlc,
    gas: Int,
    step: Stepper
): Either[String, (LStlc, Int)] = {
  def go(term: LStlc, gas: Int): (LStlc, Int) =
    if term.isInstanceOf[Value] || gas <= 0 then (term, gas)
    else
      step(term) match {
        case None          => (term, gas)
        case Some(newTerm) => go(newTerm, gas - 1)
      }

  infer(term, Map.empty).bimap(showTypeError, _ => go(term, gas))
}

object Main
    extends CommandIOApp(
      name = "stlc",
      header = "Simply Typed Lambda Calculus playground"
    ) {

  def interpTrace(
      term: LStlc,
      gas: Int,
      step: Stepper
  ): Either[String, String] =
    trace(term, gas, step).map { res =>
      val rem = s"Used gas: ${gas - res.remainingGas}"
      val tr = res.trace.map(t => s"=> $t").mkString("\n")
      s"$tr\n$rem"
    }

  // Toplevel state
  final case class TL(gas: Int, ev: Stepper, trace: Boolean)

  def interp(state: TL, term: String): String =
    parse(term)
      .leftMap(showParseErrors)
      .flatMap(term =>
        if state.trace then interpTrace(term, state.gas, state.ev)
        else evaluate(term, state.gas, state.ev).map(_(0).toString)
      )
      .merge


  val gasRegex = ":set gas (\\d+)".r

  // Crude toplevel. Transformers in scala suck.
  def interact: StateT[IO, TL, Boolean] =
    StateT.liftF(IO.print("> ")) >>
      StateT.liftF(IO.readLine).flatMap {
        case ":q" => StateT.pure(true)

        case ":set notrace" =>
          StateT.modify[IO, TL](_.copy(trace = false)) >>
            StateT.liftF(IO.println("disabling tracing")).as(false)

        case ":set trace" =>
          StateT.modify[IO, TL](_.copy(trace = true)) >>
            StateT.liftF(IO.println("enabling tracing")).as(false)

        case gasRegex(g) =>
          StateT.modify[IO, TL](_.copy(gas = g.toInt)) >>
            StateT.liftF(IO.println(s"setting gas to $g")).as(false)

        case ":set eval cbn" =>
          StateT.modify[IO, TL](_.copy(ev = stepCBN[SourceLocation])) >>
            StateT
              .liftF(IO.println(s"setting evaluation strategy to cbn"))
              .as(false)

        case ":set eval cbv" =>
          StateT.modify[IO, TL](_.copy(ev = stepCBV[SourceLocation])) >>
            StateT
              .liftF(IO.println(s"setting evaluation strategy to cbv"))
              .as(false)

        case term =>
          StateT
            .get[IO, TL]
            .flatMap(state => StateT.liftF(IO.println(interp(state, term))))
            .as(false)

      }

  override def main: Opts[IO[ExitCode]] = {

    val gasOpts =
      Opts.option[Int]("gas", "how much gas to provide the interpreter")
    val traceOpts =
      Opts
        .flag("no_trace", "if the interpreter should print out the trace")
        .orFalse
        .map(!_)
    val strategyOpts: Opts[Stepper] = Opts
      .option[String](
        "evaluation",
        "choose the evaluation strategy (cbn, cbv). Default cbv"
      )
      .mapValidated {
        case "cbn" => stepCBN[SourceLocation].validNel[String]
        case "cbv" => stepCBV[SourceLocation].validNel[String]
        case _     => "Expected cbn or cbv for evaluation strategy".invalidNel
      }
      .withDefault(stepCBV[SourceLocation])

    (gasOpts, strategyOpts, traceOpts)
      .mapN(TL.apply)
      .map(interact.iterateUntil(x => x).run(_).as(ExitCode.Success))

  }

}
