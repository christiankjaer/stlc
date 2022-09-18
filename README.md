# STLC with a small step semantics

## Usage

Requires [sbt](https://www.scala-sbt.org/)

```
Usage: stlc --gas <integer> [--evaluation <string>] [--no_trace]

Simply Typed Lambda Calculus playground

Options and flags:
    --help
        Display this help text.
    --gas <integer>
        how much gas to provide the interpreter
    --evaluation <string>
        choose the evaluation strategy (cbn, cbv). Default cbv
    --no_trace
        if the interpreter should not print out the trace
```

### Syntax

Application: `f a`

Lambda: `\x: type => body`

The rest is not surprising.

### Commands

`:set gas <gas>`

`:set <trace|notrace>`

`:set eval <cbv|cbn>`

`:t <term>`


### Example

```
$ sbt
sbt:stlc> run --gas 1000
[info] running Main --gas 1000

> 1 + ((\x: int => x + 1) 1) + 3
=> ((1 + ((\x: int => (x + 1)) 1)) + 3)
=> ((1 + (1 + 1)) + 3)
=> ((1 + 2) + 3)
=> (3 + 3)
=> 6
Used gas: 4

> (\x: int => x + 10)(\x: unit => ())
                      ~~~~~~~~~~~~~~
Type error: found term of type (unit -> unit), expected one of int

> :set gas 2
setting gas to 2

> 1+2+3+4+5+6+7
=> ((((((1 + 2) + 3) + 4) + 5) + 6) + 7)
=> (((((3 + 3) + 4) + 5) + 6) + 7)
=> ((((6 + 4) + 5) + 6) + 7)
Used gas: 2

> :set gas 1000

> (\x: int => x + x + x)(1 + 2 + 3 + 4 + 5)
=> ((\x: int => ((x + x) + x)) ((((1 + 2) + 3) + 4) + 5))
=> ((\x: int => ((x + x) + x)) (((3 + 3) + 4) + 5))
=> ((\x: int => ((x + x) + x)) ((6 + 4) + 5))
=> ((\x: int => ((x + x) + x)) (10 + 5))
=> ((\x: int => ((x + x) + x)) 15)
=> ((15 + 15) + 15)
=> (30 + 15)
=> 45
Used gas: 7

> :set eval cbn
setting evaluation strategy to cbn

> (\x: int => x + x + x)(1 + 2 + 3 + 4 + 5)
=> ((\x: int => ((x + x) + x)) ((((1 + 2) + 3) + 4) + 5))
=> ((((((1 + 2) + 3) + 4) + 5) + ((((1 + 2) + 3) + 4) + 5)) + ((((1 + 2) + 3) + 4) + 5))
=> (((((3 + 3) + 4) + 5) + ((((1 + 2) + 3) + 4) + 5)) + ((((1 + 2) + 3) + 4) + 5))
=> ((((6 + 4) + 5) + ((((1 + 2) + 3) + 4) + 5)) + ((((1 + 2) + 3) + 4) + 5))
=> (((10 + 5) + ((((1 + 2) + 3) + 4) + 5)) + ((((1 + 2) + 3) + 4) + 5))
=> ((15 + ((((1 + 2) + 3) + 4) + 5)) + ((((1 + 2) + 3) + 4) + 5))
=> ((15 + (((3 + 3) + 4) + 5)) + ((((1 + 2) + 3) + 4) + 5))
=> ((15 + ((6 + 4) + 5)) + ((((1 + 2) + 3) + 4) + 5))
=> ((15 + (10 + 5)) + ((((1 + 2) + 3) + 4) + 5))
=> ((15 + 15) + ((((1 + 2) + 3) + 4) + 5))
=> (30 + ((((1 + 2) + 3) + 4) + 5))
=> (30 + (((3 + 3) + 4) + 5))
=> (30 + ((6 + 4) + 5))
=> (30 + (10 + 5))
=> (30 + 15)
=> 45
Used gas: 15

> :q
[success] Total time: 96 s (01:36), completed Sep 10, 2022, 1:56:13 PM
```

## Design

- Scala 3 enums for syntax and types. Subtype for values. Intersection types to check for a variable `Value & Stlc`.
- Simple type inference with bidirectional typing. Make errors easy to report.
- Parser with simple syntax:
    * Lambda: `\var: type => body`
    * Application: f x
    * ...
- Tracking source locations is a bit tedious. Maybe it's nicer to remove them before stepping, but then replacing with `()` is still annoying in pattern matching.
- Capture avoiding substitution and stepping function.
- REPL that prints out traces.
