# STLC with a small step semantics

## Usage

the main function takes a gas argument and gives you a REPL that will print out the small step trace.

```
$ sbt
sbt:stlc> run 100 trace
[info] running Main 100 trace
> 1 + ((\x: int => x + 1) 1) + 3
=> ((1 + (1 + 1)) + 3)
=> ((1 + 2) + 3)
=> (3 + 3)
=> 6
Remaining gas: 96

> (\x: int => x + 10)(\x: unit => ())
                      ~~~~~~~~~~~~~~
Type error: found type (unit -> unit), expected one of int

> :q
sbt:stlc>
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
