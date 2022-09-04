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
----------------------^
Type error: found type (unit -> unit), expected one of int

> :q
[success] Total time: 228 s (03:48), completed Sep 4, 2022, 7:38:19 PM
sbt:stlc>
```

## Design

- Scala 3 enums for syntax and types. Subtype for values.
- Simple type inference with bidirectional typing.
- Parser with simple syntax.
- Capture avoiding substitution and stepping function.
- REPL that prints out traces.
