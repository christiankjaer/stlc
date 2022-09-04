# STLC with a small step semantics

## Usage

the main function takes a gas argument and gives you a REPL that will print out the small step trace.

```
$ sbt
sbt:stlc> run 10
[info] running Main 10
> (\x: int => x + 10) 5
=> (\x: int => (x + 10) 5)
=> (5 + 10)
=> 15
Remaining gas: 8
```

## Design

- Scala 3 enums for syntax and types. Subtype for values.
- Simple type inference with bidirectional typing.
- Parser with simple syntax.
- Capture avoiding substitution and stepping function.
- REPL that prints out traces.
