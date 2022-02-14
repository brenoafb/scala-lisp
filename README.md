# Lisp interpreter in Scala 3

Arithmetic operations are currently implemented.
Run with `sbt run`.

```
sbt run
> (+ 1 2)
PList(List(Atom(+), Num(1), Num(2)))
result: Num(3)
> (+ 1 (* 2 3))
PList(List(Atom(+), Num(1), PList(List(Atom(*), Num(2), Num(3)))))
result: Num(7)
```

Quit with `Ctrl+C`
