<!--

* strict -> streams
* types -> ???
* dynamic scope???
* mark I/O as "dirty" to allow for optimizing code that is known to
  be referentially transparent? vs clojure?
* sh/streams/pipes (cf. Haskell Pipes)

TODO: arith errors

quote lists, blocks? macros?

* Ident
* Raw*
* Range? ...
* Namespace/Scope, Stack, ...

* linked list vs dynamic array ???

* arity? args + body
* ..., variable arity, apply, ...
* curry, partial?
* show-stack + reverse order

ADTs ???

```
>>> 1 `+ 2                                  ; shift token (use w/ caution)
3
>>> 1 2 +                                   ; desugared
3

>>> [[ 1 2 3 + + ]]                         ; grouped expression
6
```

* !foo( ... ) ?!

* reader vs eval vs primitive vs builtin vs prelude vs stdlib
* [[ ]] is not reader-only sugar

* lib/
* DSLs, Shell, FFI, ...

* match by partial spec like ocaml!
* store/match how?

[ ] <==> [ drop () ] if empty?

* look at notes and old python stuff
* haskell features (Text, not String)

* dyn vars?
* iterators?

### Thesis

(i.e. `thesis/*`)

[![CC-BY-SA](https://licensebuttons.net/l/by-sa/4.0/88x31.png)](https://creativecommons.org/licenses/by-sa/4.0/)

-->
