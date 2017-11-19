<!-- {{{1 -->

    File        : TODO.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2017-11-19

    Copyright   : Copyright (C) 2017  Felix C. Stegerman
    Version     : v0.0.1

<!-- }}}1 -->

## Misc

* keywords & description (README, github, setup.py)
* ~~travis~~
* pypi (+ gpg?)
* ack TODO
* 子猫 in README

* other language features?
* Features from Clojure, Haskell, Factor, Python, Ruby, ...
* streams!

* agument order!
* exception handling (use with!)
* caveat: refc fails for recursive scope/block.
* protocol, multi, classes?

## Read

* , whitespace
* ; comment
* `.foo -> :foo **getattr**`
* `!foo -> .foo **call**`
* `foo -> 'foo **call**`
* isident()
* unicodeAlNum + ` ~!@$%^&*-_=+|<>/?(){}[] `
* ' or ! or : not at first
* full ident w/ .
* not all digits, not sole (){}[]
* :"kwd"
* Token, Ident, ...
* istoken()?
* /"regex"

```
x ``foo y z
```

* \uXXXX, 0xXXX, """..."""

## Eval

* 'if is ok -> builtin on stack
* eval(code[, stack, scope])

* () [] {}, ...
* tail rec `eval(... call, rec=True) -> True`

### Scope

* :x variable
* :x nonlocal ^ :x global
* .vars = set()
* .globals, .nonlocals
* .extend({}), .get(), .set(), .global(), .nonlocal("x"), ...
* .get("x.y.z")? namespace = Scope?

## REPL

* what to do w/ KeyboardInterrupt?
* -e + args?

## Data Types

* nil
* bool (T/F)
* int, float, str
* Kwd
* list, dict, Range

* regex (from re)
* Kwd (subclass of str w/ repr :foo / :"foo bar")
* Range (subclass of range), RangeIncl, RangeExcl (.., ...)?
* Rec: Foo type, Foo{, Foo(; namedtuple + repr(), str()
* Block (.params, .code, .scope) [ ]
* RawBlock (.params?, .code) '[ ]
* KonekoError, StackUnderFlowError, ...

### Blocks

* Ident -> Kwd, RawBlock -> List?
* `__call__()` | args -> stack
* call()
* new .extend()ed scope

```
call(stack)
  self.eval(self.code, stack, self.scope)

__call__(*args)
  st = list(args)
  self.eval / self.call | .scope etc
  return st

???
```

### __repr__

* Block shows code
* Builtin shows "code"

### Callable

* Block, Dict (k -> v), ...

### Iterators

* each (`.**each**`), `.**iter**`?, ...
* list, map, range, ...
* yield? coroutines? ...
* Map/Filter/Iter needed?

### Methods

* defmethod
* Rec.methods, proxy for interop?

## Primitives (**prim**)

* @defprim (uses @interop?), @defbuiltin, ...
* stack, scope
* push, pop, #args, return
* `[ ] ( ) { } ...`
* call (`**call**`)
* .prim["if"], .prelude["*"], ...

### Namespacing

* import, module, import-from
* `( :->json ) :json import-from`

## Builtins (**builtin**)

* `..`
* if, set!
* clone?
* =, not=, ...
* +, -, `*`, ... (`**add**` etc.)
* print-stack, ...
* or, or?, ...

## Prelude (**prelude**)

* case
* def = set!
* drop, dup, swap, ...
* ask, print, say, ...
* each, filter, map, zip, zip-with, ...
* `->list`, `->rec`, ...
* curry/partial?
* while
* get, set!, nth!, set-nth!
* get-var, set-var!
* get-in, set-in!
* sort, sort!, ...
* nop?

## Streams

* stdin, stdout, `<file`, `>file`, ...
* lines, unlines, ...
* .each_char, .read, ...
* grep, ...
* pipes!

## Shell

* cd, ls, ...

## Other

* data flow!
* macros? block manip? "..." read?
* pattern matching?
* concurrency

* Python interop
* other interop?
* generate bindings?

* check arity?
* full ADTs?
* C++/Haskell/Racket-based compiler/interpreter?
* compile to JS?

* GC vs games? bigbang?
* rules for no GC?

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
