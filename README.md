<!-- {{{1 -->

    File        : README.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2019-10-11

    Copyright   : Copyright (C) 2019  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+, LGPLv3+

<!-- }}}1 -->

<!-- TODO: badges -->

[![Build Status](https://travis-ci.org/obfusk/koneko.svg?branch=master)](https://travis-ci.org/obfusk/koneko)
[![GPLv3+](https://img.shields.io/badge/license-GPLv3+-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![LGPLv3+](https://img.shields.io/badge/license-LGPLv3+-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0.html)

## Description

koneko - a concatenative not-quite-lisp for kittens

NB: work in progress.

Koneko is a simple concatenative stack-based programming language with
lisp influences.  It is intended to combine the elegance of the
(point-free) "concatenation is composition" model with the elegance of
lisp-like languages (esp. anonymous functions with named arguments).

### Properties

* concatenative
  - point-free
  - juxtaposition of expressions denotes function composition
* stack-oriented
  - postfix (reverse polish) notation
  - functions consume arguments from the stack
  - functions produce return values on the stack
* lisp-like
  - homoiconic
  - blocks/lambdas (anonymous functions)
  - named arguments/points (lexically scoped)
* functional
  - only immutable data structures
  - does have side effects (I/O)
  - (mostly) strict evaluation
* dynamically, strongly typed

<!--
  * strict -> streams
  * types -> ???
  * dynamic scope???
  * mark I/O as "dirty" to allow for optimizing code that is known to
    be referentially transparent? vs clojure?
  * sh/streams/pipes (cf. Haskell Pipes)
-->

## Hello World

```bash
$ koneko -e '"Hello, World!" say'
hello, World!
```

```bash
$ koneko
>>> "Hello, World!" say
Hello, World!
>>> ^D
```

## The Language

A program is a sequence of tokens.  Each token represents a function
that takes a scope and a stack and returns an (updated) stack.

Juxtaposition (concatenation) of tokens denotes function composition.
Some tokens, like list literals, are nested.

Evaluating any data type literal results in pushing a corresponding
value of its type onto the stack.

All data types are immutable.

The syntax and semantics of concatenative languages form the algebraic
structure of a monoid [1].  We expect the same to be true for koneko,
and will update this paragraph when we confirm this.

### Type System

Koneko is strongly typed.  For now, it will use dynamic typing and
allow blocks to be of variable arity.  Optional (static) type and/or
arity checking may be added in the future.

### Comments & Whitespace

```
; comments start with a semicolon and end at the end of the line

1 2 +         ; tokens are separated by whitespace
1 2 +, 2 3 +  ; commas are whitespace
```

NB: the repl will print the top of the stack (unless empty) after it
evaluates a line, unless the line starts with a `,`.

### Ident(ifier)s

Any contiguous sequence of one or more letters, numbers, brackets (any
of `(){}[]`), special characters (any of `~@$%^&*-_=+|<>/?` and `'!:`)
is an identifier if it:

* does not start with any of `'!:` or end with `:`;
* is not a single bracket or `()`;
* is not a valid integer literal, floating point literal, or `nil`;
* and does not end with an opening bracket.

Unquoted identifiers are calls; i.e. the identifier is looked up in
the current scope, the value found is pushed onto the stack, and the
top of the stack is `call`ed.

### Quoting

Quoted identifiers ("quots") omit the `call`; the identifier is looked
up and the value found is pushed onto the stack.

```koneko
>>> 1 2 +                                   ; push and call "+"
3
>>> 'not                                    ; push "not"
#<primitive:not>
>>> 1 2 '+ call                             ; push "+", then call it
3
```

<!-- quote lists, blocks? macros? -->

### Naming Things

Identifiers refer to either named arguments or definitions in modules.

When an identifier is called or quoted, it is looked up in the
following order:

* primitives;
* the current scope and any parent scope(s);
* the module the current scope belongs to;
* builtins;
* the prelude.

```koneko
>>> , :answer 42 def            ; define a constant (in the current module)
>>> , :inc [ 1 + ] def          ; define a function
>>> 'answer inc
43
```

The default module is `__main__`; primitives, builtins, and the
prelude are `__prim__`, `__bltn__`, and `__prld__` respectively.

### Primitive Data Types

i.e. nil, bool, int, float, str, kwd

```koneko
>>> nil             ; nothing to see here...
nil

>>> #t              ; true
#t
>>> #f              ; false (nil and #f are falsy, everything else is truthy)
#f

>>> 42              ; integer
42
>>> 0x20
32

>>> 3.14            ; floating point
3.14
```

```koneko
>>> "spam & eggs"   ; string
"spam & eggs"
>>> "\u732bs"
"猫s"
>>> "\x20"
" "
```

```koneko
>>> :key-word       ; keyword
:key-word
>>> :"keyword that is not a valid identifier"
:"keyword that is not a valid identifier"
```

<!--

```
>>> /cat|kitten/    ; regex -- TODO
/cat|kitten/
```

* Ident
* Raw*
* Range? ...
* Namespace/Scope, Stack, ...

-->

### Pairs, Lists and Dicts

NB: list literals have the head on the left.

```koneko
>>> :answer 42 =>                 ; key/value pair
:answer 42 =>
>>> ,dup
>>> .key                          ; field access
:answer
>>> ,drop
>>> .value
42

>>> ( 1 2 :foo 4 )                ; linked list (parentheses)
( 1 2 :foo 4 )
>>> ,dup
>>> len
4
>>> ,drop
>>> 2 get
:foo

>>> { x: 42, :y 99 1 + => }       ; dict: key/value map (curly brackets)
{ :x 42 =>, :y 100 => }
>>> ,dup
>>> len
2
>>> ,drop
>>> :x get
42
```

<!--

* linked list vs dynamic array ???

-->

### Blocks

(aka Lambdas)

A block consists of:

* optional arguments (which are popped from the stack, right to left);
* code (i.e. a sequence of tokens) that is executed when the block is called.

A block is delimited by square brackets; arguments (if any) are
separated from the code by a `.`.

```koneko
>>> , :myblock [ 42 ] def         ; a block that pushes 42 onto the stack
>>> 'myblock                      ; put it on the stack
[ 42 ]
>>> call                          ; call the block on the stack
42
>>> myblock                       ; call it by name
42
```

```koneko
>>> , :myswap [ x y . 'y 'x ] def ; a block with named arguments
>>> 1 2 myswap
1
>>> ,show-stack
1
2
```

<!--

* arity? args + body
* ..., variable arity, apply, ...
* curry, partial?
* show-stack + reverse order

-->

### Calling vs Applying

NB: since there are usually no guarantees about whether a block has
named arguments (or how many), only blocks/functions known to
explicitly support applying should be applied.  Record constructors
always support application.

Applying a normal block isn't much different from calling it.  Except
that it takes its arguments from the list it is applied to (in reverse
order) instead of from the stack.  The number of elements of the list
must equal the number of arguments of the block.  The block cannot
(indirectly) access the part of the stack before the list it is
applied to.  It can push any number of return values on to the stack
as usual.

```koneko
>>> , :foo [ x y . ( 'x 'y ) show say ] def   ; normal block
>>> :x :y foo                                 ; normal call
( :x :y )
>>> ( :x :y ) 'foo apply                      ; apply
( :x :y )
>>> foo( :x :y )                              ; sugar
( :x :y )
```

A block with an argument named `&` will ignore that argument when
called normally (setting it to `nil`).  It can be applied to a list
with a number of elements equal to or greater than the number of
normal arguments of the block.  The argument `&` is set to a (possibly
empty) list of the remaining elements.

```koneko
>>> , :foo [ x & . ( 'x '& ) show say ] def   ; block with &
>>> :x foo                                    ; normal call
( :x nil )
>>> ( :x :y :z ) 'foo apply                   ; apply
( :x ( :y :z ) )
>>> foo( :x :y :z )                           ; sugar
( :x ( :y :z ) )
```

A block with an argument named `&&` will ignore that argument when
called normally (setting it to `nil`).  It can be applied to a dict,
which needs to provide values for each normal argument (by name).  The
argument `&&` is set to a (possibly empty) dict of the remaining
key/value pairs (the ones not used to provide values for the normal
arguments).

```koneko
>>> , :foo [ x && . ( 'x '&& ) show say ] def ; block with &&
>>> :x foo                                    ; normal call
( :x nil )
>>> { :x 1 =>, :y 2 => } 'foo apply-dict      ; apply-dict
( 1 { :y 2 => } )
>>> foo{ x: 1, y: 2 }                         ; sugar
( 1 { :y 2 => } )
```

Some more examples:

```koneko
>>> , :&+ [ x & . '& 'x '+ foldl ] def
>>> &+( 1 2 3 4 )
10
```

... TODO ...

### Multi(method)s

```koneko
>>> ; a multi w/ 2 arguments, defined for a mix of int and float
>>> , :add ( :int   :int    ) [ __int+__            ] defmulti
>>> , :add ( :float :float  ) [ __float+__          ] defmulti
>>> , :add ( :int   :float  ) [ 'int->float dip add ] defmulti
>>> , :add ( :float :int    ) [  int->float     add ] defmulti
>>> 1 2 add
3
>>> 1.0 add
4.0
```

### Records

```koneko
>>> , :Point ( :x :y ) defrecord  ; define record type
>>> Point{ x: 1, y: -1 }          ; create record instance
Point{ :x 1 =>, :y -1 => }
>>> ,dup
>>> .x
1
>>> ,drop dup
>>> .y
-1
>>> ,drop
>>> { x: 99 } update              ; update record (creates a new one)
Point{ :x 99 =>, :y -1 => }
```

<!-- ADTs ??? -->

### Syntactic Sugar

The parser provides some syntactic sugar.

```koneko
>>> answer: 42                              ; pair w/ single-token value
:answer 42 =>
>>> :answer 42 =>                           ; desugared
:answer 42 =>

>>> { :x 1 =>, :y 2 => }                    ; dict literals are actually sugar
{ :x 1 =>, :y 2 => }
>>> ( :x 1 =>, :y 2 => ) dict               ; desugared
{ :x 1 =>, :y 2 => }

>>> , :Point ( :x :y ) defrecord
>>> Point{ y: 2, x: 1 }                     ; record "constructor" (from dict)
Point{ :x 1 =>, :y 2 => }
>>> ( :y 2 =>, :x 1 => ) dict 'Point apply-dict ; desugared
Point{ :x 1 =>, :y 2 => }

>>> Point( 1 2 )                            ; record "constructor" (from list)
Point{ :x 1 =>, :y 2 => }
>>> ( 1 2 ) 'Point apply                    ; desugared
Point{ :x 1 =>, :y 2 => }

>>> ,dup
>>> .x                                      ; field access
1
>>> :x swap call                            ; desugared
1

>>> 1 ( 2 3 ) !cons                         ; field call (field access + call)
( 1 2 3 )
>>> 1 ( 2 3 ) :cons swap call call          ; desugared
( 1 2 3 )
```

<!--

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

-->

### Primitives

Primitive operations that make up the core language.

```koneko
>>> 41 [ 1 + ]
[ 1 + ]
>>> call                        ; call the block at the top of the stack
42

>>> 1 2 <                       ; comparison: = /= < <= > >=
#t
>>> [ :less ] [ :not-less ] if  ; conditional
:less

>>> #f not                      ; logical
#t
>>> #f 1 or
#t
>>> 1 nil and
#f

>>> ( 42 ) show                 ; convert to readable str
"( 42 )"
>>> "foo" show
"\"foo\""

>>> , "Hello!" say              ; print line
Hello!

>>> () type                     ; get type as keyword
:list
>>> 1 type
:int

>>> 1 callable?
#f
>>> 'swap callable?
#t
>>> () callable?
#t

>>> , :answer 42 def
>>> __name__
:__main__
>>> __module-defs__
( :answer :clear-stack :show-stack )
>>> :answer :__main__ __module-get__
42

>>> 1 int->float
1.0

>>> clear-stack                 ; only available in the repl
>>> show-stack
>>> , 1 2 show-stack
2
1
```

```
>>> "What's your name? " ask    ; print prompt and read line
What's your name? Foo
"Foo"
```

Of course `def` and `=>` are also primitives.

There are also primitive arithmetic operations, but the prelude
defines more convenient versions of these, like `+` and `div`:
`__int+__`, `__int-__`, `__int*__`, `__div__`, `__mod__`,
`__float+__`, `__float-__`, `__float*__`, `__float/__`.

### Builtins

Operations that are easier or more efficient to implement in the
interpreter but could have been defined in the prelude instead.

```koneko
>>> 42 nil?                       ; there is a predicate for each type
#f
>>> nil nil?
#t
>>> 42 int?
#t
>>> :foo kwd?
#t
```

### Prelude

A small set of standard definitions that is available automatically in
all modules.

See `lib/prelude.knk` for the complete prelude with definitions and
examples.

```koneko
>>> , 1 2 show-stack
2
1
>>> ; :swap [ x y . 'y 'x ] def
>>> , swap show-stack             ; swap top 2 values on stack
1
2
>>> ; :dup [ x . 'x 'x ] def
>>> , dup show-stack              ; dup(licate) top of stack
1
1
2
>>> ; :drop [ _ . ] def
>>> , drop show-stack             ; drop (pop & discard) top of stack
1
2
>>> ; :dip [ x f . f 'x ] def
>>> 3 '+ dip *                    ; pop x, call f, push x
9

>>> 1 2 +                         ; arithmetic
3
>>> 1.0 2.0 /
0.5
>>> 8 3 div
2

>>> () empty?                     ; sequences
#t
>>> ( :x :y :z ) len
3
>>> ( :a :b :c ) 1 get
:b

>>> ( 1 2 ) head                  ; lists
1
>>> ( 1 2 3 ) tail
( 2 3 )
>>> () head^
nil
```

```koneko
>>> , ( 1 2 3 ) uncons show-stack ; lists
( 2 3 )
1
>>> cons
( 1 2 3 )
>>> ( 3 4 ) 2 swap cons
( 2 3 4 )

>>> ( 2 3 4 ) [ dup * ] map
( 4 9 16 )
```

```koneko
>>> , [ "Hi!" say ] 3 times       ; miscellaneous
Hi!
Hi!
Hi!
```

... TODO ...

### Standard Library

... TODO ...

<!--

* lib/
* DSLs, Shell, FFI, ...

-->

### Possible Future Extensions

#### Protocols

(aka Interfaces)

... TODO ...

<!--

* match by partial spec like ocaml!
* store/match how?

-->

## More Examples

NB: work in progress.

```koneko
>>> , :twice [ f . f f ] def              ; with named arguments
>>> 42 [ 1 + ] twice
44
```

```koneko
>>> , :twice [ dup 'call dip call ] def   ; points-free
>>> 42 [ 1 + ] twice
44
```

```koneko
>>> , :twice [ f . [ f f ] ] def          ; "curried"
>>> 0 [ 1 + ] twice twice twice twice call
16

>>> 0 [ 1 + ] [ twice twice ] twice call call
16
>>> 0 [ 1 + ] [ twice ] twice twice call call
16

>>> 0 [ 1 + ] [ twice twice ] twice twice call call
256
>>> 0 [ 1 + ] [ twice ] twice twice twice call call
256

>>> 0 [ 1 + ] ???                         ; TODO
65536
```

```koneko
>>> , :mymap [ f . dup empty? [ ] [ uncons 'f dip 'f mymap cons ] if ] def
>>> ( 1 2 3 ) [ dup * ] mymap
( 1 4 9 )
```

<!-- [ ] <==> [ drop () ] if empty? -->

... TODO ...

## Installing

... TODO ...

## Build Requirements

See `koneko.cabal` for the dependencies.

### Debian

```bash
$ apt install haskell-platform libghc-cmdargs-dev libghc-doctest-dev \
  libghc-hashtables-dev libghc-megaparsec-dev libghc-safe-dev \
  libghc-silently-dev
```

## Specs & Docs

```bash
cabal v2-build --write-ghc-environment-files=always --enable-tests
cabal v2-run doctests
cabal v2-run koneko -- --doctest README.md lib/*.knk
```

TODO: haddock

## TODO

* finish design
* finish documentation
* finish implementation
* ???
* profit!

<!--

* look at notes and old python stuff
* haskell features (Text, not String)

* dyn vars?
* iterators?

-->

## License

### Interpreter

[![GPLv3+](https://www.gnu.org/graphics/gplv3-127x51.png)](https://www.gnu.org/licenses/gpl-3.0.html)

### Standard Library

(i.e. `lib/*.knk`)

[![LGPLv3+](https://www.gnu.org/graphics/lgplv3-147x51.png)](https://www.gnu.org/licenses/lgpl-3.0.html)

<!--

### Thesis

(i.e. `thesis/*`)

[![CC-BY-SA](https://licensebuttons.net/l/by-sa/4.0/88x31.png)](https://creativecommons.org/licenses/by-sa/4.0/)

-->

## References

1. https://en.wikipedia.org/wiki/Concatenative_programming_language#Properties

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
