<!-- {{{1 -->

    File        : README.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2019-11-28

    Copyright   : Copyright (C) 2019  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+, LGPLv3+

<!-- }}}1 -->

<!-- TODO: badges -->

[![Build Status](https://travis-ci.org/obfusk/koneko.svg?branch=master)](https://travis-ci.org/obfusk/koneko)
[![GPLv3+](https://img.shields.io/badge/license-GPLv3+-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![LGPLv3+](https://img.shields.io/badge/license-LGPLv3+-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0.html)

<p align="center">
  <img src="logo.svg" alt="koneko logo" width="150" /><br />
  koneko - a concatenative not-quite-lisp for kittens
</p>

## Description

**NB: work in progress.**

Koneko (子猫 -- "kitten" in Japanese) is a simple concatenative
stack-based programming language with lisp influences.  It is intended
to combine the elegance of the (point-free) "concatenation is
composition" model with the elegance of lisp-like languages (esp.
anonymous functions with named parameters).

**&rarr; Try koneko in your browser with the JavaScript
[REPL](https://koneko.dev).**

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
  - named parameters/points (lexically scoped)
* functional
  - only immutable data structures
  - does have side effects (I/O)
  - (mostly) strict evaluation
* dynamically, strongly typed

## Hello World

```bash
$ koneko -e '"Hello, World!" say!'
hello, World!
```

### REPL

```bash
$ koneko
>>> "Hello, World!" say!
Hello, World!
>>> ^D
```

## The Language

A program is a sequence of tokens.  Each token represents a function
that takes a scope and a stack and returns an (updated) stack.

Juxtaposition (concatenation) of tokens denotes function composition.
Some tokens, like list literals, are nested.

Evaluating any data type literal (e.g. bool, int, list, block) results
in pushing a corresponding value of its type onto the stack.

All data types are immutable.

The syntax and semantics of concatenative languages form the algebraic
structure of a monoid [[1]](#references).

### Type System

Koneko is strongly typed.  For now, it will use dynamic typing and
allow blocks to be of variable arity.  Optional (static) type and/or
arity checking may be added in the future.

### Errors

Invalid programs will result in errors; these include:

* parse errors (invalid syntax);
* name not defined (referencing an undefined ident);
* stack underflow;
* type error (expected e.g. an int on the stack, got something else);
* empty list (when trying to access the head or tail);
* key errors (when trying to access a key not in a dict);
* etc.

There is currently no mechanism for exception handling.  When an error
occurs, an error message is printed and the program is terminated
(except when using the repl, in which case the repl continues after
printing an error message and resetting the stack).

### Comments & Whitespace

```
; comments start with a semicolon and end at the end of the line

1 2 +         ; tokens are separated by whitespace
1 2 +, 2 3 +  ; commas are whitespace
```

NB: the repl will print the top of the stack (unless empty) after it
evaluates a line -- unless the line starts with a `,`.

### Ident(ifier)s

Any contiguous sequence of one or more:

* unicode letters, numbers, or symbols (including `~$^=+|<>`); or
* brackets (any of `(){}[]`); or
* any of these "punctuation" characters: `@%&*-_/?` and `'!:`

is an identifier if it:

* does not start with any of `'!:` or end with `:`;
* is not a single bracket or `()`;
* is not a valid integer literal, floating point literal, or `nil`;
* and does not end with an opening bracket.

Unquoted idents are always evaluated as calls: the ident is looked up
in the current scope, the value found is pushed onto the stack, and
the top of the stack is `call`ed.

NB: `+`, `foo`, and `<42>'` are all idents; there is no distinction
between the names of "functions", "variables", and "operators".

#### Special Ident(ifier)s

* idents starting and ending with `__` (e.g. `__call__`) are
  reserved for primitives and should not be used elsewhere;
* `_` is not reserved, but should only be used for ignored parameters
  and has special meaning as a "default type name" for multis;
* `&` and `&&` are not reserved, but have special meaning as
  parameter names (see `apply` and `apply-dict`).

#### Naming Conventions

Functions with names starting with:

* `~` (e.g. `~nil`) branch (and possibly pattern match) on (the type
  of) a value;
* `^` (e.g. `^seq`) pattern match (and "destructure") a value;
* `&` can be `apply`d to a variable number of arguments;
* a number (e.g. `2dip`) perform an operation on that number of their
  arguments.

Functions with names ending with:

* `!` (e.g. `say!`) are impure (i.e. they perform I/O);
* `?` (e.g. `nil?`) are predicates (i.e. functions that return a bool);
* `^` (e.g. `head^`) are partial functions (i.e. they are only defined
  for a subset of the values of the type(s) of their argument(s); e.g.
  `head^` will fail for an empty list, whereas `head` will return
  `nil`).

Combinators with names ending with:

* `$` (e.g. `bi$`) take multiple values and a single function;
* `~` (e.g. `bi~`) take multiple values and functions and "pair" them;
* `*` (e.g. `bi*`) take multiple values and functions and "multiply"
  them.

NB: combinators that take a single value and multiple functions (e.g.
`bi` and `tri`) do not end with a "special" character.

### Quoting

Quoted idents ("quots") omit the `call`: the ident is looked up and
the value found is pushed onto the stack.

```koneko
>>> 1 2 +                                   ; push and call "+"
3
>>> 'show                                   ; push "show"
#<primitive:__show__>
>>> 1 2 '+ call                             ; push "+", then call it
3
```

### Naming Things

Identifiers refer to either named parameters or definitions in modules.

When an ident is called or quoted, it is looked up in the following
order:

* primitives;
* the current scope and any parent scope(s);
* the module the current scope belongs to;
* any modules imported by the scope's module;
* the prelude;
* builtins.

NB: `def` is the module definition primitive; it takes a keyword
representing the name of the ident to be defined and a value to bind
the ident to.

```koneko
>>> , :answer 42 def            ; define a constant (in the current module)
>>> , :inc [ 1 + ] def          ; define a function
>>> 'answer inc
43
```

NB: koneko is a functional language: named parameters cannot be
"assigned" another value; "redefining" an existing definition in a
module is not allowed according to the language specification (except
in the repl), but this is currently not enforced by the
implementation.  Definitions **should** also only occur at the
beginning of modules, preceding any other code (but this is also not
currently enforced).

The default module is `__main__`; primitives, builtins, and the
prelude are `__prim__`, `__bltn__`, and `__prld__` respectively.

#### Modules

**NB: work in progress.**

```
>>> , :foo [ ... ] defmodule      ; define a module
>>> , :foo import                 ; import a module
>>> , ( :x :y ) :foo import-from  ; copy specific idents from a module
```

TODO: load modules from files.

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
>>> 0b11
3

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
>>> :key-word       ; keyword (aka symbol)
:key-word
>>> :"keyword that is not a valid ident"
:"keyword that is not a valid ident"
```

NB: for more information on keywords/symbols, see [[2]](#references).

NB: ideally keywords are implemented as interned strings but the
language specification does not guarantee that (and the current
implementation does not intern them).

<!--

```
>>> /cat|kitten/    ; regex -- TODO
/cat|kitten/
```

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
>>> answer: 42                    ; syntactic sugar
:answer 42 =>

>>> ( 1 2 :foo 4 )                ; linked list (parentheses)
( 1 2 :foo 4 )
>>> ,dup
>>> len
4
>>> ,drop
>>> 2 get^                        ; indexing
:foo
>>> () empty?
#t

>>> { x: 42, :y 99 1 + => }       ; dict: key/value map (curly brackets)
{ :x 42 =>, :y 100 => }
>>> ,dup
>>> len
2
>>> ,drop
>>> :x get^                       ; indexing
42
```

### Blocks

(aka Lambdas)

A block consists of:

* optional parameters (for which the arguments are popped from the
  stack -- right to left (i.e. from the top of the stack) -- when the
  block is called);
* code (i.e. a sequence of tokens) that is executed when the block is
  called.

A block is delimited by square brackets; parameters (if any) are
separated from the code by a `.`.

```koneko
; push a block -- that pushes 42 onto the stack -- onto the stack
>>> [ 42 ]
[ 42 ]
>>> call                          ; call the block on the stack
42

>>> , :myblock [ 42 ] def         ; let's give it a name
>>> 'myblock                      ; put it on the stack
[ 42 ]
>>> call
42
>>> myblock                       ; call it by name
42
```

```koneko
>>> , :myswap [ x y . 'y 'x ] def ; a block with named parameters
>>> 1 2 myswap
1
>>> ,show-stack
1
2
```

### Calling vs Applying

NB: since there are usually no guarantees about whether a block has
named parameters (or how many), only blocks/functions known to
explicitly support applying should be applied.  Record constructors
always support application.

Applying a normal block isn't much different from calling it.  Except
that it takes its arguments from the list it is applied to (in reverse
order) instead of from the stack.  The number of elements of the list
(i.e. the number of arguments) must equal the number of parameters of
the block.  The block cannot (indirectly) access the part of the stack
before the list it is applied to.  It can push any number of return
values on to the stack as usual.

```koneko
>>> , :foo [ x y . ( 'x 'y ) show say! ] def  ; normal block
>>> :x :y foo                                 ; normal call
( :x :y )
>>> ( :x :y ) 'foo apply                      ; apply
( :x :y )
>>> foo( :x :y )                              ; sugar
( :x :y )
```

A block with a parameter named `&` will ignore that parameter when
called normally (setting it to `nil`).  It can be applied to a list
with a number of elements equal to or greater than the number of
normal parameters of the block.  The parameter `&` is set to a
(possibly empty) list of the remaining elements/arguments.

```koneko
>>> , :foo [ x & . ( 'x '& ) show say! ] def  ; block with &
>>> :x foo                                    ; normal call
( :x nil )
>>> ( :x :y :z ) 'foo apply                   ; apply
( :x ( :y :z ) )
>>> foo( :x :y :z )                           ; sugar
( :x ( :y :z ) )
```

A block with a parameter named `&&` will ignore that parameter when
called normally (setting it to `nil`).  It can be applied to a dict,
which needs to provide values for each normal parameter (by name).
The parameter `&&` is set to a (possibly empty) dict of the remaining
key/value pairs (the ones not used to provide values for the normal
parameters).

```koneko
>>> , :foo [ x && . ( 'x '&& ) show say! ] def  ; block with &&
>>> :x foo                                      ; normal call
( :x nil )
>>> { :x 1 =>, :y 2 => } 'foo apply-dict        ; apply-dict
( 1 { :y 2 => } )
>>> foo{ x: 1, y: 2 }                           ; sugar
( 1 { :y 2 => } )
```

Some more examples:

```koneko
>>> , :&+ [ x & . '& 'x '+ foldl ] def
>>> &+( 1 2 3 4 )
10
```

... TODO ...

### Functions vs Callables

Functions are: blocks, builtins, multis, and record-types; these all
behave like (stack-based) functions and can be `call`ed (and sometimes
`apply`d and/or `apply-dict`ed).

Callables are: all types that can be `call`ed: foremostly functions,
but also other types (e.g. list and record) that are not functions but
which implement certain primitive operations via calls.

### Multi(method)s

```koneko
; a multi w/ 2 parameters, defined for a mix of int and float
>>> , :add ( :int   :int    ) [ __int+__            ] defmulti
>>> , :add ( :float :float  ) [ __float+__          ] defmulti
>>> , :add ( :int   :float  ) [ 'int->float dip add ] defmulti
>>> , :add ( :float :int    ) [  int->float     add ] defmulti
>>> 1 2 add
3
>>> 1.0 add
4.0
```

To provide a "fallback"/"default" for a multi, use a signature
consisting only of `:_` keywords (the number matching the arity).

```koneko
>>> , :add ( :_ :_ ) [ nil ] defmulti
>>> "foo" () add
nil
```

NB: `defmulti` always creates or extends a multi in the current
module; to extend a multi from e.g. the prelude, alias it first.

NB: multis are "global state": extending e.g. a prelude multi will
affect all code using that multi.

### Records

```koneko
>>> , :Point ( :x :y ) defrecord  ; define record type
>>> Point( 1 -1 )                 ; "list" constructor
Point{ :x 1 =>, :y -1 => }
>>> Point{ y: -1, x: 1 }          ; "dict" constructor
Point{ :x 1 =>, :y -1 => }
>>> ,dup
>>> Point?                        ; automatically defined type predicate
#t
>>> ,drop dup
>>> .x                            ; field access
1
>>> ,drop dup
>>> .y
-1
>>> ,drop
>>> { x: 99 } update              ; update record (creates a new one)
Point{ :x 99 =>, :y -1 => }

; automatically defined "pattern match" function:
; calls the block with the values of the records' fields
>>> , [ x y . "x = " 'x show ++ ", y = " 'y show ++ ++ say! ] ^Point
x = 99, y = -1

; automatically defined "branch" function:
; takes a second block to call if the value is of another type
>>> , Point( 1 2 )
>>> [ + ] [ "not a point" ] ~Point
3

; multis can have record names in their signatures
>>> , :+ '+ def                   ; alias to extend
>>> , :+ ( :Point :Point ) [ '.x '.y .[ '1 bi$ + ] bi$ 2bi Point ] defmulti
>>> Point( 1 2 ) Point( 3 4 ) +
Point{ :x 4 =>, :y 6 => }

>>> Point( 9 34 ) record->dict    ; conversion & type information
{ :x 9 =>, :y 34 => }
>>> Point( 3 4 ) record-type
#<record-type:Point(x#y)>
>>> dup record-type-name
:Point
>>> drop record-type-fields
( :x :y )
>>> Point( 3 4 ) record-values
( 3 4 )
>>> '=> zip ->list dict 'Point apply-dict
Point{ :x 3 =>, :y 4 => }
```

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
>>> Point{ y: 2, x: 1 }                     ; record "dict constructor"
Point{ :x 1 =>, :y 2 => }
>>> ( :y 2 =>, :x 1 => ) dict 'Point apply-dict ; desugared
Point{ :x 1 =>, :y 2 => }

>>> Point( 1 2 )                            ; record "list constructor"
Point{ :x 1 =>, :y 2 => }
>>> ( 1 2 ) 'Point apply                    ; desugared
Point{ :x 1 =>, :y 2 => }

>>> ,dup
>>> .x                                      ; field access
1
>>> ,drop
>>> :x swap call                            ; desugared
1

>>> 1 ( 2 3 ) !cons                         ; field call (field access + call)
( 1 2 3 )
>>> 1 ( 2 3 ) :cons swap call call          ; desugared
( 1 2 3 )

>>> '.x                                     ; quoted field access
[ :x __swap__ __call__ ]
>>> '!x                                     ; quoted field call
[ :x __swap__ __call__ __call__ ]

>>> , Point( 1 2 ) Point( 3 4 )
>>> '.x bi$ +                               ; useful for combinators
4
```

```koneko
>>> .[ 2 * '1 div ]                         ; "curried" block w/ "holes"
[ __1__ . [ 2 * '__1__ div ] ]
>>> 3 swap call                             ; "fill" the hole from the stack
[ 2 * '__1__ div ]
>>> 2 ![ 3 * '1 div ]                       ; "!" version calls immediately
[ 3 * '__1__ div ]
>>> 5 swap call
7
>>> 5 [ 3 * 2 div ] call                    ; equivalent
7

>>> .[ '2 .1 ]                              ; '2 is sugar for '__2__ (etc.)
[ __1__ __2__ . [ '__2__ __1__ ] ]
>>> '+ 1 ![ '2 .1 ]                         ; .1 is sugar for __1__ (etc.)
[ '__2__ __1__ ]
>>> 42 swap call
43

>>> 1 2 '+ 'show ![ .1 .2 ] call            ; function composition
"3"

>>> ( 1 2 3 ) '* ![ 3 .1 2 div ] map ->list
( 1 3 4 )
>>> ( 1 2 3 ) [ 3 * 2 div ] map ->list      ; equivalent
( 1 3 4 )
```

```koneko
>>> ...                                     ; sugar for __ellipsis__
*** ERROR: name __ellipsis__ is not defined
```

### Primitives

Primitive operations that make up the core language.

NB: all primitives start and end with `__` (e.g. `__call__`); the
prelude defines aliases without them (for the primitives intended to
be used directly); unlike the primitives themselves, these aliases
(e.g. `call`) can be shadowed by named parameters and module
definitions.

```koneko
>>> 41 [ 1 + ]
[ 1 + ]
>>> call                        ; call the block at the top of the stack
42

>>> 1 2 <                       ; comparison: = not= < <= > >=
#t
>>> [ :less ] [ :not-less ] if  ; conditional
:less

>>> ( 42 ) show                 ; convert to readable str
"( 42 )"
>>> "foo" show
"\"foo\""

>>> , "Hello!" say!             ; print line
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
>>> () function?
#f

>>> , :answer 42 def
>>> __name__
:__main__
>>> __module-defs__
( :answer :clear-stack :show-stack )
>>> :answer :__main__ __module-get__
42
>>> __modules__
( :__bltn__ :__main__ :__prim__ :__prld__ )

>>> 1 int->float
1.0

>>> clear-stack                 ; only available in the repl
>>> ,show-stack
>>> , 1 2 show-stack
2
1
>>> "oops!" fail
*** ERROR: oops!
```

```
>>> "What's your name? " ask!   ; print prompt and read line
What's your name? Foo
"Foo"
```

Of course `def` and `=>` are also primitives.

There are also primitive arithmetic operations, but the prelude
defines more convenient versions of these, like `+` and `div`:
`__int+__`, `__int-__`, `__int*__`, `__div__`, `__mod__`,
`__float+__`, `__float-__`, `__float*__`, `__float/__`.

To list all primitives, run:

```
>>> :__prim__ __module-defs__   ; (elided)
( :__=__ :__apply__ :__call__ :__def__ :__if__ ... )
```

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

See [`lib/prelude.knk`](lib/prelude.knk) for the complete prelude with
definitions and examples.

#### Stack Shuffling

```koneko
>>> , 1 2 show-stack
2
1
>>> , swap show-stack             ; swap top 2 values on stack
1
2
>>> , dup show-stack              ; dup(licate) top of stack
1
1
2
>>> , drop show-stack             ; drop (pop & discard) top of stack
1
2
```

See also: `rot>`, `<rot`, `2dup`, `2drop`, `nip`, `over`, `2over`,
`over2`.

#### Combinators

`bi` calls 2 functions on 1 value, `bi$` 1 function on 2 values, etc.

```koneko
>>> , 35 [ 2 + ] [ 7 + ] bi
>>> , show-stack
42
37

>>> 3 '+ dip *                    ; pop x, call f, push x
237
```

See also: `2dip`, `3dip`, `keep`, `2keep`, `tri`, `bi$`, `tri$`,
`bi~`, `tri~`, `bi*`, `2bi`, `2tri`, `2bi$`, `2bi~`.

#### Logic & Arithmetic

```koneko
>>> #f not
#t
>>> #f 1 or
1
>>> 1 nil and
nil

>>> #t 42 37 ?
42
```

See also: `when`, `min`, `max`.

```koneko
>>> 1 2 +
3
>>> 1.0 2.0 /
0.5
>>> 8 3 div
2
>>> 5.0 neg
-5.0
```

#### Strings, Characters, Nil, Numbers & Pairs

```koneko
>>> 0x732b chr
"猫"
>>> ord
29483
```

```koneko
>>> nil [ "<nil>" ] [ type show ] ~nil
"<nil>"
```

See also: `num?`, `~neg`, `~zero`, `~pos`, `^pair`.

#### Lists, Dicts, Ranges & Sequences

```koneko
>>> () empty?
#t
>>> ( :x :y :z ) len
3
>>> ( :a :b :c ) 1 get^
:b

>>> ( 1 2 ) head^
1
>>> ( 1 2 3 ) tail^
( 2 3 )
>>> () head
nil
```

```koneko
>>> , ( 1 2 3 ) uncons^ show-stack
( 2 3 )
1
>>> cons
( 1 2 3 )
>>> ( 3 4 ) 2 swap cons
( 2 3 4 )
```

```koneko
>>> ( 1 2 ) ( 3 4 ) ++
( 1 2 3 4 )

>>> ( :one :two :three ) 1 has?
#t
>>> ( :one :two :three ) :two elem?
#t

>>> { x: 1, y: 2 } { x: 99 } update
{ :x 99 =>, :y 2 => }

>>> 10 15 [m-n] ->list            ; ranges
( 10 11 12 13 14 15 )

>>> "0123456789" 3 -3 [i-j)       ; slicing
"3456"
```

```koneko
>>> () seq                        ; non-empty sequence or nil
nil
>>> ( 1 2 ) seq
( 1 2 )
>>> ( 1 2 ) first
1
>>> ( 1 2 ) rest
( 2 )

; branch/match on empty or first & rest
>>> [ "empty" ] [ x xt . "not empty" ] ^seq
"not empty"

>>> ( 2 3 4 ) [ dup * ] map ->list
( 4 9 16 )
>>> ( 2 3 4 ) [ 2 mod 0 = ] filter ->list
( 2 4 )
>>> ( 2 3 4 ) 10 '- foldl
1

; map etc. consume and produce "lazy" (and possibly infinite) sequences
>>> ( 1 2 3 ) cycle [ dup * ] map 7 take-first ->list
( 1 4 9 1 4 9 1 )
```

See also: `[m-n)`, `[0-)`, `[1-n]`, etc.; `1list`, `lazy-seq`,
`unseq`, `^list`, `zip`, `foldr`, `concat`, `reverse`, `each`,
`cycle`, `iterate`, `repeat`, `replicate`, `take-first`, `drop-first`,
`take-while`, `drop-while`, `sort`.

#### Miscellaneous

```koneko
>>> , [ "Hi!" say! ] 3 times
Hi!
Hi!
Hi!
```

### Standard Library

... TODO ...

### Possible Future Extensions

* Pattern matching (`( 1 Foo( #_ 2 ) ) [ x . ... ] ^~`).
* Optional (static) type and/or arity checking.
* Protocols/interfaces.
* Exception handling.
* ...

## More Examples

**NB: work in progress.**

```koneko
>>> , :fibs ( 0 1 ) [ 'fibs dup rest '+ zip ] lseq def
>>> 'fibs 10 take-first ->list
( 0 1 1 2 3 5 8 13 21 34 )
```

```koneko
>>> "" 0.0 0.0 / show ![ '1 ++ ] 10 times " batman!" ++ say!
NaNNaNNaNNaNNaNNaNNaNNaNNaNNaN batman!
```

```koneko
>>> , :twice [ f . f f ] def              ; with named parameters
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
```

```
>>> 0 [ 1 + ] ???                         ; TODO
65536
```

```koneko
>>> , :mymap [ f . dup empty? [ ] [ uncons^ 'f dip 'f mymap cons ] if ] def
>>> ( 1 2 3 ) [ dup * ] mymap
( 1 4 9 )
```

```koneko
>>> , :maybe [ swap [ over nil? not swap when ] foldl ] def
>>> , :Customer ( :orders ) defrecord
>>> , :Order ( :price ) defrecord
>>> Customer( ( Order( 42 ) ) )
Customer{ :orders ( Order{ :price 42 => } ) => }
>>> ,dup
>>> ( '.orders [ 0 get ] '.price ) maybe
42
>>> ,drop
>>> ( '.orders [ 1 get ] '.price ) maybe
nil
```

```koneko
>>> , :myif [ 'and dip or call ] def
>>> 42 [ :A ] [ :B ] myif
:A
>>> #f [ :A ] [ :B ] myif
:B
```

```koneko
>>> , :mycall [ f . f ] def           ; TODO: primitive redundancy?
>>> [ 1 2 + ] call
3
>>> [ 3 + ] mycall
6
```

... TODO ...

## Installing

... TODO ...

## Running

```bash
rlwrap cabal v2-run koneko --     # Haskell REPL
node js/koneko                    # Node.js REPL
cd js && python3 -m http.server   # Browser REPL
```

... TODO ...

## (Build) Requirements

See `koneko.cabal` for the dependencies.

### Debian

```bash
$ apt install haskell-platform libghc-cmdargs-dev libghc-doctest-dev \
  libghc-hashtables-dev libghc-megaparsec-dev libghc-safe-dev \
  libghc-silently-dev                         # Haskell version
$ apt install nodejs                          # Node.js version
```

## Specs & Docs

```bash
# Haskell
cabal v2-build --write-ghc-environment-files=always --enable-tests
cabal v2-run doctests
cabal v2-run koneko -- --doctest lib/*.knk README.md

# JavaScript
node js/koneko --doctest lib/*.knk README.md
```

TODO: haddock

## TODO

* finish design
* finish documentation
* finish implementation
* ???
* profit!

## License

### Interpreter(s)

[![GPLv3+](https://www.gnu.org/graphics/gplv3-127x51.png)](https://www.gnu.org/licenses/gpl-3.0.html)

### Standard Library

(i.e. `lib/*.knk`)

[![LGPLv3+](https://www.gnu.org/graphics/lgplv3-147x51.png)](https://www.gnu.org/licenses/lgpl-3.0.html)

## References

1. https://en.wikipedia.org/wiki/Concatenative_programming_language#Properties
2. https://en.wikipedia.org/wiki/Symbol_(programming)

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
