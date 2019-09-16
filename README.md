<!-- {{{1 -->

    File        : README.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2019-09-16

    Copyright   : Copyright (C) 2019  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+, LGPLv3+

<!-- }}}1 -->

<!-- TODO: travis etc. badges -->

[![GPLv3+](https://img.shields.io/badge/license-GPLv3+-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)

## Description

koneko - a concatenative not-quite-lisp for kittens

... TODO ...

## Examples

### Hello World

```bash
$ koneko -e '"Hello, World!" say'
hello, World!
```

```bash
$ koneko
>>> "Hello, World!" say
Hello, World!
^D
```

### ...

... TODO ...

## The Language

### Type System

For now, koneko will use dynamic strong typing; an optional static
type system with inference may be added in the future.

### Comments & Whitespace

```
; comments start with a semicolon and end at the end of the line

( 1 2 )   ; tokens are delimited by whitespace
( 3,4 )   ; commas are whitespace
```

<!-- doctest !!! -->

### Primitive Data Types

Nil, Bool, Int, Float, Str, Kwd, Rx

```
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

>>> "spam & eggs"   ; string
"spam & eggs"
>>> "\u732bs"
"猫s"
>>> "\x20"
" "

>>> :key-word       ; keyword
:key-word
>>> :"keyword that is not a valid identifier"
:"keyword that is not a valid identifier"

>>> /cat|kitten/    ; regex
/cat|kitten/
```

<!--

* Ident
* Raw*
* Range? ...
* Namespace/Scope, Stack, ...

-->

### Pairs, Lists and Dicts

```
>>> :answer 42 =>                 ; key/value pair
Pair( :answer 42 )
>>> dup .key
:answer
>>> .value
42

>>> ( 1 2 :foo 4 )                ; linked list
( 1 2 :foo 4 )
>>> dup len
4
>>> 2 get
:foo

>>> { x: 42, :y 99 1 + => }       ; key/value map
{ x: 42, y: 100 }
>>> dup len
2
>>> :x get
42
>>> { 32 5 +, 76 3 - => }
{ 37 73 => }
```

<!--

* tokens & substack for { ... }, ( ... ) etc.
* linked list vs dynamic array ???

-->

### Records

```
>>> clear-stack

>>> ( :x :y ) :Point defrecord    ; define record type
>>> Point{ x: 1, y: -1 }          ; create record instance
Point{ x: 1, y: -1 }
>>> dup .x
1
>>> dup .y
-1
>>> { x: 99 } update              ; update record (creates a new one)
Point{ x: 99, y: -1 }
```

<!-- ADTs ??? -->

### Blocks

aka Lambdas

```
>>> clear-stack

>>> [ 42 ] :myblock def           ; a block that pushes 42 onto the stack
>>> 'myblock                      ; put it on the stack
[ 42 ]
>>> call                          ; call it
42
>>> myblock
42

>>> clear-stack

>>> [ x y . 'y 'x ] :myswap def   ; a block with parameters
>>> 1 2 myswap
1
>>> show-stack
1
2
```

<!--

* arity? params + body
* ..., variable arity, apply, ...
* curry, partial?

-->

### Protocols

aka Interfaces

... TODO ...

<!--

* match by partial spec like ocaml!
* !foo vs just foo
* store/match how?

-->

### Sugar

```
>>> answer: 42                              ; pair with keyword as key
Pair( :answer 42 )
>>> :answer 42 =>                           ; desugared
Pair( :answer 42 )

>>> Point{ x: 1, y: 2 }                     ; constructor with "keys"
Point{ x: 1, y: 2 }
>>> { :x 1 =>, :y 2 => } 'Point construct   ; completely desugared
Point{ x: 1, y: 2 }

>>> dup .x                                  ; record field access
1
>>> :x get                                  ; desugared
1

>>> 1 2
2
>>> swap                                    ; push & call
1
>>> 'swap call                              ; explicit push, then call
2

>>> swap( 1 2 )                             ; non-postfix apply
1
>>> ( 1 2 ) 'swap apply                     ; desugared
1
>>> 1 2 swap                                ; same as call (for fixed arity)
1

>>> 1 `+ 2                                  ; shift token (use w/ caution)
3
>>> 1 2 +                                   ; desugared
3
```

```
>>> [[ 1 2 3 + + ]]                         ; grouped expression
6
```

... TODO ...

<!--

* list, dict, quote, grouped expr -> parse tree

* construct does what?
* ( ... ) construct-from-list ??? NO b/c is apply
* Point( 1 2 ) -> Point{ x: 1, y: 2 } ???

* overloaded get for dict, list, record
* !foo, !foo( ... )

* reader vs eval vs primitive vs builtin vs stdlib
* [[ ]] is not reader-only sugar

-->

### Quoting

NB: WIP

```
>>> clear-stack

>>> 1 :x def
>>> 2 :y def
>>> 3 :x def

>>> ( 'x 'y 'z )
( 1 2 3 )
>>> '( x y z )
( 1 2 3 )
```

... TODO ...

<!-- macros? -->

### Variables

```
>>> clear-stack                 ; (housekeeping)

>>> 42 :answer def              ; define a variable in the current namespace
>>> [ + 1 ] :inc def            ; can be a value or a block
>>> 'answer inc
43
```

### Primitives

```
>>> 41 [ 1 + ]
[ 1 + ]
>>> call                        ; call the block at the top of the stack
42

>>> 1 2 <
#t
>>> [ :less ] [ :greater ] if   ; conditional
:less
```

... TODO ...

<!-- really *call*, *if*, ... -->

### Builtins

```
>>> clear-stack
>>> 1 2 show-stack
2
1
>>> ; [ x y . 'y 'x ] :swap def
>>> swap show-stack               ; swap top 2 values on stack
1
2
>>> ; [ x . 'x 'x ] :dup def
>>> dup show-stack                ; dup(licate) top of stack
1
1
2
>>> ; [ x . ] :drop def
>>> drop show-stack               ; drop (remove) top of stack
1
2
>>> ; [ x f . f 'x ] :dip def
>>> 3 '+ dip *                    ; remove x, call f, push x
9
```

... TODO ...

<!-- could be in stdlib -->

### Standard Library

```
```

... TODO...

<!-- DSLs, Shell, FFI, ... -->

### ...

... TODO ...

## Help

... TODO ...

## Specs & Docs

... TODO ...

## Requirements

... TODO ...

## Installing

... TODO ...

## TODO

* design
* document
* implement
* ???
* profit!

<!--

* look at notes and old python stuff
* haskell features (Text, not String)
* repl

* dyn vars?
* iterators?

-->

## License

### Interpreter

[GPLv3+](https://www.gnu.org/licenses/gpl-3.0.html).

### Standard Library

(i.e. `lib/*.knk`)

[LGPLv3+](https://www.gnu.org/licenses/lgpl-3.0.html).

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
