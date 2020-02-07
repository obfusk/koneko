<!-- {{{1 -->

    File        : doc/08-syntactic-sugar.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2020-02-07

    Copyright   : Copyright (C) 2020  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ prev: [Records](07-records.md),
→ next: [Primitives, Builtins & Prelude](09-primitives-builtins-and-prelude.md)

## Syntactic Sugar

The reader (i.e. parser) provides some syntactic sugar.

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

>>> '[ 2 * '1 div ]                         ; "curried" block w/ "holes"
[ __1__ . [ 2 * '__1__ div ] ]
>>> 3 swap call                             ; "fill" the hole from the stack
[ 2 * '__1__ div ]
>>> 2 .[ 3 * '1 div ]                       ; "." version calls immediately
[ 3 * '__1__ div ]
>>> 5 swap call
7
>>> 5 [ 3 * 2 div ] call                    ; equivalent
7

>>> '[ '2 .1 ]                              ; '2 is sugar for '__2__ (etc.)
[ __1__ __2__ . [ '__2__ __1__ ] ]
>>> '+ 1 .[ '2 .1 ]                         ; .1 is sugar for __1__ (etc.)
[ '__2__ __1__ ]
>>> 42 swap call
43

>>> 1 2 '+ 'show .[ .1 .2 ] call            ; function composition
"3"

>>> ( 1 2 3 ) '* .[ 3 .1 2 div ] map ->list
( 1 3 4 )
>>> ( 1 2 3 ) [ 3 * 2 div ] map ->list      ; equivalent
( 1 3 4 )

>>> '__prld__.say!                          ; module access
#<primitive:__say!__>
>>> :say! :__prld__ __module-get__          ; desugared
#<primitive:__say!__>
>>> , "hi!" __prld__.say!                   ; unquoted -> call
hi!

>>> , :foo defmodule[ :x 1 def ]            ; block "constructor"
>>> , :foo [ :y 2 def ] defmodule           ; desugared

>>> ...                                     ; sugar for __ellipsis__
*** ERROR: name __ellipsis__ is not defined
```

## Aliases

The repl provides some aliases.

```koneko
>>> , 1 2
>>> ,s!                         ; s! ⇔ show-stack! ⇔ __show-stack!__
--- STACK ---
2
1
---  END  ---
>>> c!                          ; c! ⇔ clear-stack! ⇔ __clear-stack!__
*** STACK CLEARED ***

>>> 1 2 + d!                    ; d! ⇔ display!
3
>>> 1 2 + D! 3 +                ; D! ⇔ dup&display!
3
6
```

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
