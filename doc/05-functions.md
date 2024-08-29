<!-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

→ [README](../README.md),
→ prev: [Pairs, Lists & Dicts](04-pairs-lists-and-dicts.md),
→ next: [Multi(method)s](06-multimethods.md)

## Functions

→ [Blocks](#blocks),
→ [Calling vs Applying](#calling-vs-applying),
→ [Functions vs Callables](#functions-vs-callables)

### Blocks

(almost, but not quite, entirely unlike lambdas)

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
>>> ,s!
--- STACK ---
1
2
---  END  ---
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

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
