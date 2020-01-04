<!-- {{{1 -->

    File        : doc/09-primitives-builtins-and-prelude.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2020-01-03

    Copyright   : Copyright (C) 2020  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ prev: [Syntactic Sugar](08-syntactic-sugar.md),
→ next: [Standard Library](10-standard-library.md)

## Primitives, Builtins & Prelude

→ [Primitives](#primitives),
→ [Builtins](#builtins),
→ [Prelude](#prelude)

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

>>> "0x20" str->int
32
>>> "3.14" str->float
3.14
>>> "foo" str->int
nil
```

### Prelude

→ [Stack Shuffling](#stack-shuffling),
→ [Combinators](#combinators),
→ [Logic & Arithmetic](#logic--arithmetic);
<br/>
→ [Strings, Chars, Nil, Numbers & Pairs](#strings-chars-nil-numbers--pairs),
→ [Lists, Dicts, Ranges & Sequences](#lists-dicts-ranges--sequences),
→ [Miscellaneous](#miscellaneous)

A small set of standard definitions that is available automatically in
all modules.

See [`lib/prelude.knk`](../lib/prelude.knk) for the complete prelude
with definitions and examples.

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

See also: `rot>`, `<rot`, `2dup`, `2drop`, `3drop`, `nip`, `over`,
`2over`, `over2`.

#### Combinators

`bi` calls 2 functions on 1 value, `bi$` 1 function on 2 values, etc.

```koneko
>>> , 35 [ 2 + ] [ 7 + ] bi
>>> , show-stack
42
37

>>> 3 '+ dip *                    ; pop x, call f, push x
237

>>> 41 1 '+ $ call                ; partial function application
42
>>> 41 [ 1 + ] call
42

>>> 2 [ 1 + ] [ 3 * ] @ call      ; function composition
9
>>> 2 [ 1 + 3 * ] call
9
```

See also: `2$`, `3$`, `%`, `2dip`, `3dip`, `keep`, `2keep`, `tri`,
`bi$`, `tri$`, `bi~`, `tri~`, `bi*`, `2bi`, `2tri`, `2bi$`, `2bi~`.

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

>>> 19 [ 2 div ] [ 3 * 1 + ] 'even? ~?
58
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
>>> 37 odd?
#t
```

#### Strings, Chars, Nil, Numbers & Pairs

```koneko
>>> 0x732b chr
"猫"
>>> ord
29483
```

```koneko
>>> nil [ "<nil>" ] [ type show ] ~nil
"<nil>"
>>> ( 3 7 ) 'rest ~> 'first ~> [ 1 + ] ~>
8
>>> ( 7 3 ) ( 'rest 'first [ 1 + ] ) ~~>
4
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

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
