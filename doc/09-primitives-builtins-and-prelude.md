<!-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

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

<!--

def call apply apply-dict if defmulti defrecord => dict show say! ask!
type callable? function? defmodule import import-from = not= < <= > >=
<=> eq neq lt lte gt gte cmp abs trunc round ceil floor int->float
record->dict record-type record-values record-type-name
record-type-fields fail try rx-match rx-sub par sleep

-->

#### Calling, Branching & Comparing

```koneko
>>> 41 [ 1 + ]                  ; (put a block on the stack)
[ 1 + ]
>>> call                        ; call the callable on top of the stack
42

>>> , :T [ :truthy ] def, :F [ :falsy ] def
>>> #t  [ T ] [ F ] if          ; conditional/branch
:truthy
>>> #f  [ T ] [ F ] if          ; false is falsy
:falsy
>>> nil [ T ] [ F ] if          ; so is nil
:falsy
>>> 42  [ T ] [ F ] if          ; everything else is truthy
:truthy
>>> 0 'T 'F if                  ; "branches" need not be blocks
:truthy

>>> 1 2 <                       ; compare: = not= < <= > >=
#t
>>> 1 2 <=>                     ; compare: -1 if <, 0 if =, 1 if >
-1
```

##### CAVEATS

The comparison functions `=`, `<`, `<=>`, etc. define an almost total
ordering on all types.  This makes sorting heterogeneous lists easy.

The exceptions are that blocks, builtins, multis, and thunks are
ordered respective to all other types but cannot be compared to values
of their own type and the "usual" behaviour of NaN being unequal to
even itself.

NB: the ordering also treats all types as completely distinct --
including numeric types like `int` and `float`.

```koneko
>>> ( "foo" ( :bar ) nil ) sort ; uses <=> :-)
( nil "foo" ( :bar ) )
>>> ( 1 2.0 3 4.0 ) sort        ; uses <=> :-(
( 1 3 2.0 4.0 )
```

The "alternative comparison" functions `eq`, `lt`, `cmp`, etc. define
a partial ordering.  They compare mixed `int` and `float` (by
converting `int->float`) and reject all other mixed type comparisons.

```koneko
>>> 42 :foo <=>
-1
>>> 42 :foo cmp
*** ERROR: types int and kwd are not comparable

>>> 2 1.0 <=>                   ; also: = not= < <= > >=
-1
>>> 2 1.0 cmp                   ; also: eq neq lt lte gt gte
1

>>> ( 1 2.0 3 4.0 ) sort'       ; uses cmp :-)
( 1 2.0 3 4.0 )
```

#### Conversion, Type Information & I/O

```koneko
>>> ( 42 ) show                 ; convert to readable str
"( 42 )"
>>> "foo" show
"\"foo\""

>>> -2 abs
2
>>> -3.14 abs
3.14

>>> 3.14 trunc
3
>>> 9.5 round
10
>>> -9.5 round
-10
>>> 10.5 round
10
>>> -10.5 round
-10
>>> 0.0 0.0 / round             ; NaN or Infinite -> nil
nil
>>> -3.14 floor
-4
>>> -3.14 ceil
-3

>>> 1 int->float
1.0

>>> () type                     ; get type as keyword
:list
>>> 1 type
:int
>>> type
:kwd

>>> 1 callable?
#f
>>> 'if callable?
#t
>>> () callable?
#t
>>> () function?
#f

>>> , "Hello!" say!             ; print line
Hello!
```

```
>>> "What's your name? " ask!   ; print prompt and read line
What's your name? Foo
"Foo"
```

#### Module Information

```koneko
>>> , :answer 42 def
>>> __name__
:__main__
>>> __module-defs__
( :D! :__args__ :__repl__ :answer :c! :clear-stack! :d! :s! :show-stack! )
>>> :answer :__main__ __module-get__
42
>>> __modules__ [ show ":__" starts-with? ] filter ->list
( :__bltn__ :__main__ :__prim__ :__prld__ )
```

#### REPL: Clear & Show Stack

```koneko
>>> clear-stack!                ; only available in the repl
*** STACK CLEARED ***
>>> ,show-stack!
--- STACK ---
---  END  ---
>>> , 1 2 s!                    ; s! ⇔ show-stack!
--- STACK ---
2
1
---  END  ---
```

#### Exceptions

```koneko
>>> "oops!" fail                ; raise exception
*** ERROR: oops!

>>> , [ :try 1 2 3 ... ]
...   [ t m i . :catch { type: 't, message: 'm, info: 'i } #t ]
...   [ :finally ] try s!       ; try/catch/finally
--- STACK ---
:finally
{ :info ( "__ellipsis__" ) =>, :message "name __ellipsis__ is not defined" =>, :type :NameError => }
:catch
---  END  ---
>>> c!
*** STACK CLEARED ***
>>> , [ :no :errors :this :time ] [ ... ] [ :cleanup :here ] try s!
--- STACK ---
:here
:cleanup
:time
:this
:errors
:no
---  END  ---
>>> , [ ... ] nil [ :finally ] try  ; w/o "catch"
*** ERROR: name __ellipsis__ is not defined
>>> , [ ... ] [ 3drop "ignoring error!" say! #t ] [] try
ignoring error!
```

NB: the `catch` block must return a value that indicates whether the
exception has been handled; it will be re-raised if it is falsy.  This
can -- and should! -- be used to only catch certain types of errors.

```koneko
>>> , [ 1 0 div ] [ 2drop :DivideByZero = ] [] try
>>> , [ ...     ] [ 2drop :DivideByZero = ] [] try
*** ERROR: name __ellipsis__ is not defined
```

#### Regexes

```koneko
>>> "foo" "^f" rx-match         ; matched part
( "f" )
>>> "foo" "^f(o+)" rx-match     ; and groups
( "foo" "oo" )
>>> "bar" "^f" rx-match         ; or nil
nil

>>> "foo bar" "$2 $1" "(\w+) (\w+)" #f rx-sub       ; str or block
"bar foo"
>>> "1 2 3 4" [ swap " " ++sep++ nip ] "(\w+) (\w+)" #t rx-sub
"2 1 4 3"

>>> "foo 子猫 bar" [ reverse ] "\p{L}+" #t rx-sub   ; replace all
"oof 猫子 rab"
>>> "foo 子猫 bar" [ reverse ] "\p{L}+" #f rx-sub   ; replace first
"oof 子猫 bar"
```

#### Miscellaneous

```koneko
>>> , [ 0.1 sleep :one ] [ 0.05 sleep :two ] par    ; concurrency
>>> ,s!
--- STACK ---
:two
:one
---  END  ---

>>> , [ [ 1 sleep "oops" fail ]
...     [ 1 [ D! inc 0.2 sleep #t ] loop ] par ]
...   [ 3list d! #t ] try-catch
1
2
3
4
5
( :Fail "oops" ( "oops" ) )
```

```
>>> __version__                       ; version & platform information
( ( 0 0 1 ) :hs ( "linux x86_64" "ghc 8.6" ) )

>>> __version__
( ( 0 0 1 ) :js-node ( "linux x64" "node v10.17.0" ) )

>>> __version__ 1st "v${0}.${1}.${2}" fmt
"v0.0.1"
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
→ [Miscellaneous](#miscellaneous-1),
→ [More](#more)

A small set of standard definitions that is available automatically in
all modules.

See [`lib/prelude.knk`](../lib/prelude.knk) for the complete prelude
with definitions and examples:
<br/>
→ [Syntax Highlighted Source](https://koneko.dev/lib-doc/prelude.knk.html),
→ [Function Index](https://koneko.dev/lib-doc/prelude.knk.index.html)

NB: the following is just an overview (and neither complete nor
completely up-to-date); see the links above for all current prelude
functions.

#### Stack Shuffling

```koneko
>>> , 1 2 s!
--- STACK ---
2
1
---  END  ---
>>> , swap s!                     ; swap top 2 values on stack
--- STACK ---
1
2
---  END  ---
>>> , dup s!                      ; dup(licate) top of stack
--- STACK ---
1
1
2
---  END  ---
>>> , drop s!                     ; drop (pop & discard) top of stack
--- STACK ---
1
2
---  END  ---
```

See also: `rot>`, `<rot`, `2dup`, `2drop`, `3drop`, `nip`, `over`,
`2over`, `over2`.

#### Combinators

`bi` calls 2 functions on 1 value, `bi$` 1 function on 2 values, etc.

```koneko
>>> , 35 [ 2 + ] [ 7 + ] bi
>>> , s!
--- STACK ---
42
37
---  END  ---

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
>>> , ( 1 2 3 ) uncons^ s!
--- STACK ---
( 2 3 )
1
---  END  ---
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

#### More

→ [Syntax Highlighted Source](https://koneko.dev/lib-doc/prelude.knk.html),
→ [Function Index](https://koneko.dev/lib-doc/prelude.knk.index.html)

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
