<!-- {{{1 -->

    File        : doc/10-standard-library.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2020-11-12

    Copyright   : Copyright (C) 2020  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ prev: [Primitives, Builtins & Prelude](09-primitives-builtins-and-prelude.md),
→ next: [Possible Future Extensions](11-future.md)

## Standard Library

→ [IO](#io), → [JSON](#json), → [Math](#math), → [Str](#str), ...

### IO

**NB: work in progress.**

NB: the `io` module is built-in (and does not need to be `require`d or
`use`d; it can of course still be `import`ed).

```koneko
>>> "lib/_test.knk" io.contents! 24 [-j)    ; file contents
"\"loading module _test..."
>>> , "lib/_test.knk" io.lines!             ; file lines
>>> , "loading" grep 'say! each
"loading module _test..." say!
```

### JSON

NB: the `json` module is built-in (and does not need to be `require`d
or `use`d; it can of course still be `import`ed).

```koneko
>>> { x: 42, y: ( 1 2 3 ) } dup json.<-     ; json str <- value
"{\"x\":42,\"y\":[1,2,3]}"
>>> json.->                                 ; json str -> value
{ :x 42 =>, :y ( 1 2 3 ) => }
>>> =
#t

>>> :foo json.<- json.->                    ; kwds become strs
"foo"
>>> x: 42 json.<- json.->                   ; pairs become lists
( "x" 42 )

>>> , :Foo ( :x :y ) defrecord              ; records become dicts
>>> Foo( 1 2 ) json.<- json.->
{ :__koneko_type__ "Foo" =>, :x 1 =>, :y 2 => }
```

### Math

NB: the `math` module is built-in (and does not need to be `require`d
or `use`d; it can of course still be `import`ed).

```koneko
>>> ( :^ :** :sqrt ) :math use-from

>>> math.pi
3.141592653589793
>>> 1.0 math.exp
2.718281828459045
>>> 10.0 math.log ** round
10

>>> 2 10 ^
1024
>>> 2.0 10.0 **
1024.0
>>> 2.0 0.5 **
1.4142135623730951
>>> 2.0 sqrt
1.4142135623730951

>>> -0.0 math.sign
-0.0
>>> 1.0 0.0 / neg math.sign
-1.0

>>> math.pi 0.5 * math.sin
1.0

>>> :math __module-defs__
( :** :^ :acos :acosh :asin :asinh :atan :atan2 :atanh :cos :cosh :exp :log :pi :sign :sin :sinh :sqrt :tan :tanh )
```

### Str

→ [Syntax Highlighted Source](https://koneko.dev/lib-doc/str.knk.html),
→ [Function Index](https://koneko.dev/lib-doc/str.knk.index.html)

... TODO ...

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
