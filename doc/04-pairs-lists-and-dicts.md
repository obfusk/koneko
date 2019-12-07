<!-- {{{1 -->

    File        : doc/04-pairs-lists-and-dicts.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2019-12-06

    Copyright   : Copyright (C) 2019  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ prev: [Primitive Data Types](03-primitive-data-types.md),
→ next: [Functions](05-functions.md)

## Pairs, Lists & Dicts

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

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
