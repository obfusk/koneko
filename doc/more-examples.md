<!-- {{{1 -->

    File        : doc/more-examples.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2020-02-07

    Copyright   : Copyright (C) 2020  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md)

## More Examples

**NB: outdated, work in progress.**

### FP

```koneko
>>> , :fix '[ '1 fix .1 ] def             ; fixed point (Y combinator)
>>> [ drop 9 ] fix call
9
>>> [ 3 swap lseq1 ] fix call 10 take-first ->list
( 3 3 3 3 3 3 3 3 3 3 )

>>> , :fac [ .[ dup 1 - .1 * ] [ drop 1 ] ~pos ] fix def
>>> 5 fac
120

>>> , :fib1 [ .[ 1 2 [ - .1 ] '$ $ bi$ bi + ] [] 2 ~[>=] ] fix def
>>> , :fib2 [ .[ 1 - dup 1 - '1 bi$ + ] [] 2 ~[>=] ] fix def
>>> 6 fib1                                ; inefficient!
8
>>> 6 fib2                                ; inefficient!
8
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
>>> , :mymap [ f . dup empty? [] [ uncons^ 'f dip 'f mymap cons ] if ] def
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

### BYO Primitives

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

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
