<!-- {{{1 -->

    File        : doc/07-records.md
    Maintainer  : FC Stegerman <flx@obfusk.net>
    Date        : 2019-12-12

    Copyright   : Copyright (C) 2019  FC Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ prev: [Multi(method)s](06-multimethods.md),
→ next: [Syntactic Sugar](08-syntactic-sugar.md)

## Records

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
>>> , :+ ( :Point :Point ) [ '.x '.y '[ '1 bi$ + ] bi$ 2bi Point ] defmulti
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

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
