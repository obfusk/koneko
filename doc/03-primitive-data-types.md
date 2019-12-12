<!-- {{{1 -->

    File        : doc/03-primitive-data-types.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2019-12-12

    Copyright   : Copyright (C) 2019  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ prev: [Ident(ifiers) & Quoting](02-identifiers-and-quoting.md),
→ next: [Pairs, Lists & Dicts](04-pairs-lists-and-dicts.md)

## Primitive Data Types

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

NB: for more information on keywords/symbols, see [[1]](#references).

NB: ideally keywords are implemented as interned strings but the
language specification does not guarantee that (and the current
implementation does not intern them).

<!--

```
>>> #/cat|kitten/   ; regex -- TODO
#/cat|kitten/
```

-->

## References

1. https://en.wikipedia.org/wiki/Symbol_(programming)

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
