<!-- {{{1 -->

    File        : doc/06-multimethods.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2019-12-06

    Copyright   : Copyright (C) 2019  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ prev: [Functions](05-functions.md),
→ next: [Records](07-records.md)

## Multi(method)s

```koneko
; a multi w/ 2 parameters, defined for a mix of int and float
>>> , :add ( :int   :int    ) [ __int+__            ] defmulti
>>> , :add ( :float :float  ) [ __float+__          ] defmulti
>>> , :add ( :int   :float  ) [ 'int->float dip add ] defmulti
>>> , :add ( :float :int    ) [  int->float     add ] defmulti
>>> 1 2 add
3
>>> 1.0 add
4.0
```

To provide a "fallback"/"default" for a multi, use a signature
consisting only of `:_` keywords (the number matching the arity).

```koneko
>>> , :add ( :_ :_ ) [ nil ] defmulti
>>> "foo" () add
nil
```

NB: `defmulti` always creates or extends a multi in the current
module; to extend a multi from e.g. the prelude, alias it first.

NB: multis are "global state": extending e.g. a prelude multi will
affect all code using that multi.

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
