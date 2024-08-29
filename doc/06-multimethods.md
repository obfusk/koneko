<!-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

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

NB: the language specification prohibits "redefining" a multi for the
same signature but this is currently not enforced by the
implementations.

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
