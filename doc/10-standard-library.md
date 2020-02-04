<!-- {{{1 -->

    File        : doc/10-standard-library.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2020-02-03

    Copyright   : Copyright (C) 2020  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ prev: [Primitives, Builtins & Prelude](09-primitives-builtins-and-prelude.md),
→ next: [Possible Future Extensions](11-future.md)

## Standard Library

→ [Math](#math), ...

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

... TODO ...

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
