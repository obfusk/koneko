<!-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

→ [README](../README.md),
→ prev: [Language Features](01-language-features.md),
→ next: [Primitive Data Types](03-primitive-data-types.md)

## Ident(ifier)s & Quoting

→ [Ident(ifier)s](#identifiers),
→ [Quoting](#quoting),
→ [Naming Things](#naming-things),
→ [Modules](#modules)

### Ident(ifier)s

Any contiguous sequence of one or more:

* unicode letters, numbers, or symbols (including `~$^=+|<>`); or
* brackets (any of `(){}[]`); or
* any of these "punctuation" characters: `@%&*-_/?` and `'!:`

is an identifier if it:

* does not start with any of `'!:` or end with `:`;
* is not a single bracket or `()`;
* is not a valid integer literal, floating point literal, or `nil`;
* and does not end with an opening bracket.

[Unquoted](#quoting) idents are always evaluated as calls: the ident
is looked up in the current scope, the value found is pushed onto the
stack, and the top of the stack is `call`ed.

NB: `+`, `foo`, and `<42>'` are all idents; there is no distinction
between the names of "functions", "variables", and "operators".

#### Special Ident(ifier)s

* idents starting and ending with `__` (e.g. `__call__`) are
  reserved for primitives and should not be used elsewhere;
* `_` is not reserved, but should only be used for ignored parameters
  and has special meaning as a "default type name" for multis;
* `&` and `&&` are not reserved, but have special meaning as
  parameter names (see `apply` and `apply-dict`).

#### [ADVANCED] Naming Conventions

Functions with names starting with:

* `~` (e.g. `~nil`) branch (and possibly pattern match) on (the type
  of) a value;
* `^` (e.g. `^seq`) pattern match (and "destructure") a value;
* `&` can be `apply`d to a variable number of arguments;
* a number (e.g. `2dip`) perform an operation on that number of their
  arguments.

Functions with names ending with:

* `!` (e.g. `say!`) are impure (i.e. they perform I/O);
* `?` (e.g. `nil?`) are predicates (i.e. functions that return a bool);
* `^` (e.g. `head^`) are partial functions (i.e. they are only defined
  for a subset of the values of the type(s) of their argument(s); e.g.
  `head^` will fail for an empty list, whereas `head` will return
  `nil`).

Combinators with names ending with:

* `$` (e.g. `bi$`) take multiple values and a single function;
* `~` (e.g. `bi~`) take multiple values and functions and "pair" them;
* `*` (e.g. `bi*`) take multiple values and functions and "multiply"
  them.

NB: combinators that take a single value and multiple functions (e.g.
`bi` and `tri`) do not end with a "special" character.

### Quoting

Quoted idents ("quots") omit the `call`: the ident is looked up and
the value found is pushed onto the stack.

```koneko
>>> 1 2 +                                   ; push and call "+"
3
>>> '+                                      ; push "+"
#<multi:2:+>
>>> 1 2 '+ call                             ; push "+", then call it
3
```

### Naming Things

Identifiers refer to either named parameters or definitions in modules.

When an ident is called or quoted, it is looked up in the following
order:

* primitives;
* the current scope and any parent scope(s);
* the module the current scope belongs to;
* any modules imported by the scope's module;
* builtins;
* the prelude.

NB: `def` is the module definition primitive; it takes a
[keyword](03-primitive-data-types.md) representing the name of the
ident to be defined and a value to bind the ident to.

```koneko
>>> , :answer 42 def            ; define a constant (in the current module)
>>> , :inc [ 1 + ] def          ; define a function
>>> 'answer inc
43
```

NB: koneko is a functional language: named parameters cannot be
"assigned" another value; "redefining" an existing definition in a
module is not allowed according to the language specification (except
in the repl), but this is currently not enforced by the
implementations (although the JavaScript interpreter does raise a
warning).  Definitions **should** also only occur at the beginning of
modules, preceding any other code (but this is also not currently
enforced).

The default module is `__main__`; primitives, builtins, and the
prelude are `__prim__`, `__bltn__`, and `__prld__` respectively.

### Modules

**NB: work in progress.**

```
>>> , :foo [ ... ] defmodule      ; define a module
>>> , :foo import                 ; import a module
>>> , ( :x :y ) :foo import-from  ; copy specific idents from a module
```

```
>>> , :foo require                ; loads foo.knk if necessary
>>> , :foo use                    ; require + import
>>> , ( :x :y ) :foo use-from     ; require + import-from
```

NB: `:foo require` will find & load `foo.knk` if the module `foo` is
not already defined; `foo.knk` is assumed to define the module `foo`
(`:foo [ ... ] defmodule`).

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
