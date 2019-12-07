<!-- {{{1 -->

    File        : doc/01-language-features.md
    Maintainer  : Felix C. Stegerman <flx@obfusk.net>
    Date        : 2019-12-06

    Copyright   : Copyright (C) 2019  Felix C. Stegerman
    Version     : v0.0.1
    License     : GPLv3+

<!-- }}}1 -->

→ [README](../README.md),
→ next: [Ident(ifiers) & Quoting](02-identifiers-and-quoting.md)

## Language Features

→ [Type System](#type-system),
→ [Errors](#errors),
→ [Comments & Whitespace](#comments--whitespace)

A program is a sequence of tokens.  Each token represents a function
that takes a scope and a stack and returns an (updated) stack.

Juxtaposition (concatenation) of tokens denotes function composition.
Some tokens, like list literals, are nested.

Evaluating any data type literal (e.g. bool, int, list, block) results
in pushing a corresponding value of its type onto the stack.

All data types are immutable.

The syntax and semantics of concatenative languages form the algebraic
structure of a monoid [[1]](#references).

### Type System

Koneko is strongly typed.  For now, it will use dynamic typing and
allow blocks to be of variable arity.  Optional (static) type and/or
arity checking may be added in the future.

### Errors

Invalid programs will result in errors; these include:

* parse errors (invalid syntax);
* name not defined (referencing an undefined ident);
* stack underflow;
* type error (expected e.g. an int on the stack, got something else);
* empty list (when trying to access the head or tail);
* key errors (when trying to access a key not in a dict);
* etc.

There is currently no mechanism for exception handling.  When an error
occurs, an error message is printed and the program is terminated
(except when using the repl, in which case the repl continues after
printing an error message and resetting the stack).

### Comments & Whitespace

```
; comments start with a semicolon and end at the end of the line

1 2 +         ; tokens are separated by whitespace
1 2 +, 2 3 +  ; commas are whitespace
```

NB: the repl will print the top of the stack (unless empty) after it
evaluates a line -- unless the line starts with a `,`.

## References

1. https://en.wikipedia.org/wiki/Concatenative_programming_language#Properties

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
