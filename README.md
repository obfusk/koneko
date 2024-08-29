<!-- SPDX-FileCopyrightText: 2024 FC (Fay) Stegerman <flx@obfusk.net> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

<!-- TODO: badges -->

[![GitHub Release](https://img.shields.io/github/release/obfusk/koneko.svg?logo=github)](https://github.com/obfusk/koneko/releases)
[![Hackage](https://img.shields.io/hackage/v/koneko)](https://hackage.haskell.org/package/koneko)
[![npm](https://img.shields.io/npm/v/koneko)](https://www.npmjs.com/package/koneko)
[![CI](https://github.com/obfusk/koneko/workflows/CI/badge.svg)](https://github.com/obfusk/koneko/actions?query=workflow%3ACI)
[![GPLv3+](https://img.shields.io/badge/license-GPLv3+-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![LGPLv3+](https://img.shields.io/badge/license-LGPLv3+-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0.html)
<!--
[![Sponsor](https://img.shields.io/badge/%E2%99%A5-support-violet.svg)](https://ko-fi.com/obfusk)
-->

<p align="center">
  <img src="logo.svg" alt="koneko logo" width="150" /><br />
  koneko - a concatenative not-quite-lisp for kittens
</p>

→ [Description](#description),
→ [Whirlwind Tour](#whirlwind-tour),
→ [Language Reference](#language-reference),
→ [More Examples](#more-examples),
→ [Doctests](#doctests);
<br/>
→ [Installing](#installing),
→ [Running](#running),
→ [(Build) Requirements](#build-requirements),
→ [Specs & Docs](#specs--docs),
→ [Vim Syntax Highlighting](#vim-syntax-highlighting);
<br/>
→ [TODO](#todo),
→ [License](#license)

## Description

**NB: work in progress.**

Koneko (子猫 -- "kitten" in Japanese) is a simple functional
concatenative stack-based programming language with Lisp (& Haskell)
influences.  It is intended to combine the elegance of the
(point-free) "concatenation is composition" model with the elegance of
Lisp-like languages.

**→ Try koneko in your browser with the JavaScript
[REPL](https://koneko.dev).**

### Properties

* concatenative
  - point-free
  - juxtaposition of expressions denotes function composition
* stack-oriented
  - postfix (reverse polish) notation
  - functions consume arguments from the stack
  - functions produce return values on the stack
* Lisp-like
  - homoiconic
  - blocks (anonymous functions, similar to lambdas)
  - named parameters/points (lexically scoped)
* functional
  - only immutable data structures
  - does have side effects (I/O)
  - (mostly) strict evaluation
* dynamically, strongly typed

### Features

* primitive (built-in):
  - primitive data types: nil, bool, int, float, str, kwd
  - composite data types: pair, list, dict
  - multi(method)s (similar to multiple dispatch & type classes)
  - records (similar to algebraic data types)
  - modules
  - regexes (mostly Perl-compatible and thus not actually "regular" expressions)
  - concurrency (& parallelism)
  - exception handling
  - thunks
* prelude (standard library):
  - stack shuffling
  - combinators (stack-oriented higher-order functions)
  - arithmetic
  - string operations
  - ranges, lazy sequences & sequence operations
  - slicing & associative structure operations
  - looping & basic I/O
  - either, functor, monad
* nil punning
* syntactic sugar (on top of a minimal "core language")

## Whirlwind Tour

### Hello World

```bash
$ koneko -e '"Hello, World!" say!'
hello, World!
```

### REPL

```bash
$ koneko
>>> "Hello, World!" say!
Hello, World!
>>> ^D
```

### The Weird (& Wonderful)

```koneko
>>> 1 2 +                     ; postfix notation
3

>>> drop                      ; functions manipulate the stack
>>> ( 3 4 )                   ; literals push a value onto the stack
( 3 4 )

>>> len dup                   ; unquoted identifiers are calls
2
>>> '+                        ; quoted identifiers push themselves
#<multi:2:+>
>>> call                      ; and can then be called explicitly
4

>>> ( 1 2 3 )
( 1 2 3 )
>>> reverse show              ; concatenation is function composition
"( 3 2 1 )"

>>> ( 4 5, 6 )                ; commas are whitespace
( 4 5 6 )

; unless a line starts with a comma, the command-line repl will print
; the top of the stack after evaluating it
>>> , 7 2
>>> -
5

>>> ,s!                       ; show the stack (non-browser repl only)
--- STACK ---
5
( 4 5 6 )
"( 3 2 1 )"
4
---  END  ---
>>> clear-stack!              ; clear the stack (repl only)
*** STACK CLEARED ***
```

NB: use whitespace to separate tokens since "special" characters like
`+` and `(` are not delimiters but valid parts of identifiers.

```koneko
>>> 1 2+
*** ERROR: name 2+ is not defined
>>> (1 2)
*** ERROR: name (1 is not defined
```

Details:
→ [Language Features](doc/01-language-features.md),
→ [Ident(ifiers) & Quoting](doc/02-identifiers-and-quoting.md)

### Data Types

NB: all data types are immutable.

```koneko
>>> ()                            ; empty list
()
>>> ( nil #t #f )                 ; list containing nil, true & false
( nil #t #f )
>>> ( 1 2 + 4 )                   ; nested expressions are evaluated
( 3 4 )

>>> 32 0x20 0b100000              ; integers
32
>>> 3.14                          ; floating point
3.14

>>> "spam & eggs"                 ; string
"spam & eggs"
>>> :foo                          ; keyword (aka symbol)
:foo

>>> :answer 42 =>                 ; key/value pair
:answer 42 =>
>>> { x: 42, :y 99 1 + => }       ; dict: key/value map
{ :x 42 =>, :y 100 => }
```

NB: `nil` and `#f` are falsy, everything else is truthy.

```koneko
>>> , :Point ( :x :y ) defrecord  ; define record type
>>> Point( 1 -1 )                 ; "list" constructor
Point{ :x 1 =>, :y -1 => }
>>> Point{ y: -1, x: 1 }          ; "dict" constructor
Point{ :x 1 =>, :y -1 => }
>>> .x                            ; field access
1
```

Details:
→ [Primitive Data Types](doc/03-primitive-data-types.md),
→ [Pairs, Lists & Dicts](doc/04-pairs-lists-and-dicts.md),
→ [Records](doc/07-records.md)

### Functions

```koneko
>>> , 2 7                                 ; push 2 and 7
>>> [ swap - ]                            ; push a block
[ swap - ]
>>> call                                  ; call the block
5
>>> , :swap-and-subtract [ swap - ] def   ; named block
>>> 2 7 swap-and-subtract
5
```

NB: since purely concatenative programs contain no free variables,
almost any "subexpression" can be "factored out" simply by giving it a
name.

```koneko
>>> , :myswap [ x y . 'y 'x ] def         ; named parameters
>>> , 1 2 myswap s!
--- STACK ---
1
2
---  END  ---

>>> 1 2 [ x y . y x ] call                ; remember to quote non-calls
*** ERROR: type int is not callable
>>> [1 +]                                 ; remember to use whitespace
*** ERROR: name [1 is not defined
```

Details:
→ [Functions](doc/05-functions.md),
→ [Multi(method)s](doc/06-multimethods.md)

### Primitives & Prelude

Details:
→ [Primitives, Builtins & Prelude](doc/09-primitives-builtins-and-prelude.md),
→ [Prelude: Syntax Highlighted Source](https://koneko.dev/lib-doc/prelude.knk.html),
→ [Prelude: Function Index](https://koneko.dev/lib-doc/prelude.knk.index.html)

```koneko
>>> , :inc [ 1 + ] def          ; naming things
>>> 41 'inc call                ; explicit call
42

>>> 1 2 <                       ; comparison: = not= < <= > >=
#t
>>> [ :less ] [ :not-less ] if  ; conditional
:less

>>> ( 42 ) show                 ; convert to readable str
"( 42 )"
>>> "foo" show
"\"foo\""

>>> , "Hello!" say!             ; print line
Hello!
```

#### Stack Shuffling

```koneko
>>> 1 2 swap drop dup +         ; swap, drop the 1, dup the 2, add
4
```

#### Combinators

```koneko
>>> , 35 [ 2 + ] [ 7 + ] bi     ; call two functions on 1 value
>>> , s!
--- STACK ---
42
37
---  END  ---
```

### Syntactic Sugar

```koneko
>>> answer: 42                      ; pair w/ single-token value
:answer 42 =>
>>> { x: 1, y: 2 }                  ; dict literal
{ :x 1 =>, :y 2 => }

>>> 1 ( 2 3 ) !cons                 ; field call (field access + call)
( 1 2 3 )

>>> '.x                             ; quoted field access
[ :x __swap__ __call__ ]
>>> '!x                             ; quoted field call
[ :x __swap__ __call__ __call__ ]

>>> '[ 2 * '1 div ]                 ; "curried" block w/ "holes"
[ __1__ . [ 2 * '__1__ div ] ]
>>> 3 swap call                     ; "fill" the hole from the stack
[ 2 * '__1__ div ]
>>> 5 swap call
3
```

Details:
→ [Syntactic Sugar](doc/08-syntactic-sugar.md)

### Examples

```koneko
>>> , :fibs ( 0 1 ) [ 'fibs dup rest '+ zip ] lseq def
>>> 'fibs 10 take-first ->list
( 0 1 1 2 3 5 8 13 21 34 )
>>> 'fibs 10 nth
55
```

```koneko
>>> "" 0.0 0.0 / show .[ '1 ++ ] 10 times " batman!" ++ say!
NaNNaNNaNNaNNaNNaNNaNNaNNaNNaN batman!
```

```koneko
>>> 15 [1-n] [ dup 3 "fizz" 5 "buzz" '[ '1 div? '2 "" ? ] 2bi$ bi
...            ++ 'show 'nip ~seq say! ] each
1
2
fizz
4
buzz
fizz
7
8
fizz
buzz
11
fizz
13
14
fizzbuzz
```

## Language Reference

→ [01: Language Features](doc/01-language-features.md),
<br/>
→ [02: Ident(ifiers) & Quoting](doc/02-identifiers-and-quoting.md),
<br/>
→ [03: Primitive Data Types](doc/03-primitive-data-types.md),
<br/>
→ [04: Pairs, Lists & Dicts](doc/04-pairs-lists-and-dicts.md),
<br/>
→ [05: Functions](doc/05-functions.md),
<br/>
→ [06: Multi(method)s](doc/06-multimethods.md),
<br/>
→ [07: Records](doc/07-records.md),
<br/>
→ [08: Syntactic Sugar](doc/08-syntactic-sugar.md),
<br/>
→ [09: Primitives, Builtins & Prelude](doc/09-primitives-builtins-and-prelude.md),
<br/>
→ [10: Standard Library](doc/10-standard-library.md),
<br/>
→ [11: Possible Future Extensions](doc/11-future.md)

### Prelude

→ [Syntax Highlighted Source](https://koneko.dev/lib-doc/prelude.knk.html),
→ [Function Index](https://koneko.dev/lib-doc/prelude.knk.index.html)

## More Examples

→ [More Examples](doc/more-examples.md)

## Doctests

Like Python (& Haskell), koneko supports "doctests": executable pieces
of documentation that look like interactive REPL sessions.  Doctests
make it easy to write user tutorials, documentation, and regression
tests at the same time and confirm that examples in documentation are
correct and up to date.

NB: this README, the Language Reference, and koneko's Prelude &
Standard Library are full of doctests.

Lets look at an example, `mylib.knk`:

```koneko
:mylib defmodule[

; swap top 2 values
;
; >>> :mylib use
; >>> , 1 2 s!
; --- STACK ---
; 2
; 1
; ---  END  ---
; >>> , myswap s!
; --- STACK ---
; 1
; 2
; ---  END  ---

:myswap [ x y . 'y 'x ] def

] ; defmodule
```

We run koneko with the `--doctest` option (in this case also with `-v`
for verbosity) to execute the tests in a koneko -- or markdown --
file:

```bash
$ KONEKOPATH=. koneko --doctest -v mylib.knk
=== Testing mylib.knk (koneko) ===
Trying:
  :mylib use
Expecting:
ok
Trying:
  , 1 2 s!
Expecting:
  --- STACK ---
  2
  1
  ---  END  ---
ok
Trying:
  , myswap s!
Expecting:
  --- STACK ---
  1
  2
  ---  END  ---
ok
Total: 3, Tried: 3, Passed: 3, Failed: 0.
=== Summary ===
Total: 3, Tried: 3, Passed: 3, Failed: 0.
Test passed.

=== Summary ===
Files: 1.
Total: 3, Tried: 3, Passed: 3, Failed: 0.
Test passed.
```

NB: for `:mylib use` to be able to load the `mylib.knk` file we need
to add the current directory to `KONEKOPATH`.

## Installing

See [(Build) Requirements](#build-requirements).

... TODO ...

## Running

### Linux (& Probably macOS and *BSD)

```bash
$ make cabal_build                                      # Haskell Build
$ ./scripts/repl_hs                                     # Haskell REPL

$ ./scripts/repl_js                                     # Node.js REPL
$ make repl_browser                                     # Browser REPL
```

### Windows (Untested)

```
$ cabal v2-build --write-ghc-environment-files=always --enable-tests
$ cabal v2-run koneko                                   # Haskell REPL

$ node js\koneko                                        # Node.js REPL
```

... TODO ...

## (Build) Requirements

The Haskell implementation requires [GHC](https://www.haskell.org/ghc/),
[`cabal-install`](https://cabal.readthedocs.io/) (and a few additional libraries
that can be installed using Cabal); see `koneko.cabal` for the dependencies.

The JavaScript implementation requires Node.js.

### Debian

```bash
$ apt install cabal-install libghc-aeson-dev libghc-async-dev \
  libghc-cmdargs-dev libghc-doctest-dev libghc-hashtables-dev \
  libghc-megaparsec-dev libghc-random-dev libghc-regex-pcre-dev \
  libghc-safe-dev libghc-silently-dev libghc-split-dev \
  libghc-unordered-containers-dev libghc-vector-dev     # Haskell version
$ apt install nodejs                                    # Node.js version
$ apt install rlwrap                                    # (readline support)
```

## Specs & Docs

```bash
$ make cabal_build test_haskell                         # Haskell
$ make test_node                                        # JavaScript
```

TODO: haddock

## Vim Syntax Highlighting

```bash
$ make link_vim_syntax    # symlinks misc/vim/ files from ~/.vim
$ make copy_vim_syntax    # copies   misc/vim/ files to   ~/.vim
```

## TODO

* finish design
* finish documentation
* finish implementation
* ???
* profit!

## License

### Interpreter(s)

[![GPLv3+](https://www.gnu.org/graphics/gplv3-127x51.png)](https://www.gnu.org/licenses/gpl-3.0.html)

### Standard Library

(i.e. `lib/*.knk`)

[![LGPLv3+](https://www.gnu.org/graphics/lgplv3-147x51.png)](https://www.gnu.org/licenses/lgpl-3.0.html)

<!-- vim: set tw=70 sw=2 sts=2 et fdm=marker : -->
