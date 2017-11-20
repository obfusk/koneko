# --                                                            ; {{{1
#
# File        : koneko/read.py
# Maintainer  : Felix C. Stegerman <flx@obfusk.net>
# Date        : 2017-11-19
#
# Copyright   : Copyright (C) 2017  Felix C. Stegerman
# Version     : v0.0.1
# License     : GPLv3+
#
# --                                                            ; }}}1

                                                                # {{{1
r"""
... TODO ...

>>> read("")
TODO

... TODO ...
"""                                                             # }}}1

import pyparsing as P
import sys

from . import data as D

unicodeAlNum    = "".join( c for c in map(chr, range(sys.maxunicode))
                             if c.isalnum() )
stringRx        = r'(?:[^"\\]|\\(?:["\\]|x[0-9a-fA-F]{2}|' \
                                       r'u[0-9a-fA-F]{4}))*'
rawStringRx     = r'(?:[^"\\]|\\.)*'
space, spaceRx  = " \n\t\t,", r"[ \n\t\r,]+"

# TODO: use ""
class Kwd(str):                                                 # {{{1
  """Keyword."""
  def __repr__(self):
    return ":" + (super().__str__() if isident(self) else
                  super().__repr__())
                                                                # }}}1

D.Kwd = Kwd

# TODO: record, .foo, !foo
def _parser():                                                  # {{{1
  pre, bra    = "'!:", "(){}[]"
  sym         = unicodeAlNum + "~!@$%^&*-_=+|<>/?" + bra
  sym_        = sym + pre

  c, g, r, s  = P.Combine, P.Group, P.Regex, P.Suppress
  om, op, zm  = P.OneOrMore, P.Optional, P.ZeroOrMore

  l           = lambda x: s(P.Literal(x)).leaveWhitespace()
  k           = lambda x: P.Keyword(x, sym_)
  n           = lambda x, name: x.setName(name)

  comm        = n(r(";.*"), "comment")
  reserved    = P.Word(bra, exact = 1) + ~P.Word(sym_)
  ident       = ~reserved + P.Word(sym, sym_)
  fident      = n(c(zm(c(ident + P.Literal("."))) + ident),
                  "identifier").setParseAction(P.tokenMap(D.Ident))

  hexint      = r(r"[+-]?0x[0-9a-fA-F]+") \
                .setParseAction(P.tokenMap(int, 16))
  binint      = r(r"[+-]?0b[01]+") \
                .setParseAction(P.tokenMap(int, 2))

  nil         = n(k("nil"), "nil").setParseAction(lambda t: [None])
  bool_       = n(k("#t") | k("#f"), "bool") \
                .setParseAction(lambda t: t[0] == "#t")
  float_      = n(P.pyparsing_common.sci_real, "float")   # b4 int!
  int_        = n(hexint | binint |
                  P.pyparsing_common.signed_integer, "int")
  str_        = n(c(l('"') + r(stringRx) + l('"')), "string") \
                .setParseAction(P.tokenMap(_parse_str))
  kwd         = n(c(l(":") + (ident | str_)), "keyword") \
                .setParseAction(P.tokenMap(Kwd))
  regex       = n(c(l('/"') + r(rawStringRx) + l('"')), "regex") \
                .setParseAction(P.tokenMap(D.Regex))

  ws          = P.Suppress(r(spaceRx)).leaveWhitespace()
  term        = P.Forward()

  list_       = n(g(s(k("(")) + ws + op(om(term + ws)) + s(k(")"))),
                  "list").setParseAction(lambda t: [D.List(t[0])])
  dict_       = n(g(s(k("{")) + ws + op(om((term + ws)*2)) +
                    s(k("}"))), "dict") \
                .setParseAction(lambda t: [D.RawDict(tuple(t[0]))])

  record      = ...

  block       = n(g(s(k("[")) + ws +
                    op(g(om(ident + ws))("params") + s(k(".")) + ws) +
                    op(g(om(term + ws))("code")) +
                    s(k("]"))), "block").setParseAction(_parse_block)

  quote       = n(c(l("'") + fident), "quote") \
                .setParseAction(lambda t: D.Quote(D.Ident(t[0])))
  shift       = n(r("`+").leaveWhitespace() + fident, "shift") \
                .setParseAction(lambda t: D.Shift(t[1], len(t[0])))

  shorthands  = ... # .foo !foo

  value       = nil | bool_ | float_ | int_ | str_ | kwd | \
                regex | list_ | dict_ | block | quote           # TODO
  term       << ( value | fident | shift )
  program     = op(zm(term + ws) + term).ignore(comm)

  return program, (set(sym), set(sym_))
                                                                # }}}1

# TODO: why is this called twice? _parse_str (etc?) too...
def _parse_block(t):
  params  = tuple(map(D.Ident, t[0].get("params", ())))
  code    = tuple(             t[0].get("code"  , ()) )
  return D.RawBlock(params, code)

def _parse_str(s):                                              # {{{1
  t, i, n = [], 0, len(s)
  while i < n:
    if s[i] == '\\':
      if s[i+1] in '\\"':
        t.append(s[i+1]); i += 2
      elif s[i+1] == 'x':
        t.append(chr(int(s[i+2:i+4], 16))); i += 4
      elif s[i+1] == 'u':
        t.append(chr(int(s[i+2:i+6], 16))); i += 6
      else: raise RuntimeError("WTF")
    else:
      t.append(s[i]); i += 1
  return "".join(t)
                                                                # }}}1

_parser, _sym = _parser()

def isident(s):                                                 # {{{1
  """
  Is the string an identifier?
  NB: does not check whether it is a *valid* identifier (i.e. whether
  it is not e.g. [ or ) or nil.

  >>> isident("nil")
  True
  >>> isident("")
  False
  >>> isident("42")
  False
  >>> isident("foo-bar'")
  True
  >>> isident("[子猫]")
  True
  >>> isident("'foo")
  False
  >>> isident("!@$%^&*")
  True
  """

  return len(s) > 0 and not s.isnumeric() and s[0] in _sym[0] and \
         all( c in _sym[1] for c in s[1:] )
                                                                # }}}1

# TODO: tests, also: failures
def parse(s):                                                   # {{{1
  r"""
  Parse code into an intermediate result.

  >>> parse("nil")[0] is None
  True
  >>> parse("#t")[0]
  True
  >>> parse("#f")[0]
  False
  >>> parse("-4.2")[0]
  -4.2
  >>> parse("0x20")[0]
  32
  >>> s = '"\\u732bs like string"'; s[3:7]
  '732b'
  >>> parse(s)[0]
  '猫s like string'
  >>> parse('"\x20"')[0]
  ' '
  >>> parse(':"foo-\\u732b-bar"')[0]
  :foo-猫-bar
  >>> parse('/"foo\\bar"')[0]
  /"foo\\bar"

  >>> parse("( 1 2 :foo )")[0]
  ( 1 2 :foo )
  >>> parse("{ x 42 'y [ 37 ] }")[0]
  '{ x 42, 'y '[ 37 ] }

  >>> parse("[,]")[0]
  '[ ]
  >>> parse("[,42,]")[0]
  '[ 42 ]
  >>> parse("[,x,.,]")[0]
  '[ x . ]
  >>> parse("[,x,.,42,]")[0]
  '[ x . 42 ]

  >>> type(parse("'foo")[0]).__name__
  'Quote'
  >>> type(parse("foo")[0]).__name__
  'Ident'
  >>> type(parse("`foo")[0]).__name__
  'Shift'

  >>> parse("'foo")[0]
  'foo
  >>> parse("foo")[0]
  foo
  >>> parse("`foo")[0]
  `foo

  >>> print(" ".join(map(repr, parse(" ,#t 42 'q,"))))
  True 42 'q

  ... TODO ...
  """

  return list(_parser.parseString(s.strip(space), True))
                                                                # }}}1

# TODO
# * `foo, .foo, !foo, **call**
def read(s):
  """Parse code into an expression."""
  ...

# ... TODO ...

if __name__ == "__main__":
  import doctest
  if doctest.testmod()[0]: sys.exit(1)

# vim: set tw=70 sw=2 sts=2 et fdm=marker :
