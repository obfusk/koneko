# --                                                            ; {{{1
#
# File        : koneko/read.py
# Maintainer  : Felix C. Stegerman <flx@obfusk.net>
# Date        : 2017-11-20
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

import sys

import re as _re, regex
sys.modules["re"] = regex     # s/regex/re/ for pyparsing *sigh*
import pyparsing as P
sys.modules["re"] = _re

from . import data as D
from . import misc as M

# TODO: .foo, !foo, foo{, ...; no RawDict, List, etc.
def _make_parser():                                             # {{{1
  # NB: the order in which matched are tried is important;
  # e.g. float before int, etc.

  c, g, r, s  = P.Combine, P.Group, P.Regex, P.Suppress
  om, op, zm  = P.OneOrMore, P.Optional, P.ZeroOrMore
  ibody       = r(M.RX_IDENT_BODY).leaveWhitespace()
  lit         = lambda x: r(regex.escape(x))          # escape + rx
  l           = lambda x: s(lit(x)).leaveWhitespace() # literal
  k           = lambda x: lit(x) + ~ibody             # keyword
  n           = lambda x, name: x.setName(name)       # name it

  ws, term    = s(r(M.RX_SPACE)).leaveWhitespace(), P.Forward()
  comm        = n(r(M.RX_COMMENT), "comment")
  reserved    = P.Word(M.S_BRACKETS, exact = 1) + ~ibody
  ident       = ~reserved + r(M.RX_IDENT_C) + ~ibody
  fident      = n(c(zm(c(ident + lit("."))) + ident),
                  "identifier").setParseAction(P.tokenMap(D.Ident))

  hexint      = r(M.RX_HEXINT).setParseAction(P.tokenMap(int, 16))
  binint      = r(M.RX_BININT).setParseAction(P.tokenMap(int, 2))

  nil         = n(k("nil"), "nil").setParseAction(lambda t: [None])
  bool_       = n(k("#t") | k("#f"), "bool") \
                .setParseAction(lambda t: t[0] == "#t")
  float_      = n(P.pyparsing_common.sci_real, "float")   # b4 int!
  int_        = n(hexint | binint |
                  P.pyparsing_common.signed_integer, "int")
  str_        = n(c(l('"') + r(M.RX_STRING) + l('"')), "string") \
                .setParseAction(P.tokenMap(_parse_str))
  kwd         = n(c(l(":") + (ident | str_)), "keyword") \
                .setParseAction(P.tokenMap(D.Kwd))
  regex_      = n(c(l('/"') + r(M.RX_RAWSTRING) + l('"')), "regex") \
                .setParseAction(P.tokenMap(D.Regex))

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
                regex_ | list_ | dict_ | block | quote          # TODO
  term       << ( value | fident | shift )
  return op(zm(term + ws) + term).ignore(comm)
                                                                # }}}1

# TODO: why is this called twice? _parse_str (etc?) too...
def _parse_block(t):
  params  = tuple(map(D.Ident, t[0].get("params", ())))
  code    = tuple(             t[0].get("code"  , ()) )
  return D.RawBlock(params, code)

def _parse_str(s):                                              # {{{1
  r"""
  Parses valid string contents; handles \uXXXX and \xXX etc.

  >>> s = '"\\u732bs like string"'; s[3:7]
  '732b'
  >>> _parse_str(s[1:-1])
  '猫s like string'
  >>> _parse_str(r"\"\\...")
  '"\\...'
  """

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

_parser = _make_parser()

# TODO: more tests, also: failures; repr() -> show()
def parse(s):                                                   # {{{1
                                                                # {{{2
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
  >>> parse("[ 42 ]")[0]
  '[ 42 ]
  >>> parse("[ x . ]")[0]
  '[ x . ]
  >>> parse("[ x . 42 ]")[0]
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
  """                                                           # }}}2

  return list(_parser.parseString(s.strip(M.S_SPACE), True))
                                                                # }}}1

# TODO
# * `foo, .foo, !foo, **call**, ...
def read(s):
  """Parse code into an expression."""
  ...

# ... TODO ...

if __name__ == "__main__":
  import doctest
  if doctest.testmod()[0]: sys.exit(1)

# vim: set tw=70 sw=2 sts=2 et fdm=marker :
