//  --                                                          ; {{{1
//
//  File        : koneko.js
//  Maintainer  : FC Stegerman <flx@obfusk.net>
//  Date        : 2022-02-14
//
//  Copyright   : Copyright (C) 2022  FC Stegerman
//  Version     : v0.0.1
//  License     : GPLv3+
//
//  --                                                          ; }}}1

// -- TODO --
//
// * fix memory leak in tailrec
// * find efficient persistent data structures to use
//
// -- NOTES --
//
// * https://mathiasbynens.be/notes/javascript-unicode :(
//
// --

"use strict";
((_mod, _exp, _req, Rx) => {

// TODO
let mkInt, intToNum, strToInt
if (typeof BigInt === "undefined") {
  console.warn("warning: no BigInt support, falling back to Number")
  mkInt     = i => i|0
  intToNum  = i => i
  strToInt  = s => s.startsWith("0b") || s.startsWith("0x") ?
                   parseInt(s.slice(2), s.startsWith("0b") ? 2 : 16) :
                   parseInt(s, 10)
} else {
  mkInt     = BigInt
  intToNum  = Number
  strToInt  = BigInt
}

/* === error, stack, scope === */

class KonekoError extends Error {
  constructor(message, type, info = []) {
    super(message)
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, KonekoError)
    }
    this.name = "KonekoError"; this.type = type; this.info = info
  }
}

const KE = KonekoError

const E = {                                                   //  {{{1
  ParseError:         msg           => [`parse error: ${msg}`, [msg]],
  EvalUnexpected:     type          => [`cannot eval ${type}`, [type]],
  EvalScopelessBlock: ()            => ["cannot eval scopeless block"],
  ModuleNameError:    name          => [`no loaded module named ${name}`, [name]],
  ModuleLoadError:    name          => [`cannot load module ${name}`, [name]],
  NameError:          name          => [`name ${name} is not defined`, [name]],
  StackUnderflow:     ()            => ["stack underflow"],
  Expected:           (msg, u = "") => { const m = `${u}expected ${msg}`; return [m, [m]] },
  MultiMatchFailed:   (name, sig)   => [`no signature ${sig} for multi ${name}`, [name, sig]],
  UncomparableType:   type          => [`type ${type} is not comparable`, [type]],
  UncomparableTypes:  (t1, t2)      => [`types ${t1} and ${t2} are not comparable`, [t1, t2]],
  UncallableType:     type          => [`type ${type} is not callable`, [type]],
  UnapplicableType:   (type, op)    => [`type ${type} does not support ${op}`, [type, op]],
  UnknownField:       (field, type) => [`${type} has no field named ${field}`, [field, type]],
  EmptyList:          op            => [`${op}: empty list`, [op]],
  IndexError:         (op, index)   => [`${op}: index ${index} is out of range`, [op, index.toString()]],
  KeyError:           (op, key)     => [`${op}: key ${key} not found`, [op, key]],
  RangeError:         msg           => [`range error: ${msg}`, [msg]],
  DivideByZero:       ()            => ["divide by zero"],
  InvalidRx:          msg           => [`invalid regex: ${msg}`, [msg]],
  Fail:               msg           => [msg, [msg]],
  NotImplemented:     what          => [`not implemented: ${what}`, [what]],
}                                                             //  }}}1

for (const k in E) {
  const f = E[k]
  E[k] = (...args) => {
    const [msg, info] = f(...args)
    return [msg, k, info]
  }
}

const expect = (x, t, msg = `${t} on stack (not ${x.type})`) => {
  for (const t_ of t.split(" or ")) {
    if (x.type == t_) { return x }
  }
  throw new KE(...E.Expected(msg))
}

const chkIdent = i => {
  if (isIdent(i)) { return i }
  throw new KE(...E.Expected(`${i} to be a valid ident`))
}

const applyMissing = (x, op) =>
  `block to have parameter named ${x} for ${op}`

const isNil     = x => x.type == "nil"
const nilToDef  = (x, d, t) => isNil(x) ? d : expect(x, t).value
const maybeJ    = (f, x, d = nil) => x == null ? d : f(x)
const maybeK    = (f, x, d = null) => isNil(x) ? d : f(x)

// TODO: inefficient implementation
const stack = {                                               //  {{{1
  new: (...args) => args,
  null: s => !s.length,
  top: s => s.slice(-1)[0],
  push: (s, ...args) => s.concat(args),
  pop: (s0, ...args) => {
    if (s0.length < args.length) { throw new KE(...E.StackUnderflow()) }
    const s1 = s0.slice(), r = []
    args.reverse()
    for (const t of args) {
      const x = s1.pop()
      if (t != "_") { expect(x, t) }
      r.unshift(x)
    }
    return [r, s1]
  },
  popN: (s0, n) => stack.pop(s0, ..."_".repeat(n)),
  toArray: s => [...s].reverse(),
}                                                             //  }}}1

// TODO: inefficient implementation
const scope = {                                               //  {{{1
  new: (module = "__main__") => {
    if (!modules.has(module)) { modules.set(module, new Map()) }
    return { module, table: new Map() }
  },
  fork: (b, table) => {
    const c = b.scope, fv = freeVars(b.code), n = table.size
    for (const k of c.table.keys()) {
      if (!table.has(k) && fv.has(k)) { table.set(k, c.table.get(k)) }
    }
    return !n && table.size == c.table.size ? c : { ...c, table }
  },
  switch: (c0, c1) => {
    if (c0._meta) { return { _meta: c0._meta, ...c1 } }
    return c1
  },
  define: (c, k, v) => {
    const m = modules.get(c.module)
    if (c.module != "__main__" && m.has(k)) {
      putErr(`warning: redefining ${c.module}.${k}\n`)        //  TODO
    }
    m.set(k, v)
  },
  lookup: (c, k, d = undefined) => {
    for (const x of [modules.get("__prim__"), c.table]) {
      if (x.has(k)) { return x.get(k) }
    }
    const imps  = imports.get(c.module) || []
    const bp    = ["__bltn__", "__prld__"]
    const imbp  = [c.module, ...imps, ...bp]
    for (const m of c.module == "__prld__" ? bp : imbp) {
      const mod = modules.get(m)
      if (mod && mod.has(k)) { return mod.get(k) }
    }
    if (d !== undefined) { return d }
    throw new KE(...E.NameError(k))
  },
}                                                             //  }}}1

/* === values === */

const _tv   = (type, value) => ({ type, value })

const nil   = { type: "nil" }
const bool  = b => _tv("bool", !!b)
const int   = i => _tv("int", mkInt(i))
const float = f => _tv("float", f)
const str   = s => str_([...s])
const str_  = s => ({ type: "str", list: s, _val: null })
const kwd   = s => _tv("kwd", s)
const pair  = (key, value) => ({ type: "pair", key, value })
const list  = l => _tv("list", l)
const dict  = ps => dict_(new Map(ps.map(p => [p.key.value, p.value])))
const dict_ = m => _tv("dict", m)
const ident = i => _tv("ident", chkIdent(i))
const quot  = i => _tv("quot" , chkIdent(i))

const block = (params, code, scope = null, name = undefined) =>
  ({ type: "block", params, code, scope, name })
const builtin = (name, run, prim = true) =>
  ({ type: "builtin", prim, name, run })
const multi = (arity, name, table) =>
  ({ type: "multi", arity, name, table })
const record_type = (name, fields) =>
  ({ type: "record-type", name, fields })

const record = (rectype, values) => {
  if (values.length != rectype.fields.length) {
    throw new KE(...E.Expected(
      `${rectype.fields.length} arg(s) for record ${rectype.name}`
    ))
  }
  return { type: "record", rectype, values }
}

const thunk = f => {                                          //  {{{1
  let v
  // NB: Promise
  const run = async () => {
    if (f) {
      const vs = await f()
      if (vs.length != 1) {
        throw new KE(...E.Expected("thunk to produce exactly 1 value"))
      }
      v = vs[0]; f = null
    }
    return v
  }
  return { type: "thunk", run }
}                                                             //  }}}1

const strVal = s => {
  if (s._val == null) { s._val = s.list.join("") }
  return s._val
}

const T = bool(true), F = bool(false)

const mkPrim    = (name, f) => [name, builtin(name, f)]
const mkPrimPP  = (name, f, ...xs) => mkPrim(name, popPush(f, ...xs))
const primPP    = (name, g = x => x) => (f, ...xs) =>
  g(builtin(name, popPush(f, ...xs)))

const freeVars = (vs, seen = {}, free = new Set()) => {       //  {{{1
  for (const v of vs) {
    switch (v.type) {
      case "list":
        freeVars(v.value, seen, free)
        break
      case "ident":
      case "quot":
        if (!seen[v.value]) { free.add(v.value) }
        break
      case "block":
        for (const k of v.params) { seen[k] = (seen[k] || 0) + 1 }
        freeVars(v.code, seen, free)
        for (const k of v.params) { seen[k] = (seen[k] || 0) - 1 }
        break
    }
  }
  return free
}                                                             //  }}}1

const digitParams = b => {                                    //  {{{1
  let n = 0
  for (const k of freeVars(b.code)) {
    let m
    if (m = /^__([1-9])__$/u.exec(k)) {
      n = Math.max(n, parseInt(m[1], 10))
    }
  }
  return Array.from(Array(n), (_, i) => "__" + (i+1).toString() + "__")
}                                                             //  }}}1

const truthy = x =>
  !(isNil(x) || (x.type == "bool" && x.value == false))

const dictToList = d => list([...d.value.keys()].sort().map(
  k => pair(kwd(k), d.value.get(k))
))

const recordToDict = r => dict(zip(r.rectype.fields, r.values).map(
  ([k, v]) => pair(kwd(k), v)
))

/* === parsing === */

const isIdent = s => Rx("^"+_idt+"$", "u").test(s)
const isInt   = s => Rx("^"+_int+"$", "u").test(s)
const isFloat = s => Rx("^"+_flt+"$", "u").test(s)

const isPrint = s =>
  Rx("^[\\p{L}\\p{M}\\p{N}\\p{P}\\p{S}\\p{Zs}]+$", "u").test(s)

const _int      = "(?:-?\\d+|0x[0-9a-fA-F]+|0b[01]+)"
const _flt      = "(?:-?\\d+(?:\\.\\d+e\\d+|\\.\\d+|e\\d+))"
const _spc      = "(?:[\\s,]|;[^\\n]*(?:\\n|$))+"

const _naiveId  = "[\\p{L}\\p{N}\\p{S}\\(\\)\\{\\}\\[\\]@%&*\\-_\\/?'!:]+"
const _badId    = "['!:]|(?:[^\\s,]+[:({\\[]|[(){}\\[\\]]|"+
                  _int+"|"+_flt+")(?:"+_spc+"|$)"
const _idt      = "(?:(?!"+_badId+")"+_naiveId+")"

const pInt      = s => isInt  (s) ? strToInt  (s) : null
const pFloat    = s => isFloat(s) ? parseFloat(s) : null

const parseOne = (s, p0, fname, end = null) => {              //  {{{1
  let m, p1
  const t = pat => {
    const r = Rx("("+pat+")(?:"+_spc+"|$)", "usy")
    r.lastIndex = p0; m = r.exec(s); p1 = r.lastIndex
    return !!m
  }
  const hd  = "[0-9a-fA-F]"
  const hex = '(\\\\x'+hd+'{2})|(\\\\u'+hd+'{4})|(\\\\U'+hd+'{8})'
  const chr = "(?:" + hex + '|(\\\\[rnt\\\\"])|(\\\\.|[^\\\\"])' + ")"
  const esc = { "\\r":"\r", "\\n":"\n", "\\t":"\t", "\\\"":"\"", "\\\\":"\\" }
  const escapedStrBody = k => {
    const l = [], r = Rx(chr, "usy"); r.lastIndex = p0 + k
    const f = w => String.fromCodePoint(parseInt(cm[0].slice(w), 16))
    let cm
    while (cm = r.exec(s)) {
      l.push(cm[1] || cm[2] || cm[3] ? f(2) : cm[4] ? esc[cm[4]] : cm[5])
    }
    return l.join("")
  }
  const parseBlock = (pre = "") => {
    let params = [], p2 = p1
    if (t(pre+"\\["+_spc+"((?:"+_idt+_spc+")*?)"+"([.\\]])")) {
      if (m[3] == ".") {
        params = m[2].split(Rx(_spc, "us")).filter(x => x); p2 = p1
      }
    }
    const [p3, vs] = parseMany(s, p2, fname, "]")
    return [p3, block(params, vs)]
  }

  // primitives
  if (t("nil")) {
    return [p1, nil]
  } else if (t("#t") || t("#f")) {
    return [p1, bool(m[1] == "#t")]
  } else if (t(_int)) {
    return [p1, int(strToInt(m[1]))]
  } else if (t(_flt)) {
    return [p1, float(parseFloat(m[1]))]
  } else if (t('"(' + chr + '*)"')) {
    return [p1, str(escapedStrBody(1))]
  } else if (t(":(" + _idt + ")")) {
    return [p1, kwd(m[2])]
  } else if (t(':"(' + chr + '*)"')) {
    return [p1, kwd(escapedStrBody(2))]
  }
  // values
  else if (t("\\(\\)")) {
    return [p1, list([])]
  } else if (t("\\(")) {
    const [p2, vs] = parseMany(s, p1, fname, ")")
    return [p2, list(vs)]
  } else if (t(_idt)) {
    return [p1, ident(m[1])]
  } else if (t("'(" + _idt + ")")) {
    return [p1, quot(m[2])]
  } else if (t("\\[")) {
    return parseBlock()
  }
  // sugar
  else if (t("\\.{3}")) {
    return [p1, ident("__ellipsis__")]
  } else if (t("('?)(" + _idt + ")\.(" + _idt + ")")) {
    return [p1, kwd(m[4]), kwd(m[3]), ident("__module-get__"),
            ...(m[2] ? [] : [ident("__call__")])]
  } else if (t("(['.])\\[")) {
    const dot = m[2] == "."
    const [p2, b] = parseBlock("['.]")
    const more = dot ? [ident("__call__")] : []
    return [p2, block(digitParams(b), [b]), ...more]
  } else if (t("['.]([1-9])")) {
    return [p1, (m[1][0] == "'" ? quot : ident)("__"+m[2]+"__")]
  } else if (t("('?)([.!])(" + _idt + ")")) {
    const sw = ident("__swap__"), ca = ident("__call__")
    const vs = [kwd(m[4]), sw, ca, ...(m[3] == "!" ? [ca]: [])]
    return [p1, ...(m[2] ? [block([], vs)] : vs)]
  } else if (t("\\{")) {
    const [p2, vs] = parseMany(s, p1, fname, "}")
    return [p2, list(vs), ident("__dict__")]
  } else if (t("(" + _naiveId + "):") && isIdent(m[2])) {
    const [p2, ...v] = parseOne(s, p1, fname)
    return [p2, kwd(m[2]), ...v, ident("__=>__")]
  } else if (t("(" + _naiveId + ")([({])") && isIdent(m[2])) {
    const curly = m[3] == "{"
    const [p2, vs] = parseMany(s, p1, fname, curly ? "}" : ")")
    const l = list(vs), q = quot(m[2])
    if (curly) {
      return [p2, l, ident("__dict__"), q, ident("__apply-dict__")]
    } else {
      return [p2, l, q, ident("__apply__")]
    }
  } else if (t("(" + _naiveId + ")\\[") && isIdent(m[2])) {
    const i = ident(m[2])
    const [p2, b] = parseBlock(_naiveId)
    return [p2, b, i]
  }
  // end of nested
  else if (t("[)}\\]]") && end == m[1]) {
    return [p1, { end }]
  }
  throw new KE(...E.ParseError(parseError(s, p0, fname, "no match")))
}                                                             //  }}}1

const parseMany = (s, p, fname, end = null) => {              //  {{{1
  const vs = []; let v
  while (p < s.length) {
    [p, ...v] = parseOne(s, p, fname, end)
    if (end && v[0].end) { return [p, vs] }
    vs.push(...v)
  }
  if (end) {
    throw new KE(...E.ParseError(parseError(s, p, fname, `expected "${end}"`)))
  }
  return [p, vs]
}                                                             //  }}}1

const parseError = (s, p, fn, e) => {
  const back = s.lastIndexOf("\n", p), front = s.indexOf("\n", p)
  const b = back  != -1 ? back+1 : 0
  const f = front != -1 ? front  : s.length
  const l = s.slice(b, f) || "<empty line>"
  const n = s.slice(0, p).split("\n").length                  // FIXME
  const x = " ".repeat(n.toString().length), y = " ".repeat(p - b)
  return `${fn}:${n}:${p-b+1}:\n${x} |\n${n} | ${l}\n${x} | ${y}^\n${e}`
}

const read = (s, fname = "(read)") => {
  let p = 0, m
  if (m = /^#!.*\n/u.exec(s)) { p = m[0].length }
  const r = Rx(_spc, "usy"); r.lastIndex = p
  if (m = r.exec(s)) { p += m[0].length }
  return parseMany(s, p, fname)[1]
}

/* === evaluation === */

/*
const pushSelf  = x => (c, s) => [false, stack.push(s, x)]
const pushIdent = x => (c, s) =>
  [false, stack.push(s, scope.lookup(c, x.value))]
*/

const popArgs = (s0, params) => {
  const [vs, s1] = stack.popN(s0, params.length)
  return [zip(params, vs), s1]
}

const popPush = (f, ...parms) => async (c, s0) => {
  const [args, s1]  = stack.pop(s0, ...parms)
  const results     = await f(...args)
  return stack.push(s1, ...results)
}

const pop2push = f => popPush(f, "_", "_")

const opI = op => popPush(
  (x, y) => [int(op(x.value, y.value))], "int", "int"
)

const opF = op => popPush(
  (x, y) => [float(op(x.value, y.value))], "float", "float"
)

const opF1 = op => popPush(x => [float(op(x.value))], "float")

const partitionSpecial = (params, cur = null) => {            //  {{{1
  const sparms = [], sparms_ = [], nparms = []
  for (const p of params) {
    if (["&", "&&"].includes(p)) {
      sparms.push(p); if (cur && p != cur) { sparms_.push(p) }
    } else {
      nparms.push(p)
    }
  }
  return cur ? [sparms, sparms_, nparms] : [sparms, nparms]
}                                                             //  }}}1

const dictLookup = (op, d, ks) => ks.map(k => {
  if (!d.has(k)) { throw new KE(...E.KeyError(op, k)) }
  return d.get(k)
})

const dictWithout = (d, ks) =>
  dict_(new Map([...d.value.entries()].filter(
    ([k, v]) => !ks.includes(k)
  )))

const debug = (c, f) => {
  if (eq(scope.lookup(c, "__debug__", F), T)) { f() }
}

class Eval {
  constructor(b, c, s) {
    this.block = b; this.scope = c; this.stack = s
  }
}

// TODO
// NB: maybe Promise, maybe Eval (if tailPos)
const call = (c0, s0, tailPos = false) => {                   //  {{{1
  debug(c0, () => putErr("*** call ***\n"))
  const [[x], s1] = stack.pop(s0, "_"), xv = x.value
  const popOp = () => {
    const [[k], s2] = stack.pop(s1, "kwd")
    const r = (...xs) => stack.push(s2, ...xs)
    return [k.value, x.type + "." + k.value, r]
  }
  const has = (i, l = xv.length) => 0 <= i && i < l
  switch (x.type) {
    case "str": {
      // TODO: inefficient implementation
      const xl = x.list
      const [opv, op, r] = popOp(), p = primPP(op, r)
      switch (opv) {
        case "ord":
          if (xl.length != 1) {
            throw new KE(...E.Expected(
              `str of length 1 on stack (not str of length ${xl.length})`
            ))
          }
          return r(int(xl[0].codePointAt(0)))
        case "lower":
          return r(str_(xl.map(c => c.toLowerCase())))
        case "upper":
          return r(str_(xl.map(c => c.toUpperCase())))
        case "reverse":
          return r(str_([...xl].reverse()))
        case "trim":
          return r(str(strVal(x).trim()))
        case "triml":
          return r(str(strVal(x).trimStart()))
        case "trimr":
          return r(str(strVal(x).trimEnd()))
        case "starts-with?":
          return p(y => [bool(strVal(x).startsWith(strVal(y)))], "str")
        case "ends-with?":
          return p(y => [bool(strVal(x).endsWith(strVal(y)))], "str")
        case "->list":
          return r(list(xl.map(str)))
        case "append":
          return p(y => [str_(y.list.concat(xl))], "str")
        case "slice":
          return p((i, j, step) => {
            const i_ = intToNum(nilToDef(i, 0        , "int"))
            const j_ = intToNum(nilToDef(j, xl.length, "int"))
            if (step.value != 1) {
              throw new KE(...E.NotImplemented(`${op}: step other than 1`))
            }
            return [str_(xl.slice(i_, j_ < 0 ? xl.length + j_ : j_))]
          }, "_", "_", "int")
        case "empty?":
          return r(bool(!xl.length))
        case "len":
          return r(int(xl.length))
        case "get^":
          return p(i => {
            const i_ = intToNum(i.value)
            if (!has(i_, xl.length)) {
              throw new KE(...E.IndexError(op, i_))
            }
            return [str(xl[i_])]
          }, "int")
        case "has?":
          return p(i => [bool(has(intToNum(i.value), xl.length))], "int")
        case "elem?":
          return p(y =>
            [bool(strVal(x).includes(strVal(y)))], "str"
          )
        case "index":
          return p(y => {
            if (!y.list.length) return [int(0)]
            for (let i = 0; i < xl.length; ++i) {
              if (i + y.list.length >= xl.length) break
              const zl = xl.slice(i, i + y.list.length)
              if (eqArray(zl, y.list, eqPrim)) return [int(i)]
            }
            return [nil]
          }, "str")
        default:
          throw new KE(...E.UnknownField(op, x.type))
      }
    }
    case "pair": {
      const [opv, op, r] = popOp()
      switch (opv) {
        case "key":
          return r(x.key)
        case "value":
          return r(xv)
        default:
          throw new KE(...E.UnknownField(op, x.type))
      }
    }
    case "list": {
      // TODO: inefficient implementation
      const [opv, op, r] = popOp(), p = primPP(op, r)
      const g = () => { if (!xv.length) { throw new KE(...E.EmptyList(op)) } }
      switch (opv) {
        case "head^":
          g(); return r(xv[0])
        case "tail^":
          g(); return r(list(xv.slice(1)))
        case "uncons^":
          g(); return r(xv[0], list(xv.slice(1)))
        case "cons":
          return p(y => [list([y].concat(xv))], "_")
        case "sort":
          return r(list(xv.slice().sort(cmp)))
        case "sort'":
          return r(list(xv.slice().sort((x, y) => cmp(x, y, false))))
        case "append":
          return p(y => [list(y.value.concat(xv))], "list")
        case "slice":
          return p((i, j, step) => {
            const i_ = intToNum(nilToDef(i, 0        , "int"))
            const j_ = intToNum(nilToDef(j, xv.length, "int"))
            if (step.value != 1) {
              throw new KE(...E.NotImplemented(`${op}: step other than 1`))
            }
            return [list(xv.slice(i_, j_ < 0 ? xv.length + j_ : j_))]
          }, "_", "_", "int")
        case "empty?":
          return r(bool(!xv.length))
        case "len":
          return r(int(xv.length))
        case "get^":
          return p(i => {
            const i_ = intToNum(i.value)
            if (!has(i_)) { throw new KE(...E.IndexError(op, i_)) }
            return [xv[i_]]
          }, "int")
        case "has?":
          return p(i => [bool(has(intToNum(i.value)))], "int")
        case "elem?":
          return p(y => [bool(xv.find(z => eq(z, y)))], "_")
        default:
          throw new KE(...E.UnknownField(op, x.type))
      }
    }
    case "dict": {
      // TODO: inefficient implementation
      const [opv, op, r] = popOp(), p = primPP(op, r)
      switch (opv) {
        case "keys":
          return r(list([...xv.keys()].map(kwd)))
        case "values":
          return r(list([...xv.values()]))
        case "pairs":
          return r(list([...xv.entries()].sort().map(
            ([k, v]) => pair(kwd(k), v)
          )))
        case "merge":
          return p(y => [dict_(new Map([...y.value, ...xv]))], "dict")
        case "delete":
          return p(k =>
            [dict_(new Map([...xv].filter(([k_, v]) => k_ != k.value)))]
          , "kwd")
        case "empty?":
          return r(bool(!xv.size))
        case "len":
          return r(int(xv.size))
        case "get^":
          return p(k => {
            if (!xv.has(k.value)) {
              throw new KE(...E.KeyError(op, k.value))
            }
            return [xv.get(k.value)]
          }, "kwd")
        case "has?":
          return p(k => [bool(xv.has(k.value))], "kwd")
        default:
          throw new KE(...E.UnknownField(op, x.type))
      }
    }
    case "block": {
      const [sparms, nparms] = partitionSpecial(x.params)
      const cm = "__caller-module__"
      const nparms_ = nparms.filter(x => x != cm)
      const cma = nparms.includes(cm) ? [[cm, kwd(c0.module)]] : []
      const [as, s2] = popArgs(s1, nparms_)
      const as_ = as.concat(sparms.map(p => [p, nil])).concat(cma)
      const c1 = scope.switch(c0, scope.fork(x, new Map(as_)))
      if (tailPos) { return new Eval(x, c1, s2) }
      return tick(evaluate(x)(c1, s2))
    }
    case "builtin":
      return x.run(c0, s1)
    case "multi": {
      const sig = stack.popN(s1, x.arity)[0].map(
        v => v.type == "record" ? show(v.rectype) : v.type
      )
      const b = x.table.get(sig.join("###")) ||
                x.table.get(sig.map(_ => "_").join("###"))
      if (!b) {
        throw new KE(...E.MultiMatchFailed(x.name, show(list(sig.map(kwd)))))
      }
      return call(c0, stack.push(s1, b))
    }
    case "record-type": {
      const [l, s2] = stack.popN(s1, x.fields.length)
      return stack.push(s2, record(x, l))
    }
    case "record": {
      const [[k], s2] = stack.pop(s1, "kwd")
      const i = x.rectype.fields.indexOf(k.value)
      if (i == -1) {
        throw new KE(...E.UnknownField(k.value, x.rectype.name))
      }
      return stack.push(s2, x.values[i])
    }
    case "thunk":
      // OK b/c call() always returns a Promise
      return x.run().then(y => stack.push(s1, y))
    default:
      throw new KE(...E.UncallableType(x.type))
  }
}                                                             //  }}}1

// TODO
// NB: maybe Promise
const apply = (c0, s0) => {                                   //  {{{1
  debug(c0, () => putErr("*** apply ***\n"))
  const [[x], s1] = stack.pop(s0, "_"), xv = x.value
  switch (x.type) {
    case "block": {
      const [sparms, sparms_, nparms] = partitionSpecial(x.params, "&")
      const [[l], s2] = stack.pop(s1, "list")
      if (l.value.length < nparms.length) {
        throw new KE(...E.Expected(`${nparms.length} arg(s) for apply`))
      }
      if (l.value.length > nparms.length && !sparms.includes("&")) {
        throw new KE(...E.Expected(applyMissing("&", "apply")))
      }
      const l1 = l.value.slice(0, nparms.length)
      const l2 = l.value.slice(nparms.length)
      const as = new Map(zip(nparms, l1).concat(sparms_.map(p => [p, nil]))
                                        .concat([["&", list(l2)]]))
      const c1 = scope.switch(c0, scope.fork(x, as))
      return evaluate(x)(c1).then(s3 => stack.push(s2, ...s3))
    }
    case "multi":
      throw new KE(...E.NotImplemented("apply multi"))
    case "record-type": {
      const [[l], s2] = stack.pop(s1, "list")
      return stack.push(s2, record(x, l.value))
    }
    default:
      throw new KE(...E.UnapplicableType(x.type, "apply"))
  }
}                                                             //  }}}1

// TODO
// NB: maybe Promise
const apply_dict = (c0, s0) => {                              //  {{{1
  debug(c0, () => putErr("*** apply-dict ***\n"))
  const [[x], s1] = stack.pop(s0, "_"), xv = x.value
  switch (x.type) {
    case "block": {
      const [sparms, sparms_, nparms] = partitionSpecial(x.params, "&&")
      const [[d], s2] = stack.pop(s1, "dict")
      if (!sparms.includes("&&")) {
        throw new KE(...E.Expected(applyMissing("&&", "apply-dict")))
      }
      const vs = dictLookup("apply-dict", d.value, nparms)
      const d_ = dictWithout(d, nparms)
      const as = new Map(zip(nparms, vs).concat(sparms_.map(p => [p, nil]))
                                        .concat([["&&", d_]]))
      const c1 = scope.switch(c0, scope.fork(x, as))
      return evaluate(x)(c1).then(s3 => stack.push(s2, ...s3))
    }
    case "multi":
      throw new KE(...E.NotImplemented("apply-dict multi"))
    case "record-type": {
      const [[d], s2] = stack.pop(s1, "dict")
      const uf = [...d.value.keys()].filter(k => !x.fields.includes(k))
      if (uf.length) {
        throw new KE(...E.Expected(
          `key(s) ${uf.join(", ")} for record ${x.name}`, "un"
        ))
      }
      const l = dictLookup("record-type.apply-dict", d.value, x.fields)
      return stack.push(s2, record(x, l))
    }
    default:
      throw new KE(...E.UnapplicableType(x.type, "apply-dict"))
  }
}                                                             //  }}}1

// TODO
// NB: Promise
const evaluate = x => {                                       //  {{{1
  if (x._eval) { return x._eval }
  const comp = x => x._ccode || (x._ccode = x.code.map(compile))
  const ev = async (c = scope.new(), s = stack.new()) => {
    let code = comp(x)
    for (let i = 0; i < code.length; ++i) {
      if (c._meta && c._meta.abort) { throw new Error("ABORT") }
      const f = code[i]
      debug(c, () => {
        putErr(`==> eval ${show(f)}\n`)
        putErr(`--> ${s.map(show).join(" ")}\n`)
      })
      s = await f.run(c, s)
      debug(c, () => putErr(`<-- ${(s.it || s).map(show).join(" ")}\n`))
      if (s instanceof Defer) {
        const tailPos = i == code.length - 1
        s = await call(c, s.it, tailPos)
        if (tailPos && s instanceof Eval) {
          debug(c, () => putErr("*** tail call ***\n"))
          code = comp(s.block); c = scope.switch(c, s.scope)
          s = s.stack; i = -1
        }
      }
    }
    return s
  }
  return x._eval = ev
}                                                             //  }}}1

/*
// -> [boolean, stack | Promise]
const evl = {
  nil: pushSelf, bool: pushSelf, int: pushSelf, float: pushSelf,
  str: pushSelf, kwd: pushSelf, quot: pushIdent,
  list:  x => (c, s) => [false, evaluate({ code: x.value })(c).then(
                                  l => stack.push(s, list(l))
                                )],
  ident: x => (c, s) => [true , pushIdent(x)(c, s)[1]],
  block: x => (c, s) => [false, stack.push(s, { ...x, scope: c })],
}
*/

// NB: Promise
const evalText = (text, c = undefined, s = undefined, fn = undefined) =>
  evaluate({ code: read(text, fn) })(c, s)

/* === compilation === */

class Defer { constructor(it) { this.it = it } }

// TODO
// NB: maybe Defer
const compile = x => {                                        //  {{{1
  const b = f => {
    const b = builtin("__compiled__", f); b._src = x; return b
  }
  const p = (c, s) => stack.push(s, scope.lookup(c, x.value))
  switch (x.type) {
    case "nil":
    case "bool":
    case "int":
    case "float":
    case "str":
    case "kwd":
      return b((c, s) => stack.push(s, x))
    case "list":
      return b((c, s) => evaluate({ code: x.value })(c)
                         .then(l => stack.push(s, list(l))))
    case "ident":
      return b((c, s) => new Defer(p(c, s)))
    case "quot":
      return b(p)
    case "block":
      return b((c, s) => stack.push(s, { ...x, scope: c }))
    default:
      throw new Error("WTF")
  }
}                                                             //  }}}1

/* === show, toJS, fromJS === */

const show = x => {                                           //  {{{1
  switch (x.type) {
    case "nil":
      return "nil"
    case "bool":
      return x.value ? "#t" : "#f"
    case "int":
      return x.value.toString()
    case "float": {
      if (x.value == 0 && 1/x.value < 0) { return "-0.0" }
      const s = x.value.toString()
      return isInt(s) ? s + ".0" : s
    }
    case "str": {
      const f = c => e["="+c] || (isPrint(c) ? c : h(c.codePointAt(0)))
      const h = n => n <= 0xffff ? p("\\u", 4, n) : p("\\U", 8, n)
      const p = (pre, w, n) => pre + n.toString(16).padStart(w, '0')
      const e = { "=\r":"\\r", "=\n":"\\n", "=\t":"\\t", "=\"":"\\\"",
                  "=\\":"\\\\" }
      return '"' + x.list.map(f).join("") + '"'
    }
    case "kwd":
      return ":" + (isIdent(x.value) ? x.value : show(str(x.value)))
    case "pair":
      return show(x.key) + " " + show(x.value) + " =>"
    case "list":
      return x.value.length ?
        "( " + x.value.map(show).join(" ") + " )" : "()"
    case "dict": {
      const f = ([k, v]) => show(kwd(k)) + " " + show(v) + " =>"
      const kv = Array.from(x.value.entries(), f).sort().join(", ")
      return x.value.size ? "{ " + kv + " }" : "{ }"
    }
    case "ident":
      return x.value
    case "quot":
      return "'" + x.value
    case "block": {
      if (x.scope && x.scope.module == null) {
        return [...x.scope.table.values()].map(show).join(" ")
          + " " + x.name
      }
      const f = xs => xs.map(show).join(" ")
      if (!x.params.length && !x.code.length) {
        return "[ ]"
      } else if (!x.params.length) {
        return "[ " + f(x.code) + " ]"
      } else if (!x.code.length) {
        return "[ " + x.params.join(" ") + " . ]"
      } else {
        return "[ " + x.params.join(" ") + " . " + f(x.code) + " ]"
      }
    }
    case "builtin":
      if (x.name == "__compiled__") {
        return "#<compiled:" + show(x._src) + ">"
      }
      return "#<" + (x.prim ? "primitive" : "builtin") + ":" + x.name + ">"
    case "multi":
      return "#<multi:" + x.arity + ":" + x.name + ">"
    case "record-type":
      return "#<record-type:" + x.name + "(" + x.fields.join("#") + ")>"
    case "record": {
      const flds = zip(x.rectype.fields, x.values).map(
        ([k, v]) => show(pair(kwd(k), v))
      )
      return x.rectype.name + "{ " + flds.join(", ") + " }"
    }
    case "thunk":
      return "#<thunk>"
    default:
      throw new Error(`type ${x.type} is not showable`)
  }
}                                                             //  }}}1

const toJS = (x, options = {}) => {                           //  {{{1
  const opts = Object.assign({ useObj: false }, options)
  switch (x.type) {
    case "nil":
      return null
    case "bool":
    case "int":
    case "float":
    case "kwd":
      return x.value
    case "str":
      return strVal(x)
    case "pair":
      return [x.key.value, toJS(x.value, opts)]
    case "list":
      return x.value.map(y => toJS(y, opts))
    case "dict":
      if (opts.useObj) {
        const o = {}
        for (const [k, v] of x.value.entries()) {
          o[k] = toJS(v, opts)
        }
        return o
      } else {
        return new Map(Array.from(x.value.entries(),
          ([k, v]) => [k, toJS(v, opts)]
        ))
      }
    case "record": {
      const o = { __koneko_type__: x.rectype.name }
      for (const [k, v] of zip(x.rectype.fields, x.values)) {
        o[k] = toJS(v, opts)
      }
      return o
    }
    default:
      throw new Error(`toJS: cannot convert ${x.type}`)
  }
}                                                             //  }}}1

const fromJS = (x, options = {}) => {                         //  {{{1
  const opts = Object.assign({ useObj: false }, options)
  switch (typeof x) {
    case "boolean":
      return bool(x)
    case "bigint":
      return int(x)
    case "number":
      return (x == (x|0)) ? int(x) : float(x)
    case "string":
      return str(x)
    case "object":
      if (x === null) {
        return nil
      } else if (x instanceof Array) {
        return list(x.map(y => fromJS(y, opts)))
      } else if (x instanceof Map) {
        return dict(Array.from(x.entries(),
          ([k, v]) => pair(kwd(k), fromJS(v, opts))
        ))
      } else if (opts.useObj) {
        return dict(Array.from(Object.entries(x),
          ([k, v]) => pair(kwd(k), fromJS(v, opts))
        ))
      }
    default:
      throw new Error(`fromJS: cannot convert ${x}`)
  }
}                                                             //  }}}1

/* === eq, cmp === */

const eq = (x, y) => {                                        //  {{{1
  if (x.type != y.type) { return false }
  const a = x.value, b = y.value
  switch (x.type) {
    case "nil":
      return true
    case "float":
      // if (a === b) { return a !== 0 || 1 / a === 1 / b }
      // if (a !== a) { return b !== b }
      // return a == b
    case "bool":
    case "int":
    case "kwd":
    case "ident":
    case "quot":
      return a == b
    case "str":
      return eqArray(x.list, y.list, eqPrim)
    case "pair":
      return eq(x.key, y.key) && eq(a, b)
    case "list":
      return eqArray(a, b, eq)
    case "dict": {
      if (a.size != b.size) { return false }
      const ka = [...a.keys()].sort()
      const kb = [...b.keys()].sort()
      if (!eq(list(ka.map(kwd)), list(kb.map(kwd)))) { return false }
      for (const k of ka) {
        if (!eq(a.get(k), b.get(k))) { return false }
      }
      return true
    }
    case "record-type":
      return x.name == y.name && eqArray(x.fields, y.fields, eqPrim)
    case "record":
      return eq(x.rectype, y.rectype) && eqArray(x.values, y.values, eq)
    default:
      throw new KE(...E.UncomparableType(x.type))
  }
}                                                             //  }}}1

const cmp = (x, y, total = true) => {                         //  {{{1
  if (x.type != y.type) {
    if (total) {
      return cmpPrim(types.indexOf(x.type), types.indexOf(y.type))
    }
    if (x.type == "int" && y.type == "float") {
      x = float(intToNum(x.value))
    } else if (x.type == "float" && y.type == "int") {
      y = float(intToNum(y.value))
    } else {
      throw new KE(...E.UncomparableTypes(x.type, y.type))
    }
  }
  const f = (x, y) => cmp(x, y, total)
  const a = x.value, b = y.value
  switch (x.type) {
    case "nil":
      return 0
    case "float":
      // return eq(x, y) ? 0 : a < b ? -1 : b < a ? 1 : null
    case "bool":
    case "int":
    case "kwd":
    case "ident":
    case "quot":
      return cmpPrim(a, b)
    case "str":
      return cmpArray(x.list, y.list, cmpPrim)
    case "pair":
      return cmpArray([x.key, a], [y.key, b], f)
    case "list":
      return cmpArray(a, b, f)
    case "dict":
      return f(dictToList(x), dictToList(y))
    case "record-type":
      return cmpArray([x.name].concat(x.fields),
                      [y.name].concat(y.fields), cmpPrim)
    case "record": {
      const c = f(x.rectype, y.rectype)
      return c != 0 ? c : f(recordToDict(x), recordToDict(y))
    }
    default:
      throw new KE(...E.UncomparableType(x.type))
  }
}                                                             //  }}}1

const eqArray = (x, y, eq) => {
  if (x.length != y.length) { return false }
  for (let i = 0; i < x.length; ++i) {
    if (!eq(x[i], y[i])) { return false }
  }
  return true
}

const cmpArray = (x, y, cmp) => {                             //  {{{1
  const m = Math.max(x.length, y.length)
  for (let i = 0; i < m; ++i) {
    if (i >= x.length && i < y.length) { return -1 }
    if (i < x.length && i >= y.length) { return  1 }
    const c = cmp(x[i], y[i])
    if (c != 0) { return c }
  }
  return 0
}                                                             //  }}}1

const eqPrim  = (a, b) => a == b
const cmpPrim = (a, b) => a == b ? 0 : a < b ? -1 : 1

const cmp_eq  = c => bool(c ==  0)
const cmp_neq = c => bool(c !=  0)
const cmp_lt  = c => bool(c == -1)
const cmp_lte = c => bool(c == -1 || c == 0)
const cmp_gt  = c => bool(c ==  1)
const cmp_gte = c => bool(c ==  1 || c == 0)

/* === modules & primitives === */

const imports = new Map()
const modules = new Map([["__prld__", new Map()]])

const defMain = () => modules.set("__main__",
  new Map([["__args__", list([])], ["__repl__", F]]))

defMain()

modules.set("__prim__", new Map([                             //  {{{1
  mkPrim("__call__", call),
  mkPrim("__apply__", apply),
  mkPrim("__apply-dict__", apply_dict),
  mkPrim("__if__", (c, s0) => {
    const [[b, tb, fb], s1] = stack.popN(s0, 3)
    return call(c, stack.push(s1, truthy(b) ? tb : fb))
  }),
  mkPrim("__def__", (c, s0) => {
    const [[k, v], s1] = stack.pop(s0, "kwd", "_")
    scope.define(c, k.value, v); return s1
  }),
  mkPrim("__defmulti__", (c, s0) => {
    const [[k, sig, b], s1] = stack.pop(s0, "kwd", "list", "block")
    for (const x of sig.value) { expect(x, "kwd") }
    const sig_ = sig.value.map(
      k =>  k.value == "_" || types.includes(k.value) ? k.value :
              show(expect(scope.lookup(c, k.value), "record-type"))
    )
    let m = modules.get(c.module).get(k.value)        // module scope!
    if (m) {
      expect(m, "multi", `${k.value} to be a multi`)
      if (m.arity != sig_.length) {
        throw new KE(...E.Expected(`multi ${m.name} to have arity ${m.arity}`))
      }
    } else {
      m = multi(sig_.length, k.value, new Map())
      scope.define(c, k.value, m)
    }
    m.table.set(sig_.join("###"), b)
    return s1
  }),
  mkPrim("__defrecord__", (c, s0) => {
    const [[k, fs], s1] = stack.pop(s0, "kwd", "list")
    for (const x of fs.value) { expect(x, "kwd") }
    const nm = k.value, t = record_type(nm, fs.value.map(x => x.value))
    const is_t = x => x.type == "record" && eq(x.rectype, t)
    const notPrim = x => { x.prim = false; return x }
    const pred = k => primPP(k, notPrim)(x => [bool(is_t(x))], "_")
    const mat = k => builtin(k, (c, s0) => {
      const [[x, f], s1] = stack.popN(s0, 2)
      if (!is_t(x)) {
        const what = x.type == "record" ?
          `record of type ${x.rectype.name}` : x.type
        throw new KE(...E.Expected(
          `record of type ${nm} on stack (not ${what})`
        ))
      }
      return call(c, stack.push(s1, ...x.values, f))
    }, false)
    const mat_ = k => builtin(k, (c, s0) => {
      const [[x, f, g], s1] = stack.popN(s0, 3)
      return call(c, stack.push(s1, ...(is_t(x) ? [...x.values, f] : [g])))
    }, false)
    scope.define(c, nm, t)
    for (const [k,f] of [[nm+"?",pred], ["^"+nm,mat], ["~"+nm,mat_]]) {
      scope.define(c, k, f(c.module + ":" + k))
    }
    return s1
  }),
  mkPrimPP("__=>__", (k, v) => [pair(k, v)], "kwd", "_"),
  mkPrimPP("__dict__", l => {
    for (const x of l.value) { expect(x, "pair") }
    return [dict(l.value)]
  }, "list"),
  mkPrim("__swap__", pop2push((x, y) => [y, x])),
  mkPrimPP("__show__", x => [str(show(x))], "_"),
  mkPrimPP("__puts!__", x => { puts(strVal(x)); return [] }, "str"),
  mkPrim("__ask!__", (c, s0) => {
    const [[x], s1] = stack.pop(s0, "str or nil")
    const p = maybeK(strVal, x)
    return ask(p).then(r => stack.push(s1, maybeJ(str, r)))
  }),
  mkPrimPP("__types__", () => [list(types.map(kwd))]),
  mkPrimPP("__type__", x => [kwd(x.type, "_")], "_"),
  mkPrimPP("__callable?__",
    x => [bool(callableTypes.includes(x.type))], "_"
  ),
  mkPrimPP("__function?__",
    x => [bool(functionTypes.includes(x.type))], "_"
  ),
  mkPrim("__defmodule__", (c, s0) => {
    const [[k, b], s1] = stack.pop(s0, "kwd", "block")
    return call(c, stack.push(s1, { ...b, scope: scope.new(k.value) }))
  }),
  mkPrim("__modules__", (c, s0) =>
    stack.push(s0, list([...modules.keys()].sort().map(kwd)))
  ),
  mkPrim("__module-get__", (c, s0) => {
    const [[k, m], s1] = stack.pop(s0, "kwd", "kwd")
    return stack.push(s1, moduleLookup(m.value, k.value))
  }),
  mkPrim("__module-defs__", (c, s0) => {
    const [[m], s1] = stack.pop(s0, "kwd")
    const l = [...getModule(m.value).keys()].sort().map(kwd)
    return stack.push(s1, list(l))
  }),
  mkPrim("__name__", (c, s) => stack.push(s, kwd(c.module))),
  mkPrim("__import__", (c, s0) => {
    const [[m], s1] = stack.pop(s0, "kwd")
    let imp = imports.get(c.module)
    if (!imp) { imp = []; imports.set(c.module, imp) }
    if (!imp.includes(m.value)) { imp.unshift(m.value) }
    return s1
  }),
  mkPrim("__import-from__", (c, s0) => {
    const [[ks, m], s1] = stack.pop(s0, "list", "kwd")
    for (const x of ks.value) { expect(x, "kwd") }
    for (const k of ks.value) {
      scope.define(c, k.value, moduleLookup(m.value, k.value))
    }
    return s1
  }),
  mkPrim("__load-module__", async (c, s0) => {
    const [[m], s1] = stack.pop(s0, "kwd")
    await loadMod(m.value)
    return s1
  }),
  mkPrim("__=__",     pop2push((x, y) => [bool( eq(x, y))   ])),
  mkPrim("__not=__",  pop2push((x, y) => [bool(!eq(x, y))   ])),
  mkPrim("__<__",     pop2push((x, y) => [cmp_lt (cmp(x, y))])),
  mkPrim("__<=__",    pop2push((x, y) => [cmp_lte(cmp(x, y))])),
  mkPrim("__>__",     pop2push((x, y) => [cmp_gt (cmp(x, y))])),
  mkPrim("__>=__",    pop2push((x, y) => [cmp_gte(cmp(x, y))])),
  mkPrim("__<=>__",   pop2push((x, y) => [int    (cmp(x, y))])),
  mkPrim("__eq__",    pop2push((x, y) => [cmp_eq (cmp(x, y, false))])),
  mkPrim("__neq__",   pop2push((x, y) => [cmp_neq(cmp(x, y, false))])),
  mkPrim("__lt__",    pop2push((x, y) => [cmp_lt (cmp(x, y, false))])),
  mkPrim("__lte__",   pop2push((x, y) => [cmp_lte(cmp(x, y, false))])),
  mkPrim("__gt__",    pop2push((x, y) => [cmp_gt (cmp(x, y, false))])),
  mkPrim("__gte__",   pop2push((x, y) => [cmp_gte(cmp(x, y, false))])),
  mkPrim("__cmp__",   pop2push((x, y) => [int    (cmp(x, y, false))])),
  mkPrim("__int+__"  , opI((x, y) => x + y)),
  mkPrim("__int-__"  , opI((x, y) => x - y)),
  mkPrim("__int*__"  , opI((x, y) => x * y)),
  mkPrim("__div__"   , opI((x, y) => {
    if (y == 0) { throw new KE(...E.DivideByZero()) }
    return mkInt(x / y)
  })),
  mkPrim("__mod__"   , opI((x, y) => x % y)),                 //  TODO
  mkPrim("__float+__", opF((x, y) => x + y)),
  mkPrim("__float-__", opF((x, y) => x - y)),
  mkPrim("__float*__", opF((x, y) => x * y)),
  mkPrim("__float/__", opF((x, y) => x / y)),
  mkPrimPP("__abs__",
    n => [n.type == "float" ? float(Math.abs(n.value)) :
          n.value < 0 ? int(-n.value) : n], "int or float"
  ),
  mkPrimPP("__neg__",
    n => [(n.type == "int" ? int : float)(-n.value)], "int or float"
  ),
  mkPrimPP("__trunc__", n => [f2i(Math.trunc(n.value))], "float"),
  mkPrimPP("__round__", n => [f2i(round     (n.value))], "float"),
  mkPrimPP("__ceil__" , n => [f2i(Math.ceil (n.value))], "float"),
  mkPrimPP("__floor__", n => [f2i(Math.floor(n.value))], "float"),
  mkPrimPP("__chr__", x => {
    if (x.value < 0 || x.value >= 0x110000) {
      throw new KE(...E.Expected(
        `int in range [0,0x110000) on stack (not ${show(x)})`
      ))
    }
    return [str(String.fromCodePoint(intToNum(x.value)))]
  }, "int"),
  mkPrimPP("__int->float__", n => [float(intToNum(n.value))], "int"),
  mkPrimPP("__record->dict__"     , x => [recordToDict(x)], "record"),
  mkPrimPP("__record-type__"      , x => [x.rectype]      , "record"),
  mkPrimPP("__record-values__"    , x => [list(x.values)] , "record"),
  mkPrimPP("__record-type-name__" , x => [kwd(x.name)]    , "record-type"),
  mkPrimPP("__record-type-fields__",
    x => [list(x.fields.map(kwd))], "record-type"
  ),
  mkPrim("__thunk__", (c, s0) => {
    const [[b], s1] = stack.pop(s0, "block")
    const f = () => call(c, stack.new(b))
    return stack.push(s1, thunk(f))
  }),
  mkPrim("__fail__", (c, s0) => {
    const [[msg], s1] = stack.pop(s0, "str")
    throw new KE(...E.Fail(strVal(msg)))
  }),
  mkPrim("__try__", async (c, s0) => {
    const info = e => [kwd(e.type), str(e.message), list(e.info.map(str))]
    let s2, err = null
    const [[f, g, h], s1] = stack.pop(s0, "block", "block or nil", "block")
    try {
      s2 = await call(c, stack.new(f))
    } catch(e) {
      if (e instanceof KonekoError) {
        err = e
      } else {
        throw e
      }
    }
    const [[b], s3] = !err ? [[F], s2] : isNil(g) ? [[F], []] :
      stack.pop(await call(c, stack.new(...info(err), g)), "_")
    const s4 = stack.push(s1, ...s3, ...(await call(c, stack.new(h))))
    if (err && !truthy(b)) { throw err }
    return s4
  }),
  mkPrimPP("__ident__", x => [ident(x.value)], "kwd"),
  mkPrimPP("__quot__" , x => [quot (x.value)], "kwd"),
  mkPrimPP("__block__", (ps, code, b) => {
    for (const x of ps.value) { expect(x, "kwd") }
    const params = ps.value.map(x => chkIdent(x.value))
    return [block(params, code.value, b.scope)]
  }, "list", "list", "block"),
  mkPrimPP("__block-params__", x => [list(x.params.map(kwd))], "block"),
  mkPrimPP("__block-code__"  , x => [list(x.code)]           , "block"),
  mkPrimPP("__rx-match__", (s, r) => {
    const m = mkRx(strVal(r)).exec(strVal(s))
    return [m ? list(m.map(str)) : nil]
  }, "str", "str"),
  mkPrim("__rx-sub__", async (c, s0) => {
    let t
    const [[s, s_b, r, g], s1] =
      stack.pop(s0, "str", "str or block", "str", "bool")
    const r_ = mkRx(strVal(r), g.value ? "g" : ""), s_ = strVal(s)
    if (s_b.type == "str") {
      t = str(s_.replace(r_, strVal(s_b)))
    } else {
      let m, i = 0, ts = []
      while (m = r_.exec(s_)) {
        const vs = await call(c, stack.new(...m.map(str), s_b))
        if (vs.length != 1) {
          throw new KE(...E.Expected(
            "rx-sub block to produce exactly 1 value"
          ))
        }
        expect(vs[0], "str")
        ts.push(s_.slice(i, m.index), strVal(vs[0]))
        i = m.index + m[0].length
        if (!g.value) { break }
      }
      t = ts.length ? str(ts.join("") + s_.slice(i)) : s
    }
    return stack.push(s1, t)
  }),
  mkPrim("__par__", async (c0, s0) => {
    const [[f, g], s1] = stack.pop(s0, "block", "block")      //  TODO
    const cf = { ...c0, _meta: {} }, cg = { ...c0, _meta: {} }
    const p = async (h, c) => await call(c, stack.new(h))
    try {
      const [l1, l2] = await Promise.all([p(f, cf), p(g, cg)])
      return stack.push(s1, ...l1, ...l2)
    } catch(e) {
      cf._meta.abort = cg._meta.abort = true
      throw e
    }
  }),
  mkPrim("__sleep__", async (c, s0) => {
    const [[n], s1] = stack.pop(s0, "int or float")
    const ms = Math.round(1000 * Number(n.value))
    await new Promise((resolve, _) => setTimeout(resolve, ms))
    return s1
  }),
  mkPrim("__version__", async (c, s) => {
    const v = list((await koneko_version()).map(int))
    const t = kwd("js-" + (_req ? "node" : "browser"))
    const p = list(platform().map(str))
    return stack.push(s, list([v, t, p]))
  }),
  mkPrim("__show-stack!__", async (c, s) => {
    puts("--- STACK ---\n")
    for (const x of stack.toArray(s)) { puts(await prld_show(x) + "\n") }
    puts("---  END  ---\n")
    return s
  }),
  mkPrim("__clear-stack!__", (c, s) => {
    puts("*** STACK CLEARED ***\n")
    return stack.new()
  }),
  mkPrim("__nya!__", (c, s) => nya().then(() => s)),
]))                                                           //  }}}1

const getModule = m => {
  if (!modules.has(m)) { throw new KE(...E.ModuleNameError(m)) }
  return modules.get(m)
}

const moduleLookup = (m, k) => {
  const mod = getModule(m)
  if (!mod.has(k)) { throw new KE(...E.NameError(k)) }
  return mod.get(k)
}

const types = [
  "nil", "bool", "int", "float", "str", "kwd", "pair", "list", "dict",
  "ident", "quot", "block", "builtin", "multi", "record-type",
  "record", "thunk"
]

const functionTypes = ["block", "builtin", "multi", "record-type"]
const callableTypes =
  ["str", "pair", "list", "dict", "record", "thunk"].concat(functionTypes)

/* === builtins === */

const unprim = ([k, v]) => { v.prim = false; return [k, v] }

modules.set("__bltn__", new Map([                             //  {{{1
  mkPrimPP("str->int"  , x => [maybeJ(int  , pInt  (strVal(x)))], "str"),
  mkPrimPP("str->float", x => [maybeJ(float, pFloat(strVal(x)))], "str"),
  ...types.map(t => mkPrimPP(t+"?", x => [bool(x.type == t)], "_")),

  /* slightly faster (?!) versions of prelude functions */
  mkPrimPP("dup" , x => [x, x], "_"),
  mkPrimPP("drop", x => [], "_"),
  mkPrim("swap", modules.get("__prim__").get("__swap__").run),
  mkPrim("dip", async (c, s0) => {
    const [[x, f], s1] = stack.popN(s0, 2)
    const s2 = await call(c, stack.push(s1, f))
    return stack.push(s2, x)
  }),

  /* nicer (!) versions of prelude functions */
  mkPrimPP("$", (x, f) => [partial(x, f)], "_", "_"),
  mkPrimPP("@", (f, g) => [compose(f, g)], "_", "_"),
  mkPrimPP("%", (g, f) => [compose(f, g)], "_", "_"),

  /* noticably faster (!) versions of prelude functions */
  mkPrim("~?", async (c, s0) => {
    const [[x, f, g, p], s1] = stack.popN(s0, 4)
    const s2 = await call(c, stack.push(s1, x, x, p))
    const [[b], s3] = stack.popN(s2, 1)
    return call(c, stack.push(s3, truthy(b) ? f : g))
  }),
  mkPrim("~nil", (c, s0) => {
    const [[x, f, g], s1] = stack.popN(s0, 3)
    return call(c, stack.push(s1, ...(isNil(x) ? [f] : [x, g])))
  }),
].map(unprim)))                                               //  }}}1

const compose = (f, g) => block([], ["f", "g"].map(ident),
  { module: null, table: new Map([["f", f], ["g", g]]) }, "@")

const partial = (x, f) => block([], [quot("x"), ident("f")],
  { module: null, table: new Map([["x", x], ["f", f]]) }, "$")

/* === io === */

// TODO
// NB: node.js only
if (_req) {
  modules.set("io", new Map([                                 //  {{{1
    mkPrimPP("contents!", async s => {
      try {
        const contents = await readFile(strVal(s))
        return [str(contents)]
      } catch (e) {
        if (e instanceof Error) {
          throw new KE(...E.Fail(`io.contents!: ${e.message}`)) // TODO
        } else {
          throw e
        }
      }
    }, "str"),
    mkPrimPP("lines!", async s => {
      try {
        const lines = await fileLines(strVal(s))
        return [list(lines.map(([i, l]) => str(l)))]
      } catch (e) {
        if (e instanceof Error) {
          throw new KE(...E.Fail(`io.lines!: ${e.message}`))  //  TODO
        } else {
          throw e
        }
      }
    }, "str"),
  ].map(unprim)))                                             //  }}}1
}

/* === json === */

// NB: converting to JSON turns
// * kwds into strs
// * pairs into lists
// * records into dicts

modules.set("json", new Map([                                 //  {{{1
  mkPrimPP("->", s => {
    let x
    try {
      x = JSON.parse(strVal(s))
    } catch (e) {
      if (e instanceof SyntaxError) {
        throw new KE(...E.Fail(`json.->: ${e.message}`))      //  TODO
      } else {
        throw e
      }
    }
    return [fromJS(x, { useObj: true })]
  }, "str"),
  mkPrimPP("<-", x => {
    let y
    try {
      y = toJS(x, { useObj: true })
    } catch(e) {
      if (e instanceof Error && e.message.startsWith("toJS: ")) {
        const msg = e.message.slice(6)
        throw new KE(...E.Fail(`json.<-: ${msg}`))            //  TODO
      } else {
        throw e
      }
    }
    const replace = (k, v) => typeof v === "bigint" ? Number(v) : v
    return [str(JSON.stringify(y, replace))]
  }, "_"),
].map(unprim)))                                               //  }}}1

/* === math === */

const mkPF1 = (name, op) => mkPrim(name, opF1(op))

modules.set("math", new Map([                                 //  {{{1
  mkPrimPP("sign",
    n => [n.type == "int" ? int((n.value > 0) - (n.value < 0)) :
          float(Math.sign(n.value))], "int or float"
  ),
  mkPrim("^",     opI((x, y) => {
    if (y < 0) { throw new KE(...E.RangeError("negative exponent")) }
    return x ** y
  })),
  mkPrim("**",    opF(Math.pow)),
  mkPrimPP("pi",  () => [float(Math.PI)]),
  mkPF1("exp",    Math.exp),
  mkPF1("log",    Math.log),
  mkPF1("sqrt",   Math.sqrt),
  mkPF1("sin",    Math.sin),
  mkPF1("cos",    Math.cos),
  mkPF1("tan",    Math.tan),
  mkPF1("asin",   Math.asin),
  mkPF1("acos",   Math.acos),
  mkPF1("atan",   Math.atan),
  mkPF1("sinh",   Math.sinh),
  mkPF1("cosh",   Math.cosh),
  mkPF1("tanh",   Math.tanh),
  mkPF1("asinh",  Math.asinh),
  mkPF1("acosh",  Math.acosh),
  mkPF1("atanh",  Math.atanh),
  mkPrim("atan2", opF(Math.atan2)),
].map(unprim)))                                               //  }}}1

/* === event loop === */

let _last_tick = Date.now()
const tick = p => {
  const t = Date.now()
  if (t - _last_tick < 100) { return p }                      //  TODO
  _last_tick = t
  return new Promise((...rr) => setTimeout(() => p.then(...rr), 0))
}

/* === miscellaneous === */

// TODO
const round = f => {
  const i = Math.round(f)
  return i % 2 && f + 0.5 == i ? i - 1 : i
}

// TODO
const f2i = f => Number.isFinite(f) ? int(f) : nil

// TODO
const zip = (xs, ys) => xs.map((x, i) => [x, ys[i]])

const mkRx = (s, flags = "") => {                             //  {{{1
  let rx
  try {
    rx = Rx(s, "u" + flags)
  } catch(e) {
    if (e instanceof SyntaxError) {
      throw new KE(...E.InvalidRx(e.message))
    } else {
      throw e
    }
  }
  return rx
}                                                             //  }}}1

/* === files === */

const _path   = _req ? _req("path") : null
const join    = (...xs) => _req ? _path.join(...xs) : xs.join("/")
const baseDir = _req && module.id != "[stdin]" ?
                join(_path.dirname(module.filename), "..") : null
const bpath   = (...xs) => join(baseDir, ...xs)

// NB: browser only; Promise
const requestFile = fname => fetch(fname).then(r => {
  if (!r.ok) {
    throw new Error(`response (${r.status} ${r.statusText}) was not ok`)
  }
  return r.text()
})

// NB: node.js only; Promise
const readFile = fname => new Promise((resolve, reject) => {
  _req("fs").readFile(fname, "utf8", (err, data) => {
    if (err) { reject(err) }
    resolve(data)
  })
})

// TODO
// NB: node.js only; Promise
const fileLines = fname => readFile(fname).then(
  text => text.replace(/\n$/, "").split("\n").map((line, i) => [i+1, line])
)

// NB: Promise
const evalFile = (fname, c = undefined, s = undefined) => {
  return (_req ? readFile : requestFile)(fname).then(
    text => evalText(text, c, s, fname)
  )
}

// NB: Promise
const loadMod = async name => {                               //  {{{1
  if (typeof __koneko_modules__ !== "undefined" &&
      __koneko_modules__.has(name)) {
    const s = __koneko_modules__.get(name)
    return evalText(s, undefined, undefined, `(module:${name})`)
  }
  if (!_req) {
    return await evalFile(join("lib", `${name}.knk`)).catch(e => {
      if (e instanceof KonekoError) {
        throw e
      } else {
        console.error(e)
        throw new KE(...E.ModuleLoadError(name))
      }
    })
  }
  const fs = _req("fs"), d = _path.delimiter
  const ps = (process.env.KONEKOPATH || "").split(d).filter(x => x)
  const xs = baseDir ? [bpath("lib"), ...ps] : ps
  for (const x of xs.map(x => join(x, `${name}.knk`))) {
    if (fs.existsSync(x)) { return await evalFile(x) }
  }
  throw new KE(...E.ModuleLoadError(name))
}                                                             //  }}}1

// NB: Promise
const loadPrelude = () => {
  if (!modules.get("__prld__").size) { return loadMod("prelude") }
}

/* === version information === */

let _version  = typeof __koneko_version__ !== "undefined" ?
                __koneko_version__.split(".") : null

// NB: Promise
const koneko_version = async () => {
  if (!_version) {
    const pj = "package.json"
    _version = (_req ? _req(bpath(pj)) :
      JSON.parse(await requestFile(pj))).version.split(".")
  }
  return _version
}

const platform = () => {
  if (_req) {
    const p = process
    return [`${p.platform} ${p.arch}`, `${p.release.name} ${p.version}`]
  } else {
    const n = navigator
    return [n.platform.toLowerCase(), n.userAgent]
  }
}

/* === output === */

// NB: node.js only
const putOut = s => process.stdout.write(s)
const putErr = s => process.stderr.write(s)

/* === input === */

// NB: node.js only
const _readline = _req ? _req("readline") : null
const _rl_buf   = []                                          //  TODO

// NB: node.js only; Promise
const read_line = (prompt = null) =>                          //  {{{1
  new Promise((resolve, reject) => {
    const p = prompt || "", i = process.stdin, o = process.stdout
    if (_rl_buf.length) {
      if (p) { o.write(p) }                                   //  TODO
      resolve(_rl_buf.shift())
      return
    }
    const rl = _readline.createInterface({
      input: i, output: o, prompt: p, terminal: false
    })
    let done = false
    const f = (g, h = null) => {
      if (!done) { done = true; rl.close(); g() }
      else if (h) { h() }
    }
    rl.on("line"  , line => f(() => resolve(line),
                              () => _rl_buf.push(line)))
    rl.on("close" , ()   => f(() => resolve(null)))
    rl.on("error" , e    => f(() => reject(e)))               //  TODO
    rl.prompt()
  })                                                          //  }}}1

/* === repl === */

// NB: node.js only

const repl_init = (c, w_show = true) => {
  const alias = (xs, y = `__${xs[0]}__`) =>
    xs.forEach(x => scope.define(c, x, scope.lookup(c, y)))
  if (w_show) { alias(["show-stack!", "s!"]) }
  alias(["clear-stack!", "c!"])
  alias(["d!"], "display!"); alias(["D!"], "ddisplay!")
  scope.define(c, "__repl__", T)
}

// NB: Promise
const repl_process_line =                                     //  {{{1
  async (line, c, s, stdout, stderr) => {
    if (!line) { return s }
    try {
      s = await evalText(line, c, s, "(repl)")
      if (!stack.null(s) && !",;".includes(line[0])) {
        stdout.write(await prld_show(stack.top(s)) + "\n")    //  TODO
      }
    } catch(e) {
      if (e instanceof KonekoError) {
        stderr.write("*** ERROR: " + e.message + "\n")
      } else {
        throw e
      }
    }
    return s
  }                                                           //  }}}1

// NB: Promise
const repl = async (verbose = false, c = scope.new()) => {    //  {{{1
  if (!process.stdin.isTTY && process.platform != "win32") {
    return evalFile("/dev/stdin")                             //  TODO
  }
  let s = stack.new(); repl_init(c)
  if (verbose) { scope.define(c, "__debug__", T) }
  const f = async () => {
    const line = await read_line(">>> ")
    if (line == null) { putOut("\n"); return }
    s = await repl_process_line(line, c, s, process.stdout, process.stderr)
    await f()
  }
  await f()
}                                                             //  }}}1

/* === doctest === */

// NB: node.js only

// NB: Promise
const doctest = (files, verbose = false, cov = false) =>
  testFiles(files, verbose, cov).then(([t,o,fail]) => fail == 0)

// NB: Promise
const testFiles = async (files, vbs = false, cov = false) => { // {{{1
  let total = 0, ok = 0, fail = 0
  const nl = fname => fileLines(fname).then(lines => [fname, lines])
  const fs = await Promise.all(files.map(nl))
  if (cov) { coverageMonkeyPatch() }
  for (const [fname, lines] of fs) {
    const md      = /\.md$/u.test(fname)
    const test    = md ? testMarkdownFile : testKonekoFile
    putOut(`=== Testing ${fname} (${md ? "markdown" : "koneko"}) ===\n`)
    const [t,o,f] = await test(fname, lines, vbs)
    total += t; ok += o; fail += f
    putOut("\n")
  }
  putOut("=== Summary ===\n")
  putOut(`Files: ${files.length}.\n`)
  printTestSummary(total, ok, fail)
  if (cov) { coverageReport() }
  return [total, ok, fail]
}                                                             //  }}}1

const coverageMonkeyPatch = () => {                           //  {{{1
  const patch = m => {
    if (m._get) { return }
    m._get = m.get
    m.get = k => {
      const r = m._get(k)
      if (r) { if (!r._cov) { r._cov = 0 }; ++r._cov }
      return r
    }
  }
  for (const m of modules.values()) { patch(m) }
  modules._set = modules.set
  modules.set = (k, v) => { patch(v); modules._set(k, v) }
}                                                             //  }}}1

// TODO: select which modules w/ option
const coverageReport = () => {                                //  {{{1
  const blacklist = ["__main__", "_test", "foo", "no-such-module"]
  putOut("\n=== Coverage ===\n")
  for (const [mn, mo] of [...modules.entries()].sort()) {
    if (blacklist.includes(mn)) { continue }
    let uncov = [...mo.entries()].sort()
      .filter(([k, v]) => !v._cov).map(([k, v]) => k)
    if (mn == "__prld__") {
      uncov = uncov.filter(k => !modules.get("__bltn__").has(k))
    }
    const n = mo.size - uncov.length, p = Math.round(n / mo.size * 100)
    putOut(`Module: ${mn} (${n}/${mo.size}, ${p}%)\n`)
    if (uncov.length) { putOut(`Uncovered: ${uncov.join(" ")}.\n`) }
    putOut("\n")
  }
}                                                             //  }}}1

const blocksToExamples = (fname, blocks) =>
  blocks.map(ls => exampleGroup(fname, ls)).filter(es => es.length)

const exampleGroup = (fname, lines) => {                      //  {{{1
  const es = []
  let m, e, in_e = false, prefix
  for (const [lineno, l] of lines) {
    if (m = /^(\s*)>>> (.*)$/u.exec(l)) {
      if (in_e) { es.push(e) } else { in_e = true }
      prefix  = m[1]
      e       = { fname, lineno, input: m[2], output: [] }
    } else if (in_e) {
      if (!l.startsWith(prefix) || l.length <= prefix.length) {
        es.push(e); in_e = false
      } else if ((m = /^(\s*)\.\.\. (.*)$/u.exec(l)) && m[1] == prefix) {
        e.input += m[2]
      } else {
        e.output.push(l.slice(prefix.length))
      }
    }
  }
  if (in_e) { es.push(e) }
  return es
}                                                             //  }}}1

const knkCommentBlocks = lines => {                           //  {{{1
  const bs = []
  let m, b, in_b = false, prefix
  for (const [ln, l] of lines) {
    if (m = /^(\s*;)(.*)$/u.exec(l)) {
      if (!in_b) {
        b = []; in_b = true; prefix = m[1]
      } else if (prefix != m[1]) {
        bs.push(b); b = []; prefix = m[1]
      }
      b.push([ln, m[2]])
    } else if (in_b) {
      bs.push(b); in_b = false
    }
  }
  if (in_b) { bs.push(b) }
  return bs
}                                                             //  }}}1

const mdCodeBlocks = lines => {                               //  {{{1
  const bs = []
  let b, in_b = false
  for (const [ln, l] of lines) {
    if (!in_b && Rx("^```koneko$").test(l)) {
      b = []; in_b = true
    } else if (in_b) {
      if (Rx("^```$").test(l)) {
        bs.push(b); in_b = false
      } else {
        b.push([ln, l])
      }
    }
  }
  return bs
}                                                             //  }}}1

// NB: Promise
const testExamples = async (es, verbose = false) => {         //  {{{1
  let total = 0, ok = 0, fail = 0
  for (const g of es) {
    const [t,o,f] = await testExampleGroup(g, verbose)
    total += t; ok += o; fail += f
  }
  if (verbose) {
    putOut("=== Summary ===\n")
    printTestSummary(total, ok, fail)
  }
  return [total, ok, fail]
}                                                             //  }}}1

// NB: Promise
const testExampleGroup = async (g, verbose = false) => {      //  {{{1
  const wr = l => ({ write: s => l.push(s) })
  defMain()                                                   //  TODO
  const c = scope.new(); let s = stack.new(); repl_init(c)
  let ok = 0, fail = 0
  for (const e of g) {
    const out = [], err = []
    overrides.puts = s => out.push(s)                         //  TODO
    s = await repl_process_line(e.input, c, s, wr(out), wr(err))
    if (compareExampleOutput(e.output, out, err)) {
      ok +=1
      if (verbose) { printSucc(e) }
    } else {
      fail +=1
      printFail(e, out, err)
      break
    }
  }
  if (verbose) { printTTPF(g.length, ok, fail) }
  return [g.length, ok, fail]
}                                                             //  }}}1

// NB: Promise
const _testFile = f => (fname, lines, verbose = false) =>
  testExamples(blocksToExamples(fname, f(lines)), verbose)
const testKonekoFile    = _testFile(knkCommentBlocks)
const testMarkdownFile  = _testFile(mdCodeBlocks)

const compareExampleOutput = (exp, got, err) => {
  // NB: can't == arrays :(
  const exp_ = exp.map(l => l == "<BLANKLINE>" ? "\n" : `${l}\n`).join("")
  const got_ = got.join(""), err_ = err.join("")
  return !err_.length ? exp_ == got_ : !got_.length && exp_ == err_ &&
    /^\*\*\* ERROR: /u.test(err_)
}

const printTestSummary = (total, ok, fail) => {
  printTTPF(total, ok, fail)
  putOut(`Test ${fail == 0 ? "passed" : "failed"}.\n`)
}

const printTTPF = (total, ok, fail) =>
  putOut(`Total: ${total}, Tried: ${ok + fail}, Passed: ${ok}, Failed: ${fail}.\n`)

const printSucc = ex => {
  putOut(`Trying:\n  ${ex.input}\n`)
  putOut("Expecting:\n")
  for (const l of ex.output) { putOut(`  ${l}\n`) }
  putOut("ok\n")
}

const printFail = (ex, out, err) => {                         //  {{{1
  const out_ = out.join(""), err_ = err.join("")
  putErr(`File ${ex.fname}, line ${ex.lineno}\n`)
  putErr(`Failed example:\n  ${ex.input}\n`)
  putErr("Expected:\n")
  for (const l of ex.output) { putErr(`  ${l}\n`) }
  putErr("Got:\n")
  if (out_.length) { putErr(out_.split("\n").map(l => `  ${l}`).join("\n")) }
  if (err.length) {
    putErr("Errors:\n")
    if (err_.length) { putErr(err_.split("\n").map(l => `  ${l}`).join("\n")) }
  }
}                                                             //  }}}1

/* === main === */

// NB: node.js only

// NB: Promise
const main = () => {                                          //  {{{1
  process.on("unhandledRejection" , e => {})
  process.on("rejectionHandled"   , e => {})
  main_().catch(e => {
    if (e instanceof KonekoError || e.name == "Error") {
      putErr(`koneko: ${e.message}\n`)
    } else {
      console.error("koneko:", e)
    }
    process.exitCode = 1
  })
}                                                             //  }}}1

// TODO: fix args; --eval, --interactive, ...
// NB: Promise
const main_ = async () => {                                   //  {{{1
  await loadPrelude()
  const argv    = process.argv.slice(2)
  const opts    = argv.filter(a =>  a.startsWith("-"))        //  TODO
  const args    = argv.filter(a => !a.startsWith("-"))
  const verbose = opts.includes("-v") || opts.includes("--verbose")
  if (opts.includes("--version")) {
    const version = await koneko_version()
    putOut(`koneko 「子猫」 ${version.join(".")}\n`)
  } else if (opts.includes("--doctest")) {
    const cov = opts.includes("--coverage")
    if (!(await doctest(args, verbose, cov))) { process.exitCode = 1 }
  } else if (args.length) {
    const c = scope.new()
    scope.define(c, "__args__", list(args.slice(1).map(str)))
    await evalFile(args[0], c)
    if (opts.includes("-i") || opts.includes("--interactive")) {
      await repl(verbose, c)
    }
  } else {
    await repl(verbose)
  }
}                                                             //  }}}1

/* === exports & overrides === */

// NB: Promise
const prld_show = async x => {
  const p = modules.get("__prld__").get("show")
  const s = await call(scope.new(), stack.new(x, p))
  const [[y], _] = stack.pop(s, "str")
  return strVal(y)
}

// FIXME
const puts = s => (overrides.puts || putOut)(s)

// NB: Promise
const ask = async s => await (overrides.ask || read_line)(s)

// NB: Promise
const nya = async () => {                                     //  {{{1
  if (overrides.nya) {
    return await overrides.nya()
  } else if (baseDir) {
    return readFile(bpath("nya", "tabby.cat"), "utf8").then(putOut)
  } else {
    throw new KE(...E.NotImplemented("__nya__"))
  }
}                                                             //  }}}1

const overrides = {}

_mod[_exp] = {
  KonekoError, read, evaluate, evalText, show, toJS, fromJS,
  loadPrelude, overrides, repl_init, prld_show,
  get_version: koneko_version,
  initContext: scope.new, emptyStack: stack.new,
  ...(_req ? { repl, doctest, main } : {})
}

// NB: node.js only
if (_req && (_mod === _req.main ||
             typeof __koneko_modules__ !== "undefined")) { main() }

})(
  ...(typeof module === "undefined" ?
    [this  , "koneko" , null   , XRegExp] : // browser
    [module, "exports", require,  RegExp])  // node.js
)

// vim: set tw=70 sw=2 sts=2 et fdm=marker :
