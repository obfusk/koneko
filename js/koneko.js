//  --                                                          ; {{{1
//
//  File        : koneko.js
//  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
//  Date        : 2019-11-22
//
//  Copyright   : Copyright (C) 2019  Felix C. Stegerman
//  Version     : v0.0.1
//  License     : GPLv3+
//
//  --                                                          ; }}}1

"use strict";
((_mod, _exp, _req, Rx) => {

/* === error, stack, scope === */

class KonekoError extends Error {
  constructor(message, type, info = {}) {
    super(message)
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, KonekoError)
    }
    this.name = "KonekoError"
    this.type = type
    for (const k in info) { this["info_"+k] = info[k] }
  }
}

const KE = KonekoError

const E = {                                                   //  {{{1
  ParseError:           msg           => [`parse error: ${msg}`, { msg }],
  EvalUnexpected:       type          => [`cannot eval: ${type}`, { type }],
  EvalScopelessBlock:   ()            => ["cannot eval scopeless block", {}],
  ModuleNotFound:       name          => [`no module named ${name}`, { name }],
  LookupFailed:         name          => [`name ${name} is not defined`, { name }],
  StackUnderflow:       ()            => ["stack underflow", {}],
  Expected:             msg           => [`expected ${msg}`, { msg }],
  MultiMatchFailed:     (name, sig)   => [`no signature ${sig} for multi ${name}`, {name, sig }],
  UncomparableType:     type          => [`type ${type} is not comparable`, { type }],
  UncallableType:       type          => [`type ${type} is not callable`, { type }],
  UnapplicableType:     (type, op)    => [`type ${type} does not support ${op}`, { type, op }],
  UnknownField:         (field, type) => [`${type} has no field named ${field}`, { field, type }],
  EmptyList:            op            => [`${op}: empty list`, { op }],
  IndexError:           (op, index)   => [`${op}: index ${index} is out of range`, { op, index }],
  KeyError:             (op, key)     => [`${op}: key ${key} not found`, { op, key }],
  DivideByZero:         ()            => ["divide by zero", {}],
  NotImplementedError:  what          => [`not implemented: ${what}`, { what }],
}                                                             //  }}}1

for (const k in E) {
  const f = E[k]
  E[k] = (...args) => {
    const [msg, info] = f(...args)
    return [msg, k, info]
  }
}

const expect = (x, t, msg = `${t} on stack`) => {
  if (x.type != t) { throw new KE(...E.Expected(msg)) }
  return x
}

const nilToDef = (x, d, t) => x.type == "nil" ? d : expect(x, t).value

// TODO: inefficient implementation
const stack = {                                               //  {{{1
  empty: () => [],
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
  popN: (s0, n) => stack.pop(s0, ...Array(n).fill("_")),
  toArray: s => [...s].reverse(),
}                                                             //  }}}1

// TODO: inefficient implementation
const scope = {                                               //  {{{1
  new: (module = "__main__") =>
    ({ module, parent: null, table: new Map() }),
  fork: (c, table) => ({ module: c.module, parent: c, table }),
  define: (c, k, v) => { modules.get(c.module).set(k, v) },
  lookup: (c, k, d = undefined) => {
    if (modules.get("__prim__").has(k)) {
      return modules.get("__prim__").get(k)
    }
    let c_ = c
    while (c_) {
      if (c_.table.has(k)) { return c_.table.get(k) }
      c_ = c_.parent
    }
    for (const m of [c.module, "__bltn__", "__prld__"]) {
      if (modules.get(m).has(k)) { return modules.get(m).get(k) }
    }
    if (d !== undefined) { return d }
    throw new KE(...E.LookupFailed(k))
  },
}                                                             //  }}}1

/* === values === */

const _tv   = (type, value) => ({ type, value })

const nil   = { type: "nil" }
const bool  = b => _tv("bool", !!b)
const int   = i => _tv("int", i)
const float = f => _tv("float", f)
const str   = s => str_([...s])
const str_  = s => ({ type: "str", list: s })
const kwd   = s => _tv("kwd", s)
const pair  = (key, value) => ({ type: "pair", key, value })
const list  = l => _tv("list", l)
const dict  = ps => _tv("dict", new Map(ps.map(p => [p.key.value, p.value])))
const ident = i => _tv("ident", i)
const quot  = i => _tv("quot", i)

const block = (params, code, scope = null) =>
  ({ type: "block", params, code, scope })
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

const T = bool(true), F = bool(false)

const mkBltn    = (name, f) => [name, builtin(name, f)]
const mkBltnPP  = (name, f, ...xs) => mkBltn(name, popPush(f, ...xs))

const builtinPP = (name, g = x => x) => (f, ...xs) =>
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

const digitParams = b => {
  let n = 0
  for (const k of freeVars(b.code)) {
    let m
    if (m = /^__([1-9])__$/u.exec(k)) {
      n = Math.max(n, parseInt(m[1], 10))
    }
  }
  return Array.from(Array(n), (_, i) => "__" + (i+1).toString() + "__")
}

const truthy = v =>
  !(v.type == "nil" || (v.type == "bool" && v.value == false))

const dictToList = d => list([...d.value.keys()].sort().map(
  k => pair(kwd(k), d.value.get(k))
))

/* === parsing === */

const isIdent = s => Rx("^"+_naiveIdent+"$", "u").exec(s) &&
  ![..."'!:"   ].includes(s[0]) &&
  ![...":({["  ].includes(s.slice(-1)) &&
  ![..."(){}[]"].includes(s) &&
  s != "()" && s != "nil" && !isInt(s) && !isFloat(s)

const isInt   = s => /^(?:-?\d+|0x[0-9a-fA-F]+|0b[01]+)$/u.test(s)
const isFloat = s => /^-?\d+(?:\.\d+e\d+|\.\d+|e\d+)$/u.test(s)
const isPrint = s =>
  Rx("^[\\p{L}\\p{M}\\p{N}\\p{P}\\p{S}\\p{Zs}]+$", "u").test(s)

const _naiveIdent = "[\\p{L}\\p{N}\\p{S}\\(\\)\\{\\}\\[\\]@%&*\\-_\\/?'!:]+"
const _space      = "(?:[\\s,]|;[^\\n]*(?:\\n|$))+"

const parseOne = (s, p0 = 0, end = null) => {                 //  {{{1
  let m, p1
  const t = pat => {
    const r = new Rx("("+pat+")(?:"+sp+"|$)", "usy")
    r.lastIndex = p0; m = r.exec(s); p1 = r.lastIndex
    return !!m
  }
  const sp  = _space, hd = "[0-9a-fA-F]"
  const hex = '(\\\\x'+hd+'{2})|(\\\\u'+hd+'{4})|(\\\\U'+hd+'{8})'
  const chr = hex + '|(\\\\[rnt\\\\"])|([^"])'
  const esc = { "\\r":"\r", "\\n":"\n", "\\t":"\t", "\\\"":"\"", "\\\\":"\\" }
  const escapedStrBody = k => {
    const l = [], r = Rx(chr, "usy"); r.lastIndex = p0 + k
    const f = w => String.fromCodePoint(parseInt(cm[0].slice(w), 16))
    let cm
    while (cm = r.exec(s)) {
      l.push(cm[1] || cm[2] || cm[3] ? f(2) : cm[4] ? esc[cm[4]] : cm[5])
    }
    return l
  }
  // TODO: only match actual idents
  const parseBlock = (pre = "") => {
    let params = [], p2 = p1
    if (t(pre+"\\["+sp+"((?:"+_naiveIdent+sp+")*?)"+"([.\\]])")) {
      if (m[3] == ".") {
        params = m[2].split(Rx(sp, "us")).filter(x => x); p2 = p1
      }
    }
    const [p3, vs] = parseMany(s, p2, "]")
    return [p3, block(params, vs)]
  }

  // primitives
  if (t("nil")) {
    return [p1, nil ]
  } else if (t("#t") || t("#f")) {
    return [p1, bool(m[1] == "#t") ]
  } else if (t("-?\\d+")) {
    return [p1, int(parseInt(m[1], 10))]
  } else if (t("(0x)(" + hd + "+)") || t("(0b)([01]+)")) {
    return [p1, int(parseInt(m[3], m[2] == "0x" ? 16 : 2))]
  } else if (t("-?\\d+(?:\\.\\d+e\\d+|\\.\\d+|e\\d+)")) {
    return [p1, float(parseFloat(m[1]))]
  } else if (t('"(' + chr + '*)"')) {
    return [p1, str_(escapedStrBody(1))]
  } else if (t(":(" + _naiveIdent + ")") && isIdent(m[2])) {
    return [p1, kwd(m[2])]
  } else if (t(':"(' + chr + '*)"')) {
    return [p1, kwd(escapedStrBody(2).join(""))]
  }
  // values
  else if (t("\\(\\)")) {
    return [p1, list([])]
  } else if (t("\\(")) {
    const [p2, vs] = parseMany(s, p1, ")")
    return [p2, list(vs)]
  } else if (t(_naiveIdent) && isIdent(m[1])) {
    return [p1, ident(m[1])]
  } else if (t("'(" + _naiveIdent + ")") && isIdent(m[2])) {
    return [p1, quot(m[2])]
  } else if (t("\\[")) {
    return parseBlock()
  }
  // sugar
  else if (t("\\.{3}")) {
    return [p1, ident("__ellipsis__")]
  } else if (t("([.!])\\[")) {
    const bang = m[2] == "!"
    const [p2, b] = parseBlock("[.!]")
    const more = bang ? [ident("__call__")]: []
    return [p2, block(digitParams(b), [b]), ...more]
  } else if (t("['.]([1-9])")) {
    return [p1, (m[1][0] == "'" ? quot : ident)("__"+m[2]+"__")]
  } else if (t("('?)([.!])(" + _naiveIdent + ")") && isIdent(m[4])) {
    const sw = ident("__swap__"), ca = ident("__call__")
    const vs = [kwd(m[4]), sw, ca, ...(m[3] == "!" ? [ca]: [])]
    return [p1, ...(m[2] ? [block([], vs)] : vs)]
  } else if (t("\\{")) {
    const [p2, vs] = parseMany(s, p1, "}")
    return [p2, list(vs), ident("__dict__")]
  } else if (t("(" + _naiveIdent + "):") && isIdent(m[2])) {
    const [p2, ...v] = parseOne(s, p1)
    return [p2, kwd(m[2]), ...v, ident("__=>__")]
  } else if (t("(" + _naiveIdent + ")([({])") && isIdent(m[2])) {
    const curly = m[3] == "{"
    const [p2, vs] = parseMany(s, p1, curly ? "}" : ")")
    const l = list(vs), q = quot(m[2])
    if (curly) {
      return [p2, l, q, ident("__apply__")]
    } else {
      return [p2, l, ident("__dict__"), q, ident("__apply-dict__")]
    }
  }
  // end of nested
  else if (t("[)}\\]]") && end == m[1]) {
    return [p1, { end }]
  }
  throw new KE(...E.ParseError(`no match at pos ${p0}`))
}                                                             //  }}}1

const parseMany = (s, p, end = null) => {
  const vs = []; let v
  while (p < s.length) {
    [p, ...v] = parseOne(s, p, end)
    if (end && v[0].end) { return [p, vs] }
    vs.push(...v)
  }
  if (end) {
    throw new KE(...E.ParseError(`expected "${end}" at pos ${p}`))
  }
  return [p, vs]
}

const read = s => {
  let p = 0, m
  if (m = /^#!.*\n/u.exec(s)) { p = m[0].length }
  const r = Rx(_space, "usy"); r.lastIndex = p
  if (m = r.exec(s)) { p += m[0].length }
  return parseMany(s, p)[1]
}

/* === evaluation === */

const pushSelf  = v => (c, s) => [false, stack.push(s, v)]
const pushIdent = v => (c, s) =>
  [false, stack.push(s, scope.lookup(c, v.value))]

const popArgs = (s0, b) => {
  const ps = new Map(), [vs, s1] = stack.popN(s0, b.params.length)
  for (let i = 0; i < b.params.length; i++) {
    ps.set(b.params[i], vs[i])
  }
  return [ps, s1]
}

const popPush = (f, ...parms) => (c, s0) => {
  const [args, s1] = stack.pop(s0, ...parms)
  return stack.push(s1, ...f(...args))
}

const pop2push = f => popPush(f, "_", "_")

const opI = op => popPush(
  (x, y) => [int(op(x.value, y.value))], "int", "int"
)

const opF = op => popPush(
  (x, y) => [float(op(x.value, y.value))], "float", "float"
)

const debug = (c, f) => {
  if (eq(scope.lookup(c, "__debug__", F), T)) { f() }
}

class Eval {
  constructor(code, c, s) {
    this.code   = code
    this.scope  = c
    this.stack  = s
  }
}

// TODO
const call = (c0, s0, tailPos = false) => {                   //  {{{1
  debug(c0, () => putErr("*** call ***"))
  const [[x], s1] = stack.pop(s0, "_"), xv = x.value
  const popOp = () => {
    const [[k], s2] = stack.pop(s1, "kwd")
    const r = (...xs) => stack.push(s2, ...xs)
    return [k.value, x.type + "." + k.value, r]
  }
  const mem = (i, l = xv.length) => 0 <= i && i < l
  switch (x.type) {
    case "str": {
      // TODO: inefficient implementation
      const xl = x.list
      const [opv, op, r] = popOp(), p = builtinPP(op, r)
      switch (opv) {
        case "ord":
          if (xl.length != 1) {
            throw new KE(...E.Expected("str of length 1 on stack"))
          }
          return r(int(xl[0].codePointAt(0)))
        case "append":
          return p(v => [str_(v.list.concat(xl))], "str")
        case "slice":
          return p((i, j, step) => {
            const i_ = nilToDef(i, 0        , "int")
            const j_ = nilToDef(j, xl.length, "int")
            if (step.value != 1) {
              throw new KE(...E.NotImplementedError(`${op}: step other than 1`))
            }
            return [str_(xl.slice(i_, j_ < 0 ? xl.length + j_ : j_))]
          } , "_", "_", "int")
        case "empty?":
          return r(bool(!xl.length))
        case "len":
          return r(int(xl.length))
        case "get":
          return p(i => {
            if (!mem(i.value, xl.length)) {
              throw new KE(...E.IndexError(op, i.value))
            }
            return [xl[i.value]]
          }, "int")
        case "member?":
          return p(i => [bool(mem(i.value, xl.length))], "int")
        case "elem?":
          return p(y =>
            [bool(xl.join("").includes(y.list.join("")))], "str"
          )
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
      const [opv, op, r] = popOp(), p = builtinPP(op, r)
      const g = () => { if (!xv.length) { throw new KE(...E.EmptyList(op)) } }
      switch (opv) {
        case "head":
          g(); return r(xv[0])
        case "tail":
          g(); return r(list(xv.slice(1)))
        case "uncons":
          g(); return r(xv[0], list(xv.slice(1)))
        case "cons":
          return p(v => [list([v].concat(xv))], "_")
        case "sort":
          return r(list(xv.slice().sort(cmp)))
        case "append":
          return p(v => [list(v.value.concat(xv))], "list")
        case "slice":
          return p((i, j, step) => {
            const i_ = nilToDef(i, 0        , "int")
            const j_ = nilToDef(j, xv.length, "int")
            if (step.value != 1) {
              throw new KE(...E.NotImplementedError(`${op}: step other than 1`))
            }
            return [list(xv.slice(i_, j_ < 0 ? xv.length + j_ : j_))]
          } , "_", "_", "int")
        case "empty?":
          return r(bool(!xv.length))
        case "len":
          return r(int(xv.length))
        case "get":
          return p(i => {
            if (!mem(i.value)) {
              throw new KE(...E.IndexError(op, i.value))
            }
            return [xv[i.value]]
          }, "int")
        case "member?":
          return p(i => [bool(mem(i.value))], "int")
        case "elem?":
          return p(y => [bool(xv.find(v => eq(v, y)))], "_")
        default:
          throw new KE(...E.UnknownField(op, x.type))
      }
    }
    // TODO: dict
    case "block": {
      const [ps, s2] = popArgs(s1, x)
      const c1 = x.params.length ? scope.fork(x.scope, ps) : x.scope
      return tailPos ? new Eval(x.code, c1, s2)
                     : evaluate(x.code)(c1, s2)
    }
    case "builtin":
      return x.run(c0, s1)
    case "multi": {
      const sig = stack.popN(s1, x.arity)[0].map(
        v => v.type == "record" ? v.rectype.name : v.type
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
    default:
      throw new KE(...E.UncallableType(x.type))
  }
}                                                             //  }}}1

const evaluate = code0 =>                                     //  {{{1
  (c = scope.new(), s = stack.empty()) => {
    let code = code0
    for (let i = 0; i < code.length; ++i) {
      const x = code[i]
      debug(c, () => {
        putErr(`==> eval ${show(x)}`)
        putErr("--> " + s.map(show).join(" "))
      })
      const [deferred, s_] = evl[x.type](x)(c, s); s = s_
      debug(c, () => putErr("<-- " + s.map(show).join(" ")))
      if (deferred) {
        const tailPos = i == code.length - 1
        s = call(c, s, tailPos)
        if (tailPos && s instanceof Eval) {
          debug(c, () => putErr("*** tail call ***"))
          code = s.code; c = s.scope; s = s.stack; i = -1
        }
      }
    }
    return s
  }                                                           //  }}}1

const evl = {
  nil: pushSelf, bool: pushSelf, int: pushSelf, float: pushSelf,
  str: pushSelf, kwd: pushSelf,
  list: v => (c, s) =>
    [false, stack.push(s, list(evaluate(v.value)(c)))],
  ident: v => (c, s) => [true, pushIdent(v)(c, s)[1]],
  quot: pushIdent,
  block: v => (c, s) =>
    [false, stack.push(s, { ...v, scope: c })],
}

const evalText = (text, c = undefined, s = undefined) =>
  evaluate(read(text))(c, s)

/* === show, toJS, fromJS === */

// TODO
const show = v => {                                           //  {{{1
  switch (v.type) {
    case "nil":
      return "nil"
    case "bool":
      return v.value ? "#t" : "#f"
    case "int":
      return v.value.toString()
    case "float": {
      const s = v.value.toString()
      return s.includes(".") ? s : s + ".0"
    }
    case "str": {
      const f = c => e["="+c] || isPrint(c) ? c : h(c.codePointAt(0))
      const h = n => n <= 0xffff ? p("\\u", 4, n) : p("\\U", 8, n)
      const p = (pre, w, n) => pre + n.toString(16).padStart(w, '0')
      const e = { "=\r":"\\r", "=\n":"\\n", "=\t":"\\t", "=\"":"\\\"",
                  "=\\":"\\\\" }
      return '"' + v.list.map(f).join("") + '"'
    }
    case "kwd":
      return ":" + (isIdent(v.value) ? v.value : show(str(v.value)))
    case "pair":
      return show(v.key) + " " + show(v.value) + " =>"
    case "list":
      return v.value.length ?
        "( " + v.value.map(show).join(" ") + " )" : "()"
    case "dict": {
      const f = ([k, x]) => show(kwd(k)) + " " + show(x) + " =>"
      return v.value.size ?
        "{ " + Array.from(v.value.entries(), f).join(", ") + " }" : "{ }"
    }
    case "ident":
      return v.value
    case "quot":
      return "'" + v.value
    case "block": {
      const f = xs => xs.map(show).join(" ")
      if (!v.params.length && !v.code.length) {
        return "[ ]"
      } else if (!v.params.length) {
        return "[ " + f(v.code) + " ]"
      } else if (!v.code.length) {
        return "[ " + v.params.join(" ") + " . ]"
      } else {
        return "[ " + v.params.join(" ") + " . " + f(v.code) + " ]"
      }
    }
    case "builtin":
      return "#<" + (v.prim ? "primitive" : "builtin") + ":" + v.name + ">"
    case "multi":
      return "#<multi:" + v.arity + ":" + v.name + ">"
    case "record-type":
      return "#<record-type:" + v.name + "(" + v.fields.join("#") + ")>"
    case "record": {
      const flds = zip(v.rectype.fields, v.values).map(
        ([k, v]) => show(pair(kwd(k), v))
      )
      return v.rectype.name + "{ " + flds.join(", ") + " }"
    }
    default:
      throw new Error(`type ${v.type} is not showable`)
  }
}                                                             //  }}}1

// TODO
const toJS = v => {                                           //  {{{1
  switch (v.type) {
    case "nil":
      return null
    case "bool":
    case "int":
    case "float":
    case "kwd":
      return v.value
    case "str":
      return v.list.join("")
    case "pair":
      return [v.key.value, toJS(v.value)]
    case "list":
      return v.value.map(toJS)
    case "dict":
      return new Map(Array.from(dict.entries(),
        ([k, v]) => [toJS(k), toJS(v)]
      ))
    case "record":
      throw "TODO"
    default:
      throw new Error(`type ${v.type} is not supported by toJS`)
  }
}                                                             //  }}}1

const fromJS = v => {                                         //  {{{1
  switch (typeof v) {
    case "boolean":
      return bool(v)
    case "number":
      return (v == (v|0)) ? int(v) : float(v)
    case "string":
      return str(v)
    case "object":
      if (v === null) {
        return nil
      } else if (v instanceof Array) {
        return list(v.map(fromJS))
      } else if (v instanceof Map) {
        return dict(Array.from(v.entries(),
          ([k, v]) => pair(kwd(k), fromJS(v))
        ))
      }
    default:
      throw new Error(`fromJS: cannot convert ${v}`)
  }
}                                                             //  }}}1

/* === eq, cmp === */

// TODO
const eq = (x, y) => {                                        //  {{{1
  if (x.type != y.type) { return false }
  const a = x.value, b = y.value
  switch (x.type) {
    case "nil":
      return true
    case "float":
      if (a === b) { return a !== 0 || 1 / a === 1 / b }
      if (a !== a) { return b !== b }
      return a == b
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
      throw "TODO"
    default:
      throw new KE(...E.UncomparableType(x.type))
  }
}                                                             //  }}}1

// TODO
const cmp = (x, y) => {                                       //  {{{1
  if (x.type != y.type) { return x.type < y.type ? -1 : 1 }
  const a = x.value, b = y.value
  switch (x.type) {
    case "nil":
      return 0
    case "float":
      return eq(x, y) ? 0 : a < b ? -1 : b < a ? 1 : null
    case "bool":
    case "int":
    case "kwd":
    case "ident":
    case "quot":
      return cmpPrim(a, b)
    case "str":
      return cmpArray(x.list, y.list, cmpPrim)
    case "pair":
      return cmp(x.key, y.key) && cmp(a, b)
    case "list":
      return cmpArray(a, b, cmp)
    case "dict":
      return cmp(dictToList(x), dictToList(y))
    case "record-type":
      throw "TODO"
    case "record":
      throw "TODO"
    default:
      throw new KE(...E.UncomparableType(x.type))
  }
}                                                             //  }}}1

const eqArray = (x, y, eq) => {
  if (a.length != b.length) { return false }
  for (let i = 0; i < a.length; ++i) {
    if (!eq(a[i], b[i])) { return false }
  }
  return true
}

const cmpArray = (x, y, cmp) => {
  const m = Math.max(x.length, y.length)
  for (let i = 0; i < m; ++i) {
    if (i >= x.length && i < y.length) { return -1 }
    if (i < x.length && i >= y.length) { return  1 }
    const c = cmp(x[i], y[i])
    if (c != 0) { return c }
  }
  return 0
}

const eqPrim  = (a, b) => a == b
const cmpPrim = (a, b) => a == b ? 0 : a < b ? -1 : 1

const cmp_lt  = c => bool(c == -1)
const cmp_lte = c => bool(c == -1 || c == 0)
const cmp_gt  = c => bool(c ==  1)
const cmp_gte = c => bool(c ==  1 || c == 0)

/* === modules & primitives === */

const modules = new Map(
  ["__bltn__", "__prld__", "__main__"].map(k => [k, new Map()])
)

// TODO
modules.set("__prim__", new Map([                             //  {{{1
  mkBltn("__call__", call),
  mkBltn("__apply__", (c, s) => {
    throw "TODO"
  }),
  mkBltn("__apply-dict__", (c, s) => {
    throw "TODO"
  }),
  mkBltn("__if__", (c, s0) => {
    const [[b, tb, fb], s1] = stack.pop(s0, "bool", "_", "_")
    return call(c, stack.push(s1, truthy(b) ? tb : fb))
  }),
  mkBltn("__def__", (c, s0) => {
    const [[k, v], s1] = stack.pop(s0, "kwd", "_")
    scope.define(c, k.value, v); return s1
  }),
  mkBltn("__defmulti__", (c, s0) => {
    const [[k, sig, b], s1] = stack.pop(s0, "kwd", "list", "block")
    for (const x of sig.value) { expect(x, "kwd") }
    const sig_ = sig.value.map(
      k =>  k.value == "_" || types.includes(k.value) ? k.value :
              show(expect(scope.lookup(c, k.value), "record-type"))
    )
    let m = scope.lookup(c, k.value, null)
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
  mkBltn("__defrecord__", (c, s0) => {
    const [[k, fs], s1] = stack.pop(s0, "kwd", "list")
    for (const x of fs.value) { expect(x, "kwd") }
    const name = k.value, pred = name + "?"
    const r = record_type(name, fs.value.map(x => x.value))
    const p = builtinPP(c.module + ":" + pred)(
      x => [x.type == "record" && eq(x.rectype, r)], "_"
    )
    scope.define(c, name, r)
    scope.define(c, pred, p)
    return s1
  }),
  mkBltnPP("__=>__", (k, v) => [pair(k, v)], "kwd", "_"),
  mkBltnPP("__dict__", l => {
    for (const x of l.value) { expect(x, "pair") }
    return [dict(l.value)]
  }, "list"),
  mkBltn("__swap__", pop2push((x, y) => [y, x])),
  mkBltnPP("__show__", x => [str(show(x))], "_"),
  mkBltnPP("__say__", x => { say(x.list.join("")); return [] }, "str"),
  mkBltn("__ask__", (c, s) => {
    throw new KE(...E.NotImplementedError("ask"))
  }),
  mkBltnPP("__type__", x => [kwd(x.type, "_")], "_"),
  mkBltnPP("__callable?__",
    v => [bool(callableTypes.includes(v.type))], "_"
  ),
  mkBltnPP("__function?__",
    v => [bool(functionTypes.includes(v.type))], "_"
  ),
  mkBltn("__module-get__", (c, s0) => {
    const [[k, m], s1] = stack.pop(s0, "kwd", "kwd")
    const mod = getModule(m.value)
    if (!mod.has(k.value)) { throw new KE(...E.LookupFailed(k.value)) }
    return stack.push(s1, mod.get(k.value))
  }),
  mkBltn("__module-defs__", (c, s0) => {
    const [[m], s1] = stack.pop(s0, "kwd")
    const l = [...getModule(m.value).keys()].sort().map(kwd)
    return stack.push(s1, list(l))
  }),
  mkBltn("__name__", (c, s) => stack.push(s, kwd(c.module))),
  mkBltn("__not__",   popPush(x => [bool(!truthy(x))], "_")),
  mkBltn("__and__",   pop2push((x, y) => [truthy(x) ? y : x ])),
  mkBltn("__or__" ,   pop2push((x, y) => [truthy(x) ? x : y ])),
  mkBltn("__=__",     pop2push((x, y) => [bool( eq(x, y))   ])),
  mkBltn("__not=__",  pop2push((x, y) => [bool(!eq(x, y))   ])),
  mkBltn("__<__",     pop2push((x, y) => [cmp_lt (cmp(x, y))])),
  mkBltn("__<=__",    pop2push((x, y) => [cmp_lte(cmp(x, y))])),
  mkBltn("__>__",     pop2push((x, y) => [cmp_gt (cmp(x, y))])),
  mkBltn("__>=__",    pop2push((x, y) => [cmp_gte(cmp(x, y))])),
  mkBltn("__int+__"  , opI((x, y) => x + y)),
  mkBltn("__int-__"  , opI((x, y) => x - y)),
  mkBltn("__int*__"  , opI((x, y) => x * y)),
  mkBltn("__div__"   , opI((x, y) => {
    if (y == 0) { throw new KE(...E.DivideByZero()) }
    return (x / y) | 0
  })),
  mkBltn("__mod__"   , opI((x, y) => x % y)),                //  TODO
  mkBltn("__float+__", opF((x, y) => x + y)),
  mkBltn("__float-__", opF((x, y) => x - y)),
  mkBltn("__float*__", opF((x, y) => x * y)),
  mkBltn("__float/__", opF((x, y) => x / y)),
  mkBltnPP("__chr__",
    x => [str(String.fromCodePoint(x.value))], "int"
  ),
  mkBltnPP("__int->float__", n => [float(n.value)], "int"),
  mkBltn("__record->dict__", (c, s) => {
    throw "TODO"
  }),
  mkBltnPP("__record-type__", x => [x.rectype], "record"),
  mkBltnPP("__record-type-name__", x => [kwd(x.name)], "record-type"),
  mkBltnPP("__record-type-fields__",
    x => [list(x.fields.map(kwd))], "record-type"
  ),
  mkBltn("__show-stack__", (c, s) => {
    for (const x of stack.toArray(s)) { say(show(x)) }
    return s
  }),
  mkBltn("__clear-stack__", (c, s) => stack.empty()),
  mkBltn("__nya__", (c, s) => {
    throw "TODO"
  }),
]))                                                           //  }}}1

const getModule = m => {
  if (!modules.has(m)) { throw new KE(...E.ModuleNotFound(m)) }
  return modules.get(m)
}

const types = [
  "nil", "bool", "int", "float", "str", "kwd", "pair", "list", "dict",
  "ident", "quot", "block", "builtin", "multi", "record-type",
  "record"
]

const functionTypes = ["block", "builtin", "multi", "record-type"]
const callableTypes =
  ["str", "pair", "list", "dict", "record"].concat(functionTypes)

for (const t of types) {
  modules.get("__bltn__").set(t+"?", builtin(
    t+"?", popPush(x => [bool(x.type == t)], "_"), false
  ))
}

/* === miscellaneous === */

// TODO
const zip = (xs, ys) => xs.map((x, i) => [x, ys[i]])

/* === files === */

// NB: browser only; Promise
const requestFile = fname => new Promise((resolve, reject) => {
  const r = new XMLHttpRequest()
  r.addEventListener("load", () => resolve(r.responseText))
  r.open("GET", fname)
  r.send()
})

// NB: node.js only; Promise
const readFile = fname => new Promise((resolve, reject) => {
  _req("fs").readFile(fname, "utf8", (err, data) => resolve(data))
});

// NB: node.js only; Promise
// TODO
const fileLines = fname => readFile(fname).then(
  text => text.split("\n").map((line, i) => [i+1, line])
)

// NB: Promise
const evalFile = (fname, modOrCtx = undefined, s = undefined) => {
  const c = typeof modOrCtx === "string" ? scope.new(modOrCtx) : modOrCtx
  return (_req ? readFile : requestFile)(fname).then(
    text => evalText(text, c, s)
  )
}

// NB: Promise
const loadPrelude = (fname = "lib/prelude.knk") =>
  modules.get("__prld__").get("def") ?
    new Promise((resolve, reject) => resolve()) :
    evalFile(fname, "__prld__")

/* === output === */

// NB: node.js only
const putOut = (s = "") => process.stdout.write(s + "\n")
const putErr = (s = "") => process.stderr.write(s + "\n")

/* === repl === */

// NB: node.js only

const repl_init = c => ["clear-stack", "show-stack"].forEach(x =>
  scope.define(c, x, scope.lookup(c, "__" + x + "__"))
)

const repl_process_line = (line, c, s, stdout, stderr) => {     // {{{1
  if (line) {
    try {
      s = evalText(line, c, s)
      if (!stack.null(s) && !",;".includes(line[0])) {
        stdout.write(show(stack.top(s)) + "\n")
      }
    } catch(e) {
      if (e instanceof KonekoError) {
        stderr.write("*** ERROR: " + e.message + "\n")
      } else {
        throw e
      }
    }
  }
  return s
}                                                             //  }}}1

const repl = (verbose = false) => {                           //  {{{1
  if (!process.stdin.isTTY) {
    evalFile("/dev/stdin")                                    //  TODO
    return
  }
  const c = scope.new(); let s = stack.empty(); repl_init(c)
  if (verbose) { scope.define(c, "__debug__", T) }
  const rl = _req("readline").createInterface({
    input: process.stdin, output: process.stdout, prompt: ">>> "
  })
  rl.on("line", line => {
    s = repl_process_line(line, c, s, process.stdout, process.stderr)
    rl.prompt()
  }).on("close", () => { putOut() })
  rl.prompt()
}                                                             //  }}}1

/* === doctest === */

// NB: node.js only

// NB: Promise
const doctest = (files, verbose = false) =>
  testFiles(files, verbose).then(([t,o,fail]) => fail == 0)

const doctest_ = (files, verbose = false) =>
  doctest(files, verbose).then(ok => ok || process.exit(1))

// NB: Promise
const testFiles = (files, verbose = false) => {               //  {{{1
  let total = 0, ok = 0, fail = 0
  const nl = fname => fileLines(fname).then(lines => [fname, lines])
  return Promise.all(files.map(nl)).then(fs => {
    for (const [fname, lines] of fs) {
      const md      = /\.md$/u.test(fname)
      const test    = md ? testMarkdownFile : testKonekoFile
      putOut(`=== Testing ${fname} (${md ? "markdown" : "koneko"}) ===`)
      const [t,o,f] = test(fname, lines, verbose)
      total += t; ok += o; fail += f
      putOut()
    }
    putOut("=== Summary ===")
    putOut(`Files: ${files.length}.`)
    printTestSummary(total, ok, fail)
    return [total, ok, fail]
  })
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
    if (!in_b && RegExp("^```koneko$").test(l)) {
      b = []; in_b = true
    } else if (in_b) {
      if (RegExp("^```$").test(l)) {
        bs.push(b); in_b = false
      } else {
        b.push([ln, l])
      }
    }
  }
  return bs
}                                                             //  }}}1

const testExamples = (es, verbose = false) => {               //  {{{1
  let total = 0, ok = 0, fail = 0
  for (const g of es) {
    const [t,o,f] = testExampleGroup(g, verbose)
    total += t; ok += o; fail += f
  }
  if (verbose) {
    putOut("=== Summary ===")
    printTestSummary(total, ok, fail)
  }
  return [total, ok, fail]
}                                                             //  }}}1

const testExampleGroup = (g, verbose = false) => {            //  {{{1
  const wr = l => ({ write: s => l.push(s.slice(0,-1)) })
  modules.set("__main__", new Map())                          //  TODO
  const c = scope.new(); let s = stack.empty(); repl_init(c)
  let ok = 0, fail = 0
  for (const e of g) {
    const out = [], err = []
    overrides.say = s => out.push(s)
    s = repl_process_line(e.input, c, s, wr(out), wr(err))
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

const _testFile = f => (fname, lines, verbose = false) =>
  testExamples(blocksToExamples(fname, f(lines)), verbose)

const testKonekoFile    = _testFile(knkCommentBlocks)
const testMarkdownFile  = _testFile(mdCodeBlocks)

const compareExampleOutput = (exp, got, err) => {
  // NB: can't == arrays :(
  const exp_ = exp.map(l => l == "<BLANKLINE>" ? "" : l).join("\n")
  const got_ = got.join("\n"), err_ = err.join("\n")
  return !err.length ? exp_ == got_ : !got.length && exp_ == err_ &&
    /^\*\*\* ERROR: /u.test(err[0])
}

const printTestSummary = (total, ok, fail) => {
  printTTPF(total, ok, fail)
  putOut(`Test ${fail == 0 ? "passed" : "failed"}.`)
}

const printTTPF = (total, ok, fail) =>
  putOut(`Total: ${total}, Tried: ${ok + fail}, Passed: ${ok}, Failed: ${fail}.`)

const printSucc = ex => {
  putOut("Trying:\n  " + ex.input)
  putOut("Expecting:")
  for (const l of ex.output) { putOut("  " + l) }
  putOut("ok")
}

const printFail = (ex, out, err) => {                         //  {{{1
  putErr(`File ${ex.fname}, line ${ex.lineno}`)
  putErr("Failed example:\n  " + ex.input)
  putErr("Expected:")
  for (const l of ex.output) { putErr("  " + l) }
  putErr("Got:")
  for (const l of out) { putErr("  " + l) }
  if (err.length) {
    putErr("Errors:")
    for (const l of err) { putErr("  " + l) }
  }
}                                                             //  }}}1

/* === main === */

// NB: node.js only

// TODO: use args; --eval, --interactive, ...
const main = () => loadPrelude().then(() => {
  const argv    = process.argv.slice(2)
  const opts    = argv.filter(a =>  a.startsWith("-"))
  const args    = argv.filter(a => !a.startsWith("-"))
  const verbose = opts.includes("-v")
  if (opts.includes("--doctest")) {
    doctest_(args, verbose).catch(e => {
      console.error(e); process.exit(1)
    })
  } else if (args.length) {
    evalFile(args[0])
  } else {
    repl(verbose)
  }
})

/* === exports & overrides === */

const say = s => (overrides.say || (_req ? putOut : console.log))(s)

const overrides = {}

_mod[_exp] = {
  KonekoError, read, evaluate, evalText, show, toJS, fromJS,
  loadPrelude, overrides,
  initContext: scope.new, emptyStack: stack.empty,
  ...(_req ? { repl, doctest, doctest_ } : {})
}

// NB: node.js only
if (_req && _mod === _req.main) { main() }

})(
  ...(typeof module === "undefined" ?
    [this  , "koneko" , null   , XRegExp] : // browser
    [module, "exports", require,  RegExp])  // node.js
)

// vim: set tw=70 sw=2 sts=2 et fdm=marker :
