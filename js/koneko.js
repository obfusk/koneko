//  --                                                          ; {{{1
//
//  File        : koneko.js
//  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
//  Date        : 2019-11-18
//
//  Copyright   : Copyright (C) 2019  Felix C. Stegerman
//  Version     : v0.0.1
//  License     : GPLv3+
//
//  --                                                          ; }}}1

"use strict";
((_mod, _exp, _req) => {

// TODO:
//  * multi (arity, name, table)
//  * record-type (name, fields)
//  * record (type, values)

class KonekoError extends Error {
  constructor(...args) {
    super(...args);
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, KonekoError)
    }
    this.name = "KonekoError"
  }
}

const _E = KonekoError

// TODO: inefficient
const stack = {                                               //  {{{1
  empty: () => [],
  null: s => !s.length,
  head: s => s.slice(-1)[0],
  push: (s, ...args) => s.concat(args),
  pop: (s0, ...args) => {
    if (s0.length < args.length) { throw new _E("stack underflow") }
    const s1 = s0.slice(), r = []
    args.reverse()
    for (const t of args) {
      const x = s1.pop()
      if (t != "value" && t != x.type) {
        throw new _E(`expected ${t} on stack`)
      }
      r.unshift(x)
    }
    return [r, s1]
  },
  popN: (s0, n) => stack.pop(s0, ...Array(n).fill("value")),
}                                                             //  }}}1

// TODO: inefficient
const scope = {                                               //  {{{1
  new: (module = "__main__") =>
    ({ module, parent: null, table: {} }),
  fork: (c, table) => ({ parent: c, table }),
  lookup: (c, k) => {
    if (k in modules.__prim__) { return modules.__prim__[k] }
    let c_ = c
    while (c_) {
      if (k in c_.table) { return c_.table[k] }
      c_ = c_.parent
    }
    for (const m of [c.module, "__bltn__", "__prld__"]) {
      if (k in modules[m]) { return modules[m][k] }
    }
    throw new _E(`name ${k} is not defined`)
  },
}                                                             //  }}}1

const _tv   = (type, value) => ({ type, value })
const kwd   = s => _tv("kwd", s)
const str   = s => _tv("str", s)
const pair  = (key, value) => ({ type: "pair", key, value })
const list  = l => _tv("list", l)
const ident = i => _tv("ident", i)
const quot  = i => _tv("quot", i)
const block = (params, code, scope = null) =>
  ({ type: "block", params, code, scope })
const builtin = (name, f) =>
  ({ type: "builtin", prim: true, name, run: f })

const isIdent = s => RegExp("^"+_naiveIdent+"$", "u").exec(s) &&
  !Array.from("'!:"   ).includes(s[0]) &&
  !Array.from(":({["  ).includes(s.slice(-1)) &&
  !Array.from("(){}[]").includes(s) &&
  s != "()" && s != "nil" && !isInt(s) && !isFloat(s)

const isInt   = s => /^-?\d+$/u.test(s)
const isFloat = s => /^-?\d+(\.\d+e\d+|\.\d+|e\d+)$/u.test(s)
const isPrint = s => /^[\p{L}\p{M}\p{N}\p{P}\p{S}\{Zs}]+$/u.test(s)

const _naiveIdent = /[\p{L}\p{N}\p{S}\(\)\{\}\[\]@%&*\-_\/?'!:]+/.source

const truthy = v =>
  v.type == "nil" || (v.type == "bool" && v.value == false)

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

const parseOne = (s, p0 = 0, end = null) => {                 //  {{{1
  let m, p1
  const t = pat => {
    const r = new RegExp(sp+"*("+pat+")(?:"+sp+"+|$)", "usy")
    r.lastIndex = p0; m = r.exec(s); p1 = r.lastIndex
    return !!m
  }
  const sp  = "[\\s,]"
  const hd  = "[0-9a-fA-F]"
  const hex = '(\\\\x'+hd+'{2})|(\\\\u'+hd+'{4})|(\\\\U'+hd+'{8})'
  const chr = hex + '|(\\\\[rnt\\\\"])|([^"])'
  const esc = { "\\r":"\r", "\\n":"\n", "\\t":"\t", "\\\"":"\"", "\\\\":"\\" }
  const escapedStrBody = k => {
    const l = [], r = RegExp(chr, "usy"); r.lastIndex = p0 + k
    const f = w => String.fromCodePoint(parseInt(cm[0].slice(w), 16))
    let cm
    while (cm = r.exec(s)) {
      l.push(cm[1] || cm[2] || cm[3] ? f(2) : cm[4] ? esc[cm[4]] : cm[5])
    }
    return l.join("")
  }
  const parseBlock = (pre = "") => {
    let params = [], p2 = p1
    if (t(pre+"\\["+sp+"+((?:"+_naiveIdent+sp+"+)*)"+"\\.")) {
      params = m[2].split(RegExp(sp+"+")).filter(x => x); p2 = p1
    }
    const [p3, vs] = parseMany(s, p2, "]")
    return [p3, block(params, vs)]
  }

  // primitives
  if (t("nil")) {
    return [p1, { type: "nil" } ]
  } else if (t("#t") || t("#f")) {
    return [p1, { type: "bool", value: m[1] == "#t" } ]
  } else if (t("-?\\d+")) {
    return [p1, { type: "int", value: parseInt(m[1], 10) }]
  } else if (t("-?\\d+(\\.\\d+e\\d+|\\.\\d+|e\\d+)")) {
    return [p1, { type: "float", value: parseFloat(m[1]) }]
  } else if (t('"(' + chr + '*)"')) {
    return [p1, str(escapedStrBody(1))]
  } else if (t(":(" + _naiveIdent + ")") && isIdent(m[2])) {
    return [p1, kwd(m[2])]
  } else if (t(':"(' + chr + '*)"')) {
    return [p1, kwd(escapedStrBody(2))]
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
  } else if (t("('?)([.!])(" + _naiveIdent + ")")) {
    const sw = ident("__swap__"), ca = ident("__call__")
    const vs = [ident(m[4]), sw, ca, ...(m[3] == "!" ? [ca]: [])]
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
  throw new _E(`parse error: no match at pos ${p0}`)
}                                                             //  }}}1

const parseMany = (s, p, end = null) => {
  const vs = []; let v
  do {
    [p, ...v] = parseOne(s, p, end)
    if (end && v[0].end) { return [p, vs] }
    vs.push(...v)
  } while (p < s.length)
  if (end) {
    throw new _E(`parse error: expected "${end}" at pos ${p}`)
  }
  return [p, vs]
}

const read = s => {
  let p = 0, m
  if (m = /^#!.*\n/u.exec(s)) { p = m[0].length }
  if (/^[\s,]*$/u.test(s.slice(p))) { return [] }
  return parseMany(s, p)[1]
}

const pushSelf = v => (c, s) => stack.push(s, v)

const pushIdent = v => (c, s) =>
  stack.push(s, scope.lookup(c, v.value))

const popArgs = (s0, b) => {
  const ps = {}, [vs, s1] = stack.popN(s0, b.params.length)
  for (let i = 0; i < b.params.length; i++) { ps[b.params[i]] = vs[i] }
  return [ps, s1]
}

const popPush = (f, ...parms) => (c, s0) => {
  const [args, s1] = stack.pop(s0, ...parms)
  return stack.push(s1, f(...args))
}

const opI = op => popPush(
  (x, y) => ({ type: "int", value: op(x.value, y.value) }),
  "int", "int"
)

const opF = op => popPush(
  (x, y) => ({ type: "float", value: op(x.value, y.value) }),
  "float", "float"
)

const lookupFailed = (t, op) => `${t} has no field named ${op}`

// TODO
const call = (c0, s0) => {                                    //  {{{1
  const [[x], s1] = stack.pop(s0, "value")
  switch (x.type) {
    // str
    case "pair": {
      const [op, s2] = stack.pop(s1, "kwd")
      switch (op) {
        case "key":
          return stack.push(s2, x.key)
        case "value":
          return stack.push(s2, x.value)
        default:
          throw new _E(lookupFailed(x.type, op))
      }
    }
    // list dict
    case "block": {
      const [ps, s2] = popArgs(s1, x)
      const c1 = x.params.length ? scope.fork(x.scope, ps) : x.scope
      return evaluate(x.code)(c1, s2)
    }
    case "builtin":
      return x.run(c0, s1)
    // multi recordt record
    default:
      throw new _E(`type ${x.type} is not callable`)
  }
}                                                             //  }}}1

const evaluate = code => (c = scope.new(), s = stack.empty()) => {
  for (const x of code) { s = evl[x.type](x)(c, s) }
  return s
}

const evl = {
  nil: pushSelf, bool: pushSelf, int: pushSelf, float: pushSelf,
  str: pushSelf, kwd: pushSelf,
  list: v => (c, s) => {
    const l = evaluate(v.value)(c); l.reverse()
    return stack.push(s, list(l))
  },
  ident: v => (c, s) =>
    modules.__prim__.__call__.run(c, pushIdent(v)(c, s)),
  quot: pushIdent,
  block: v => (c, s) =>
    stack.push(s, { ...v, scope: c }),
}

const evalText = s => evaluate(read(s))

// TODO
const show = (v) => {                                         //  {{{1
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
      return '"' + Array.from(v.value).map(f).join("") + '"'
    }
    case "kwd":
      return ":" + (isIdent(v.value) ? v.value : show(str(v.value)))
    case "pair":
      return show(v.key) + " " + show(v.value) + " =>"
    case "list":
      return v.value.length ?
        "( " + v.value.map(show).join(" ") + " )" : "()"
    case "dict": {
      const f = k =>
        show(kwd(k.slice(1) /* :key */)) + " " + show(v[k]) + " =>"
      return v.value.length ?
        "{ " + Object.keys(v.value).map(f).join(", ") + " }" : "{ }"
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
    // multi recordt record
    default:
      throw new _E(`type ${v.type} is not showable`)
  }
}                                                             //  }}}1

// TODO
const modules = {                                             //  {{{1
  __prim__: {
    __call__: builtin("call", call),
    // apply, apply-dict if def
    // defmulti defrecord
    "__=>__": builtin("=>", (c, s) => popPush(pair, "kwd", "value")),
    // dict swap
    // show say ask type callable? function?
    // module-get module-defs name
    // not and or
    // = not= < <= > >=
    "__int+__"  : builtin("int+"  , opI((x, y) =>            x + y)),
    "__int-__"  : builtin("int-"  , opI((x, y) =>            x - y)),
    "__int*__"  : builtin("int*"  , opI((x, y) =>            x * y)),
    "__div__"   : builtin("div"   , opI((x, y) => Math.floor(x / y))),
    "__mod__"   : builtin("mod"   , opI((x, y) =>            x % y)),
    "__float+__": builtin("float+", opF((x, y) =>            x + y)),
    "__float-__": builtin("float-", opF((x, y) =>            x - y)),
    "__float*__": builtin("float*", opF((x, y) =>            x * y)),
    "__float/__": builtin("float/", opF((x, y) =>            x / y)),
    // chr int->float record->dict record-type
    // record-type-name record-type-fields
    // show-stack clear-stack nya
  },
  __bltn__: {},
  __prld__: {},
  __main__: {},
}                                                             //  }}}1

const repl = () => {                                          //  {{{1
  const c = scope.new(); let s = stack.empty()
  const rl = require("readline").createInterface({
    input: process.stdin, output: process.stdout, prompt: ">>> "
  })
  rl.on("line", line => {
    if (line) {
      try {
        s = evalText(line)(c, s)
        if (!stack.null(s) && !",;".includes(line[0])) {
          process.stdout.write(show(stack.head(s)) + "\n")
        }
      } catch(e) {
        if (e instanceof KonekoError) {
          process.stderr.write("*** ERROR: " + e.message + "\n")
        } else {
          throw e
        }
      }
    }
    rl.prompt();
  }).on("close", () => {
    // process.exit(0)
  })
  rl.prompt()
}                                                             //  }}}1

_mod[_exp] = { KonekoError, read, show, evaluate, evalText, repl }

if (_req && _mod === _req.main) { repl() }

})(...(typeof module === "undefined" ? [this  , "koneko" , null]
                                     : [module, "exports", require]));

// vim: set tw=70 sw=2 sts=2 et fdm=marker :
