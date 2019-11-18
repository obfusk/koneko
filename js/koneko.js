"use strict";

// TODO:
//  * pair (key, value)
//  * multi (arity, name, table)
//  * record-type (name, fields)
//  * record (type, values)

// TODO: inefficient
const stack = {
  empty: () => [],
  push: (s, ...args) => s.concat(args),
  pop: (s0, ...args) => {
    if (s0.length < args.length) {
      throw "stack underflow"
    }
    const s1 = s0.slice(), r = []
    args.reverse()
    for (let t of args) {
      const x = s1.pop()
      if (t != "value" && t != x.type) {
        throw `expected: ${t}`
      }
      r.unshift(x)
    }
    return [r, s1]
  },
  popN: (s0, n) => stack.pop(s0, ...Array(n).fill("value")),
}

// TODO: inefficient
const scope = {
  new: (module = "__main__") =>
    ({ module, parent: null, table: {} }),
  fork: (c, table) => ({ parent: c, table }),
  lookup: (c, k) => {
    if (k in modules.__prim__) {
      return modules.__prim__[k]
    }
    while (c) {
      if (k in c.table) {
        return c.table[k]
      }
      c = c.parent
    }
    for (let m in [c.module, "__bltn__", "__prld__"]) {
      if (k in modules[m]) {
        return modules[m][k]
      }
    }
    throw `lookup failed: ${k}`
  },
}

// TODO
const isIdent = s => !!s.match(/^[\p{L}\p{N}\p{S}]+$/u)

const pushSelf = v => (c, s) => stack.push(s, v)

const pushIdent = v => (c, s) =>
  stack.push(s, scope.lookup(c, v.value))

const popArgs = (s0, b) => {
  const ps = {}, [vs, s1] = stack.popN(s0, b.params.length)
  for (let i = 0; i < b.params.length; i++) {
    ps[b.params[i]] = vs[i]
  }
  return [ps, s1]
}

const arith = op => (c, s0) => {
  const [[x, y], s1] = stack.pop(s0, "int", "int")
  return stack.push(s1, { type: "int", value: op(x.value, y.value) })
}

// TODO
const call = (c, s0) => {
  const [[x], s1] = stack.pop(s0, "value")
  switch (x.type) {
    // str pair list dict
    case "block":
      const [ps, s2] = popArgs(s0, x)
      const c1 = x.params.length ? scope.fork(x.scope, ps) : x.scope
      return evaluate(x.code)(c1, s2)
    case "builtin":
      return x.run(c, s1)
    // multi recordt record
    default:
      throw `uncallable: ${x.type}`
  }
}

const evaluate = code => (c, s) => {
  for (let x of code) {
    s = evl[x.type](x)(c, s)
  }
  return s
}

const evl = {
  nil: pushSelf, bool: pushSelf, int: pushSelf, float: pushSelf,
  str: pushSelf, kwd: pushSelf,
  list: v => (c, s) => {
    const l = evaluate(v)(c, stack.empty()); l.reverse()
    return stack.push({ type: list, value: l })
  },
  ident: v => (c, s) =>
    modules.__prim__.__call__.run(c, pushIdent(v)(c, s)),
  quot: pushIdent,
  block: v => (c, s) =>
    stack.push(s, { ...v, scope: c }),
}

// TODO
const show = (v) => {
  switch (v.type) {
    case "nil":
      return "nil"
    case "bool":
      return v.value ? "#t" : "#f"
    case "int":
      return v.value.toString()
    case "float":
      return v.value.toString()                               //  TODO
    case "str":
      return JSON.stringify(v.value)                          //  TODO
    case "kwd":
      return ":" + (isIdent(v.value) ? v.value
                    : JSON.stringify(v.value))                //  TODO
    case "pair":
      return show(v.key) + " " + show(v.value) + " =>"
    case "list":
      return v.value.length ?
        "( " + v.value.map(show).join(" ") + " )" : "()"
    case "dict": {
      const f = k =>
        show({ type: "kwd", value: k.slice(1) /* :key */ }) +
          " " + show(v[k]) + " =>"
      return v.value.length ?
        "{ " + Object.keys(v.value).map(f).join(", ") + " }" : "{ }"
    }
    case "ident":
      return v.value
    // quot
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
      throw `unshowable: ${v.type}`
  }
}

// TODO
const modules = {
  __prim__: {
    __call__: {
      type: "builtin",
      run: call
    },
    // apply, apply-dict if def
    // defmulti defrecord => dict swap
    // show say ask type callable? function?
    // module-get module-defs name
    // not and or
    // = not= < <= > >=
    "__int+__": {
      type: "builtin",
      run: arith ((x, y) => x + y)
    },
    "__int-__": {
      type: "builtin",
      run: arith ((x, y) => x - y)
    },
    // int* div mod
    // float+ float- float* float/
    // chr int->float record->dict record-type
    // record-type-name record-type-fields
    // show-stack clear-stack nya
  },
  __bltn__: {
  },
  __prld__: {
  },
  __main__: {
  },
}

for (let k of Object.keys(modules.__prim__)) {
  modules.__prim__[k].name = k
  modules.__prim__[k].prim = true
}

const example = [
  {
    type: "int",
    value: 7
  },
  {
    type: "block",
    params: ["x"],
    code: [
      {
        type: "int",
        value: 2
      },
      {
        type: "ident",
        value: "__int-__"
      },
    ],
    scope: null
  },
  {
    type: "ident",
    value: "__call__"
  }
]

const test = () =>
  evaluate(example)(scope.new(), stack.empty())

module.exports = { modules, example, test }

if (require.main === module) {
  console.log(
    example.map(show).join(" "), "\n=>", test().map(show).join(" ")
  )
}
