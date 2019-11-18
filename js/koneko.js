"use strict";

// TODO:
//  * multi (arity, name, table)
//  * record-type (name, fields)
//  * record (type, values)

// TODO: inefficient
const stack = {
  new: () => [],
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

const pushSelf = v => (c, s) => stack.push(s, v)

const popArgs = (s0, b) => {
  const ps = {}, [vs, s1] = stack.popN(s0, b.params.length)
  for (let i = 0; i < b.params.length; i++) {
    ps[b.params[i]] = vs[i]
  }
  return [ps, s1]
}

const arith = op => (c, s0) => {
  const [[x, y], s1] = stack.pop(s0, "int", "int")
  return stack.push(s1, op(x.value, y.value))
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
      break
    case "builtin":
      return x.run(c, s1)
      break
    // multi recordt record
    default:
      throw `uncallable: ${x.type}`
  }
}

// TODO
const run = {
  // nil bool
  int: pushSelf,
  // float str kwd pair list dict
  ident: v => (c, s) =>
    modules.__prim__.__call__.run(
      c, stack.push(s, scope.lookup(c, v.value))
    ),
  // quot
  block: v => (c, s) =>
    stack.push(s, { ...v, scope: c }),
  // builtin multi recordt record
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

const evaluate = code => (c, s) => {
  for (let x of code) {
    s = run[x.type](x)(c, s)
  }
  return s
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
  evaluate(example)(scope.new(), stack.new())

module.exports = { modules, example, test }

if (require.main === module) {
  console.log(test())
}
