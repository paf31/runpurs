#!/usr/bin/env node

const fs = require('fs');
const tmp = require('tmp');
const path = require('path');
const purs = require('purescript');
const { spawnSync } = require('child_process');

const expression = process.argv[2] || '\\input -> input';

const input = JSON.parse(fs.readFileSync('/dev/stdin', 'utf8'));

const inputFile = tmp.fileSync({ postfix: '.purs' }).name;
const outputFolder = tmp.dirSync().name;

fs.writeFileSync(inputFile, `
module Main where

foreign import data Expr :: Type -> Type

foreign import map :: forall a b. (a -> b) -> Array a -> Array b
foreign import filter :: forall a. (a -> Boolean) -> Array a -> Array a
foreign import append :: forall a. Array a -> Array a -> Array a
foreign import eq :: forall a. a -> a -> Boolean

infix 4 eq as ==

main :: Partial => _ -> _
main = 
  ${ expression.split('\n').join('\n  ') }
`);

var spawn = spawnSync(purs, ['compile', '-g', 'corefn', '-o', outputFolder, '--json-errors', inputFile], { stdio: 'pipe' })
var spawnOutput = JSON.parse(spawn.stdout.toString());

if (spawnOutput.errors.length) {
  spawnOutput.errors.forEach(({ message, errorCode }) => 
    console.error(`[${errorCode}] ${message}`));
  return;
}

const corefn = JSON.parse(fs.readFileSync(path.join(outputFolder, 'Main', 'corefn.json'), 'utf8'));

// TODO: this is unnecessary, since we only support a single 
// declaration right now anyway. However, that might change,
// in which case we should add each of these to the initialEnv
// object.
var decls = {};
corefn.decls.forEach((decl) => {
  decls[decl.identifier] = decl.expression;
});

const lit = (val, env) => {
  switch (val.literalType) {
    case "ObjectLiteral":
      var result = {};
      val.value.forEach(([key, value]) => {
        result[key] = eval(value, env);
      });
      
      return result;
    case "ArrayLiteral":
      return val.value.map((e) => eval(e, env));
    case "IntLiteral":
    case "NumberLiteral":
    case "BooleanLiteral":
    case "StringLiteral":
    case "CharLiteral":
      return val.value;
    default:
      throw "unknown literal type: " + val.literalType;  
  }
};

const bindLiteral = (lit, e) => {
  switch (lit.literalType) {
    case "ObjectLiteral":
      var result = {};
      for (var i = 0; i < lit.value.length; i++) {
        const [key, innerB] = lit.value[i];
        
        const envOne = bindOne(innerB, e[key]);
        if (envOne) {
          result = Object.assign(result, envOne);
        } else {
          return null;
        }
      }
      return result;
    case "ArrayLiteral":
      if (e.length !== lit.value.length) {
        return null;
      }
      var result = {};
      for (var i = 0; i < lit.value.length; i++) {
        const envOne = bindOne(lit.value[i], e[i]);
        if (envOne) {
          result = Object.assign(result, envOne);
        } else {
          return null;
        }
      }
      return result;
    case "IntLiteral":
    case "NumberLiteral":
    case "BooleanLiteral":
    case "StringLiteral":
    case "CharLiteral":
      return e == lit.value ? {} : null;
    default:
      throw "unknown literal type: " + lit.literalType;  
  }
}

const bindOne = (b, e) => {
  
  switch (b.binderType) {
    case "NullBinder":
      return {};
    case "LiteralBinder":
      return bindLiteral(b.literal, e);
    case "VarBinder":
      const env = {};
      env[b.identifier] = e;
      return env;
    case "NamedBinder":
      throw "Named binders are not supported yet"
    case "ConstructorBinder":
      throw "Constructor binders are not supported yet"
    default:
      throw "unknown binder type: " + b.binderType;  
  }
}

const bind = (binders, exprs) => {
  var result = {};
  for (var i = 0; i < binders.length; i++) {
    const b = binders[i];
    const e = exprs[i];
    const envOne = bindOne(b, e);
    if (envOne) {
      result = Object.assign(result, envOne);
    } else {
      return null;
    }
  }
  return result;
}

const eval = (expr, env) => {
  switch (expr.type) {
    case "Abs":
      return { 
        env, 
        arg: expr.argument, 
        body: expr.body,
        toJSON: () => '<closure>'
      };
    case "App":
      return apply(eval(expr.abstraction, env), eval(expr.argument, env));
    case "Literal":
      return lit(expr.value, env);
    case "Accessor": {
      const rec = eval(expr.expression, env);
      return rec[expr.fieldName];
    }
    case "Var": {
      if (!(expr.value.identifier in env)) {
        throw "unknown value: " + expr.value.identifier;
      }
      return env[expr.value.identifier];
    }
    case "Case": {
      const exprs = expr.caseExpressions.map((e) => eval(e, env));
      const alts = expr.caseAlternatives;
      for (var i = 0; i < alts.length; i++) {
        const bound = bind(alts[i].binders, exprs);
        if (bound) {
          return eval(alts[i].expression, Object.assign({}, env, bound));
        }
      }
      throw "Inexhaustive pattern match!"
    }
    case "Let": {
      var newEnv = env;
      for (var i = 0; i < expr.binds.length; i++) {
        const bind = expr.binds[i];
        switch (bind.bindType) {
          case "Rec":
            for (var j = 0; j < bind.binds.length; j++) {
              const { identifier, expression } = bind.binds[j];
              newEnv[identifier] = eval(expression, newEnv);
            }
            break;
          case "NonRec": {
            const { identifier, expression } = bind;
            newEnv[identifier] = eval(expression, newEnv);
          }
          default:
            throw "unknown bindType: " + bind.bindType;
        }
      }
      return eval(expr.expression, newEnv);
    }
    case "ObjectUpdate": {
      const rec = eval(expr.expression, env);
      const copy = Object.assign({}, rec);
      for (var i = 0; i < expr.updates.length; i++) {
        const [label, value] = expr.updates[i];
        copy[label] = eval(value, env);
      }
      return copy;
    }
    case "Constructor":
      throw "Data constructors are not supported yet"
    case "Prim":
      return expr.impl(env);
    default:
      throw "unknown expr type: " + expr.type;  
  }
};

const apply = ({env, arg, body}, val) => {
  env[arg] = val;
  return eval(body, env);
};

const primFn = (argName, f) => {
  return { 
    env: {}, 
    arg: argName, 
    body: 
      {
        type: "Prim",
        impl: (env) => f(env[argName], env)
      },
    toJSON: () => '<closure>'
  };
};

const initialEnv = 
  { map: primFn("f", (f) => 
      primFn("xs", (xs) => 
        xs.map((x) => apply(f, x))))
  , filter: primFn("p", (p) => 
      primFn("xs", (xs) => 
        xs.filter((x) => apply(p, x))))
  , append: primFn("xs", (xs) => 
      primFn("ys", (ys) => 
        [].concat(xs, ys)))
  , eq: primFn("a", (a) => 
      primFn("b", (b) => 
        a === b))
  };

const output = apply(apply(eval(decls["main"], initialEnv), null), input);
console.log(JSON.stringify(output, null, 2));
