// L2-eval-box.ts
// L2 with mutation (set!) and env-box model
// Direct evaluation of letrec with mutation, define supports mutual recursion.

import { map, range, reduce, repeat, zipWith } from "ramda";
import {
    isBoolExp, isCExp, isLitExp, isNumExp, isPrimOp, isStrExp, isVarRef,  
    isAppExp, isDefineExp, isIfExp, isLetExp, isProcExp, Binding, VarDecl, CExp, Exp, IfExp, LetExp, ProcExp, Program, SetExp, 
    parseL21Exp, DefineExp, isSetExp
} from "./L21-ast";
import { applyEnv, makeExtEnv, Env, Store, setStore, extendStore, ExtEnv , theGlobalEnv, globalEnvAddBinding, theStore, applyStore } from "./L21-env-store";
import { isClosure, makeClosure, Closure, Value } from "./L21-value-store";
import { applyPrimitive } from "./evalPrimitive-store";
import { first, rest, isEmpty } from "../shared/list";
import { Result, bind, safe2, mapResult, makeFailure, makeOk } from "../shared/result";
import { parse as p } from "../shared/parser";
import { unbox } from "../shared/box";

// ========================================================
// Eval functions

const applicativeEval = (exp: CExp, env: Env): Result<Value> =>
    isNumExp(exp) ? makeOk(exp.val) :
    isBoolExp(exp) ? makeOk(exp.val) :
    isStrExp(exp) ? makeOk(exp.val) :
    isPrimOp(exp) ? makeOk(exp) :
    isVarRef(exp) ? bind(applyEnv(env, exp.var), (address : number) =>applyStore(theStore, address)) :
    isLitExp(exp) ? makeOk(exp.val as Value) :
    isIfExp(exp) ? evalIf(exp, env) :
    isProcExp(exp) ? evalProc(exp, env) :
    isLetExp(exp) ? evalLet(exp, env) :
    isAppExp(exp) ? safe2((proc: Value, args: Value[]) => applyProcedure(proc, args))
                        (applicativeEval(exp.rator, env), mapResult((rand: CExp) => applicativeEval(rand, env), exp.rands)) :
    isSetExp(exp) ? evalSet(exp, env) :
    exp;

export const isTrueValue = (x: Value): boolean =>
    !(x === false);

const evalIf = (exp: IfExp, env: Env): Result<Value> =>
    bind(applicativeEval(exp.test, env),
        (test: Value) => isTrueValue(test) ? applicativeEval(exp.then, env) : applicativeEval(exp.alt, env));

const evalProc = (exp: ProcExp, env: Env): Result<Closure> =>
    makeOk(makeClosure(exp.args, exp.body, env));

// KEY: This procedure does NOT have an env parameter.
//      Instead we use the env of the closure.
const applyProcedure = (proc: Value, args: Value[]): Result<Value> =>
    isPrimOp(proc) ? applyPrimitive(proc, args) :
        isClosure(proc) ? applyClosure(proc, args) :
            makeFailure(`Bad procedure ${JSON.stringify(proc)}`);

const applyClosure = (proc: Closure, args: Value[]): Result<Value> => {
    const vars = map((v: VarDecl) => v.var, proc.params);
    map((val : Value) =>extendStore(theStore,val),args) // added each val to store
    const address_length = unbox(theStore.vals).length
    const addresses = range(address_length-args.length, address_length)
    const newEnv: ExtEnv = makeExtEnv(vars, addresses, proc.env)
    return evalSequence(proc.body, newEnv);
}

// Evaluate a sequence of expressions (in a program)
export const evalSequence = (seq: Exp[], env: Env): Result<Value> =>
    isEmpty(seq) ? makeFailure("Empty program") :
        evalCExps(first(seq), rest(seq), env);

const evalCExps = (first: Exp, rest: Exp[], env: Env): Result<Value> =>
    isDefineExp(first) ? evalDefineExps(first, rest, env) :
        isCExp(first) && isEmpty(rest) ? applicativeEval(first, env) :
            isCExp(first) ? bind(applicativeEval(first, env), _ => evalSequence(rest, env)) :
                first;


const evalSet = (exp: SetExp, env: Env): Result<void> =>
    safe2((val: Value, address: number) => makeOk(setStore(theStore, address, val)))
        (applicativeEval(exp.val, env), applyEnv(env, exp.var.var)); // maybe works, maybe not?? 

const evalDef = (def: DefineExp,exps: Exp[], env: Env): Result<Value> => {
    const address = unbox(theStore.vals).length
    globalEnvAddBinding(def.var.var, address) // at first we extended env instead of define var in global env
    extendStore(theStore, undefined) // added to store as undefine, to keep the sequence of addresses correct
    // const extEnv = makeExtEnv([def.var.var], [unbox(theStore.vals).length], env);
    bind(applicativeEval(def.val, env), (rhs: Value) => makeOk(setStore(theStore,address, rhs)));
    return evalSequence(exps, theGlobalEnv);
}

const evalDefineExps = (def: DefineExp, exps: Exp[], env : Env): Result<Value> =>
    isDefineExp(def) ? evalDef(def, exps, env):
    makeFailure("Unexpected " + def);

// Main program
// L2-BOX @@ Use GE instead of empty-env
export const evalProgram = (program: Program): Result<Value> =>
    evalSequence(program.exps, theGlobalEnv);

export const evalParse = (s: string): Result<Value> =>
    bind(bind(p(s), parseL21Exp), (exp: Exp) => evalSequence([exp], theGlobalEnv));

// LET: Direct evaluation rule without syntax expansion
// compute the values, extend the env, eval the body.
const evalLet = (exp: LetExp, env: Env): Result<Value> => {
    const vals = mapResult((v: CExp) => applicativeEval(v, env), map((b: Binding) => b.val, exp.bindings));
    const vars = map((b: Binding) => b.var.var, exp.bindings);


    return bind(vals, (vals: Value[]) => {
        map((val : Value) =>extendStore(theStore,val),vals) // added each val to store
        const address_length = unbox(theStore.vals).length
        const addresses = range(address_length-vals.length, address_length) // fixed addressess by theStore and not by global env
        const newEnv = makeExtEnv(vars, addresses, env)
        return evalSequence(exp.body, newEnv);
    })
}
