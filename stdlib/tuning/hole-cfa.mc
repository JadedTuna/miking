-- Uses 0-CFA analysis for analyzing data-flow of holes in an MExpr program. The
-- final output is the set of holes that (may) affect the execution time, for
-- each labelled subexpression.
--
-- There are currently two ways in which execution time dependency is inferred:
-- 1. In a match, if the _condition is data-dependent_ on hole `h`, then the
-- _execution time_ of the match is dependent on `h`.
-- 2. In the result of applying some intrinsic functions. For example, in
-- `sleepMs x`, if `x` is data-dependent, then the result of the application is
-- execution time dependent. The behaviour for each intrinsic function is
-- encoded in `const-dep.mc`.
--
-- Limitations:
-- * Some side-effects are not handled, e.g. parallelism and mutable data.
-- * Context depth of holes is not considered.

-- NOTE(Linnea, 2021-11-25): Currently, execution time dependency is not
-- propagated from a subexpression into its enclosing expression. For example:
-- ```
-- let h = hole (IntRange {default = 1, min = 1, max = 1}) in
-- let x =  -- { } ⊆ x
--   let y = sleepMs h in  -- { e(h) } ⊆ y
--   2
-- in
-- ()

include "name.mc"
include "common.mc"

include "decision-points.mc"
include "const-dep.mc"

include "mexpr/cfa.mc"
include "mexpr/const-arity.mc"
include "mexpr/symbolize.mc"

lang MExprHoleCFA = Holes + MExprCFA + MExprArity

  syn AbsVal =
  | AVDHole { id : Name }
  | AVEHole { id : Name }
  | AVConst { const : Const, args : [Name] }

  sem absValToString (env : PprintEnv) =
  | AVDHole {id = id} -> (env, join ["dhole", "(", nameGetStr id, ")"])
  | AVEHole {id = id} -> (env, join ["ehole", "(", nameGetStr id, ")"])
  | AVConst { const = const, args = args } ->
    let const = getConstStringCode 0 const in
    let args = strJoin ", " (map nameGetStr args) in
    (env, join [const, "(", args, ")"])

  sem cmpAbsValH =
  | (AVDHole {id = id1}, AVDHole {id = id2}) -> nameCmp id1 id2
  | (AVEHole {id = id1}, AVEHole {id = id2}) -> nameCmp id1 id2
  | (AVConst lhs, AVConst rhs) ->
    use ConstCmp in
    let cmp = cmpConst lhs.const rhs.const in
    if eqi 0 cmp then subi (length lhs.args) (length rhs.args)
    else cmp

  syn Constraint =
    -- {dhole} ⊆ lhs ⇒ {dhole} ⊆ rhs
  | CstrHoleDirect { lhs: Name, rhs: Name }
    -- {dhole} ⊆ lhs ⇒ ({dhole} ⊆ res) AND ({ehole} ⊆ res)
  | CstrHoleApp { lhs: Name, res: Name }
    -- ({const with args = args} ⊆ lhs AND |args| = arity(const)-1
    --    ⇒ ∀(a,i): (a,i) in ({args} ∪ {rhs} ⨯ [1,...,arity(const)]):
    --        if const is data dep on position i AND {dhole} ⊆ a ⇒ {dhole} ⊆ res
    --        AND
    --        if const is exe dep on position i AND {dhole} ⊆ a ⇒ {ehole} ⊆ res)
    -- AND
    -- ({const with args = args} ⊆ lhs AND |args| < arity(const)-1
    --    ⇒ {const with args = snoc args rhs } ⊆ res)
  | CstrConstApp { lhs: Name, rhs : Name, res: Name }
    -- {dhole} ⊆ lhs ⇒ {ehole} ⊆ res
  | CstrHoleMatch { lhs: Name, res: Name }

  sem initConstraint (graph : CFAGraph) =
  | CstrHoleApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrHoleDirect r & cstr -> initConstraintName r.lhs graph cstr
  | CstrConstApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrHoleMatch r & cstr -> initConstraintName r.lhs graph cstr

  sem propagateConstraint (update : (Name, AbsVal)) (graph : CFAGraph) =
  | CstrHoleDirect { lhs = lhs, rhs = rhs } ->
    match update.1 with AVDHole _ & av then addData graph av rhs else graph
  | CstrHoleApp { lhs = lhs, res = res } ->
    match update.1 with AVDHole {id = id} & av then
      let graph = addData graph av res in
      addData graph (AVEHole {id = id}) res
    else graph
  | CstrHoleMatch { lhs = lhs, res = res } ->
    match update.1 with AVDHole {id = id} then addData graph (AVEHole {id = id}) res else graph
  | CstrConstApp { lhs = lhs, rhs = rhs, res = res } ->
    use MExprConstDep in
    match update.1 with AVConst ({ const = const, args = args } & avc) then
      let arity = constArity const in
      let args = snoc args rhs in
      if eqi arity (length args) then
        -- Last application, analyse data and execution time
        let cdeps = constDep const in
        let graph = foldl (lam graph. lam argDep : (Name, (Bool, Bool)).
          let arg = argDep.0 in
          let dep = argDep.1 in
          let isDataDep = dep.0 in
          let isExeDep = dep.1 in
          let s = dataLookup arg graph in
          let avHoles : [Name] = setFold (lam acc. lam e.
            match e with AVDHole {id = id} then cons id acc
            else acc) [] s
          in
          -- Add data dependencies to the result
          let graph =
            if isDataDep then
              foldl (lam acc. lam id. addData acc (AVDHole {id=id}) res)
                graph avHoles
            else graph
          in
          -- Add execution time dependencies the result
          if isExeDep then
            foldl (lam acc. lam id. addData acc (AVEHole {id = id}) res)
              graph avHoles
          else graph) graph (zip args cdeps) in
        graph
      else
        -- Curried application, just add the new argument
        addData graph (AVConst { avc with args = args }) res
    else graph

  sem generateHoleConstraints =
  | _ -> []
    -- Holes
  | TmLet { ident = ident, body = TmHole _} ->
    [ CstrInit {lhs = AVDHole {id = ident}, rhs = ident } ]
  | TmLet { ident = ident, body = TmConst { val = c } } ->
    let arity = constArity c in
    if eqi arity 0 then []
    else [ CstrInit { lhs = AVConst { const = c, args = [] }, rhs = ident }
         ]
  | TmLet { ident = ident, body = TmApp app} ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar r then [
        CstrHoleApp { lhs = l.ident, res = ident},
        CstrConstApp { lhs = l.ident, rhs = r.ident, res = ident }
      ]
      else infoErrorExit (infoTm app.rhs) "Not a TmVar in application"
    else infoErrorExit (infoTm app.lhs) "Not a TmVar in application"

  sem constraintToString (env: PprintEnv) =
  | CstrHoleDirect { lhs = lhs, rhs = rhs } ->
    match pprintVarName env rhs with (env,rhs) in
    match pprintVarName env lhs with (env,lhs) in
    (env, join [ "{dhole} ⊆ ", lhs, " ⇒ {dhole} ⊆ ", rhs ])
  | CstrHoleApp { lhs = lhs, res = res } ->
    match pprintVarName env lhs with (env,lhs) in
    match pprintVarName env res with (env,res) in
    (env, join [
      "{dhole} ⊆ ", lhs, " ⇒ {dhole} ⊆ ", res ])
  | CstrHoleMatch { lhs = lhs, res = res } ->
    match pprintVarName env lhs with (env,lhs) in
    match pprintVarName env res with (env,res) in
    (env, join [
      "{dhole} ⊆ ", lhs, " ⇒ {ehole} ⊆ ", res ])
  | CstrConstApp { lhs = lhs, rhs = rhs, res = res } ->
    match pprintVarName env lhs with (env,lhs) in
    match pprintVarName env rhs with (env,rhs) in
    match pprintVarName env res with (env,res) in
    (env, join [
      "({const with args = args} ⊆ ", lhs, " AND |args| = arity(const)-1\n",
      "  ⇒ ∀(a,i): (a,i) in ({args} ∪ {", rhs, "} ⨯ [1,...,arity(const)]):\n",
      "    if const is data dep. on position i AND {dhole} ⊆ a ⇒ {dhole} ⊆ ", res, "\n",
      "    AND\n",
      "    if const is exe. dep. on position i AND {dhole} ⊆ a ⇒ {ehole} ⊆ ", res, ")\n",
      "AND\n",
      "({const with args = args} ⊆ ", lhs, " AND |args| < arity(const)-1\n",
      "  ⇒ {const with args = snoc args ", rhs, "} ⊆ ", res, ")"
    ])

  sem generateHoleMatchResConstraints (id: Name) (target: Name) =
  | ( PatSeqTot _
    | PatSeqEdge _
    | PatCon _
    | PatInt _
    | PatChar _
    | PatBool _
    ) & pat -> [
      CstrHoleDirect { lhs = target, rhs = id },
      CstrHoleMatch { lhs = target, res = id }
    ]
  | ( PatAnd p
    | PatOr p
    | PatNot p
    ) -> infoErrorExit p.info "Pattern currently not supported"
  | _ -> []

  sem generateHoleMatchConstraints (id: Name) (target: Name) =
  | pat ->
    recursive let f = lam acc. lam pat.
      let acc = match pat with PatNamed { ident = PName name }
                             | PatSeqEdge { middle = PName name }
                then cons name acc else acc in
      sfold_Pat_Pat f acc pat
    in
    let pnames = f [] pat in
    foldl (lam acc. lam name.
      cons (CstrHoleDirect { lhs = target, rhs = name }) acc
    ) [] pnames

  -- Type: Expr -> CFAGraph
  sem initGraph =
  | t ->

    -- Initial graph
    let graph = emptyCFAGraph in

    -- Initialize match constraint generating functions
    let graph = { graph with mcgfs = [ generateMatchConstraints
                                     , generateHoleMatchConstraints
                                     , generateHoleMatchResConstraints
                                     ] } in

    -- Initialize constraint generating functions
    let cgfs = [ generateConstraints
               , generateConstraintsMatch graph.mcgfs
               , generateHoleConstraints
               ] in

    -- Recurse over program and generate constraints
    let cstrs: [Constraint] = collectConstraints cgfs [] t in

    -- Initialize all collected constraints
    let graph = foldl initConstraint graph cstrs in

    -- Return graph
    graph

end

lang Test = MExprHoleCFA + BootParser + MExprANFAll + MExprSym

mexpr
use Test in

-- Test functions --
let debug = false in
let parse = lam str.
  let ast = parseMExprString decisionPointsKeywords str in
  let ast = makeKeywords [] ast in
  symbolize ast
in
let test: Bool -> Expr -> [String] -> [[AbsVal]] =
  lam debug: Bool. lam t: Expr. lam vars: [String].
    if debug then
      -- Version with debug printouts
      let tANF = normalizeTerm t in
      match pprintCode 0 pprintEnvEmpty t with (_,tStr) in
      printLn "\n--- ORIGINAL PROGRAM ---";
      printLn tStr;
      match pprintCode 0 pprintEnvEmpty tANF with (env,tANFStr) in
      printLn "\n--- ANF ---";
      printLn tANFStr;
      match cfaDebug (Some env) tANF with (Some env,cfaRes) in
      match cfaGraphToString env cfaRes with (_, resStr) in
      printLn "\n--- FINAL CFA GRAPH ---";
      printLn resStr;
      let cfaRes : CFAGraph = cfaRes in
      map (lam var: String.
        let binds = mapBindings cfaRes.data in
        let res = foldl (lam acc. lam b : (Name, Set AbsVal).
          if eqString var (nameGetStr b.0) then b.1 else acc
        ) (setEmpty cmpAbsVal) binds in
        (var, res)
      ) vars

    else
      -- Version without debug printouts
      let tANF = normalizeTerm t in
      let cfaRes : CFAGraph = cfa tANF in
      map (lam var: String.
        let binds = mapBindings cfaRes.data in
        let res = foldl (lam acc. lam b : (Name, Set AbsVal).
          if eqString var (nameGetStr b.0) then b.1 else acc
        ) (setEmpty cmpAbsVal) binds in
        (var, res)
      ) vars
in

-- Custom equality function for testing lambda control flow only
type Dep = {d: [String], e: [String]} in
let eqTestHole = eqSeq (lam t1:(String,Set AbsVal). lam t2:(String,Dep).
  if eqString t1.0 t2.0 then
    let data = setFold (lam acc. lam av.
      match av with AVDHole {id = id} then cons (nameGetStr id) acc else acc)
      [] t1.1
    in
    let exe = setFold (lam acc. lam av.
      match av with AVEHole {id = id} then cons (nameGetStr id) acc else acc)
      [] t1.1
    in
    let deps : Dep = t2.1 in
    if setEq (setOfSeq cmpString data) (setOfSeq cmpString deps.d) then
      setEq (setOfSeq cmpString exe) (setOfSeq cmpString deps.e)
    else false
  else false
) in
--------------------

let t = parse
"
let h1 = hole (Boolean {default = true}) in
let h2 = hole (Boolean {default = true}) in
let x = h1 in
let y = h2 in
x
"
in

utest test debug t ["h1", "h2", "x", "y"]
with [("h1", {d = ["h1"], e = []}),("h2", {d = ["h2"], e = []}),("x", {d = ["h1"], e = []}),("y", {d = ["h2"], e = []})]
using eqTestHole in


let t = parse
"
let foo = lam.
  let h = hole (Boolean {default = true}) in
  h
in
let x = foo () in x
"
in

utest test debug t ["x"]
with [("x", {d = ["h"], e = []})]
using eqTestHole in


let t = parse
"
let foo = lam x.
  x
in
let h = hole (Boolean {default = true}) in
let y = foo h in y
"
in

utest test debug t ["x", "y"]
with [("x", {d = ["h"], e = []}), ("y", {d = ["h"], e = []})]
using eqTestHole in


let t = parse
"
let foo = lam x.
  let h = hole (Boolean {default = true}) in
  2
in
let h1 = hole (Boolean {default = true}) in
let y = foo 3 in
let z = foo h1 in
y
"
in

utest test debug t ["y", "z"]
with [("y", {d = [], e = []}), ("z", {d = [], e = []})]
using eqTestHole in


let t = parse
"
let h = hole (Boolean {default = true}) in
let x = if h then 1 else 2 in
let y = if true then 1 else 2 in
let z = if true then h else 2 in
let a = match h with h1 then true else false in
let b = match h with h1 then h1 else false in
x
"
in

utest test debug t ["x", "y", "z", "a", "b", "h"]
with [ ("x", {d=["h"],e=["h"]})
     , ("y", {d=[],e=[]})
     , ("z", {d=["h"],e=[]})
     , ("a", {d = [], e = []})
     , ("b", {d=["h"],e=[]})
     , ("h", {d=["h"],e=[]})
     ]
using eqTestHole in


let t = parse
"
let h = hole (Boolean {default = true}) in
let f = if h then lam x. x else lam x. x in
let a = f 1 in
a
"
in

utest test debug t ["f", "a"]
with [ ("f", {d=["h"],e=["h"]})
     , ("a", {d=["h"],e=["h"]})
     ]
using eqTestHole in


let t = parse
"
let h1 = hole (IntRange {default = 1, min = 0, max = 1}) in
let h2 = hole (IntRange {default = 1, min = 0, max = 1}) in
let x = negi h1 in
let y = addi 3 x in
let y2 = addi h1 h2 in
let z = sleepMs y in
x
"
in

utest test false t ["x", "y", "z", "y2"]
with [ ("x", {d=["h1"],e=[]})
     , ("y", {d=["h1"],e=[]})
     , ("z", {d=[],e=["h1"]})
     , ("y2",{d=["h1", "h2"],e=[]})
     ]
using eqTestHole in


let t = parse
"
let h = hole (Boolean {default = true}) in
let r = {x = h, y = 2} in
let a = r.x in
let b = if r.x then true else false in
let c = match r with {x = x, y = 2} then 2 else 42 in
let d = match r with {x = true} then 2 else 42 in
let e = r.y in
()
"
in

utest test debug t ["a", "b", "c", "d", "e"]
with [ ("a", {d=["h"],e=[]})
     , ("b", {d=["h"],e=["h"]})
     , ("c", {d=[],e=[]})
     , ("d", {d=["h"],e=["h"]})
     , ("e", {d=[],e=[]})
     ]
using eqTestHole in


let t = parse
"
let f = lam y. if y then 1 else 2 in
let h = hole (Boolean {default = true}) in
let x = f h in
x
"
in

utest test debug t ["x"]
with [ ("x", {d=["h"],e=["h"]}) ]
using eqTestHole in


let t = parse
"
let h1 = hole (IntRange {default = 1, min = 1, max = 2}) in
let h2 = hole (IntRange {default = 1, min = 1, max = 2}) in
let h3 = hole (IntRange {default = 1, min = 1, max = 2}) in

let f = lam x. lam y.
  addi x y
in
let a = f h1 1 in
let b = f 1 h2 in
let c = f h1 h2 in
let d = f h2 h3 in
()
" in

utest test debug t ["a", "b", "c", "d"]
with [ ("a",{d=["h1","h2","h3"],e=[]})
     , ("b",{d=["h1","h2","h3"],e=[]})
     , ("c",{d=["h1","h2","h3"],e=[]})
     , ("d",{d=["h1","h2","h3"],e=[]})
     ]
using eqTestHole in


let t = parse
"
-- Direct data-flow --
let h = hole (Boolean {default = true}) in
let x1 = h in -- { d(h) } ⊆ x1

-- Applications --
let f1 = lam x. x in
let x2 = f1 h in -- { d(h) } ⊆ x2
-- Limitation of 0-CFA: x22 gets d(h) because of above application
let x22 = f1 3 in -- { d(h) } ⊆ x22

let f2 = lam x. 3 in
let x3 = f2 h in -- { } ⊆ x3

-- Matches --
let r = {a = h, b = false} in
-- x4 is both data and execution time dependent. In a more exact analysis,
-- it should only be data dependent since the two branches have equal execution time.
let x4 = match r with {a = true} then 1 else 2 in -- { d(h), e(h) ⊆ x4 }
-- x5 is not dependent on h since the match can never fail
let x5 = match r with {a = a, b = false} then 1 else 2 in -- { } ⊆ x5

let f = lam x. if x then 1 else 2 in
let g = lam f. f h in
let x6 = g f in -- { d(h), e(h) } ⊆ x6

let f = if h then lam x. x else lam y. y in
-- Similar for x4, x0 should not have e(h).
-- Possibly, we could detect that it shouldn't have d(h) either.
let x0 = f 1 in  -- { d(h), e(h) } ⊆ x0

-- Constants --
let h1 = hole (IntRange {default = 1, min = 1, max = 2}) in
let h2 = hole (IntRange {default = 1, min = 1, max = 2}) in

let x7 = addi 1 h1 in  -- { d(h1) } ⊆ x7
let x8 = addi 1 h2 in -- { d(h2) } ⊆ x8
let x9 = addi h1 h2 in  -- { d(h1), d(h2) } ⊆ x9

()
" in
utest test debug t ["x1", "x2", "x22", "x3", "x4", "x5", "x6", "x0", "x7", "x8", "x9"]
with [ ("x1", {d=["h"],e=[]})
     , ("x2", {d=["h"],e=[]})
     , ("x22", {d=["h"],e=[]})
     , ("x3", {d=[],e=[]})
     , ("x4", {d=["h"],e=["h"]})
     , ("x5", {d=[],e=[]})
     , ("x6", {d=["h"],e=["h"]})
     , ("x0", {d=["h"],e=["h"]})
     , ("x7", {d=["h1"],e=[]})
     , ("x8", {d=["h2"],e=[]})
     , ("x9", {d=["h1", "h2"],e=[]})
     ]
using eqTestHole
in

let t = parse
"
let h = hole (IntRange {default = 1, min = 1, max = 1}) in
let x =
  let y = sleepMs h in
  2
in
x
" in

utest test debug t ["x", "y"]
with [ ("x", {d=[], e=[]})
     , ("y", {d=[], e=["h"]})
     ]
using eqTestHole
in

-- TODO(Linnea,2021-11-22): test sequences, maps

()