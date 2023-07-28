include "ocaml/ast.mc"
include "mexpr/ast.mc"

include "jit.mc"

lang ExternalsOCaml = OCamlExternal + ExternalsAst + MExprAst
  sem chooseConvFrom =
  | _ -> lam a. a
  -- -- String
  -- | TySeq {ty = TyChar _} -> lam a. strJoin " " ["(", "Boot.Intrinsics.Mseq.Helpers.to_utf8", a, ")"]
  -- -- List/Rope
  -- | TySeq {ty = t} -> lam a. strJoin " " ["(", "List.map", "( fun a -> ", (chooseConvFrom t) "a", ")", "(", "Boot.Intrinsics.Mseq.Helpers.to_list", a, ")", ")"]
  -- | TyRecord {fields = f} ->
  --   if mapIsEmpty f then lam a. a
  --   else error "records are not supported in externals"
  -- | TyInt _
  -- | TyFloat _
  -- | TyBool _
  --   -> lam a. a

  sem chooseConvTo =
  | _ -> lam a. a
  -- -- String
  -- | TySeq {ty = TyChar _} -> lam a. strJoin " " ["(", "Boot.Intrinsics.Mseq.Helpers.of_utf8", a, ")"]
  -- -- List/Rope
  -- | TySeq {ty = t} -> lam a. strJoin " " ["(", "Boot.Intrinsics.Mseq.Helpers.of_list", "(", "List.map", "( fun a -> ", (chooseConvTo t) "a", ")", a, ")", ")"]
  -- -- Unit
  -- | TyRecord {fields = f} ->
  --   if mapIsEmpty f then lam a. a
  --   else error "records are not supported in externals"
  -- | TyInt _
  -- | TyFloat _
  -- | TyBool _
  --   -> lam a. a

  sem generate (env : GenerateEnv) =
  | TmExtBind t ->
    match tyTm (TmExtBind t) with ty then
      recursive let tmp =
        lam acc. lam ty.
        match ty with TyArrow {from = from, to = to} then
          cons from (tmp acc to)
        else cons ty acc
      in
      let types = tmp [] ty in
      match types with [rettype] ++ argtypes then
        let args = map (lam i. concat "a" (int2string i)) (create (length argtypes) (lam i. addi i 1)) in
        let begin_s = strJoin " " ["fun", strJoin " " args, "->"] in
        let argconvs = map chooseConvFrom argtypes in
        let call_s = strJoin " " (map (lam ac. join ["(", (ac.1 ac.0), ")"]) (zip args argconvs)) in
        let unconv_body_s = strJoin " " ["(", "(", tmSeq2String t.e, ")", call_s, ")"] in
        let conv_ret_s = strJoin " " ["(", (chooseConvTo rettype) unconv_body_s, ")"] in
        let full_s = strJoin " " [begin_s, conv_ret_s] in
          OTmExprExt { expr = full_s }
      else never
    else never

  sem collectExternals acc =
  | TmExtBind t ->
    collectExternals (cons (TmExtBind t) acc) t.e
  | t -> sfold_Expr_Expr collectExternals acc t

  sem dummyValue =
  | TyFloat _ -> float_ 0.0
  | TyInt _ -> int_ 0
  | TyChar _ -> char_ '_'
  | TyBool _ -> bool_ false
  | TySeq {ty = ty} -> use MExprAst in
    TmSeq {tms = [dummyValue ty], ty = tyseq_ ty, info = NoInfo ()}
  | TyRecord {fields = {root = avlEmpty}} -> unit_

  sem wrapInLams partial d =
  | TyArrow {from = from, to = to} ->
    lam_ (int2string d) from (wrapInLams partial (addi d 1) to)
  | ty -> partial
      (
      seq_
        (map unsafeCoerce_
          (snoc (map var_
            (create d int2string)
          ) (dummyValue ty))
        )
      )

  sem replaceTmExtBind acc =
  | TmExtBind tm ->
    let ty = tyTm (TmExtBind tm) in
    let name2string = lam n.
      match nameGetSym n with Some sym then
        join [nameGetStr n, "_", int2string (sym2hash sym)]
      else never
    in
    let name = name2string (nameSym "mexpr_jit") in
    let partial = callExternal_ (str_ name) in
    let wrapped = wrapInLams partial 0 ty in
    (cons (name, TmExtBind tm) acc, wrapped)
  | tm -> smapAccumL_Expr_Expr replaceTmExtBind acc tm

  sem findReplaceExternals =
  | ast -> smapAccumL_Expr_Expr replaceTmExtBind [] ast

  sem jitCompileAllExternals =
  | ast ->
    match findReplaceExternals ast with (externals, ast) in
    jitCompile externals;
    ast
end
