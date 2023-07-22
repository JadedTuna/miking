include "ocaml/ast.mc"
include "mexpr/ast.mc"

lang ExternalsOCaml = OCamlExternal + ExternalsAst + MExprAst
  sem chooseConvFrom =
  -- String
  | TySeq {ty = TyChar _} -> lam a. strJoin " " ["(", "Boot.Intrinsics.Mseq.Helpers.to_utf8", a, ")"]
  -- List/Rope
  | TySeq {ty = t} -> lam a. strJoin " " ["(", "List.map", "( fun a -> ", (chooseConvFrom t) "a", ")", "(", "Boot.Intrinsics.Mseq.Helpers.to_list", a, ")", ")"]
  | TyInt _
  | TyFloat _
    -> lam a. a

  sem chooseConvTo =
  -- String
  | TySeq {ty = TyChar _} -> lam a. strJoin " " ["(", "Boot.Intrinsics.Mseq.Helpers.of_utf8", a, ")"]
  -- List/Rope
  | TySeq {ty = t} -> lam a. strJoin " " ["(", "Boot.Intrinsics.Mseq.Helpers.of_list", "(", "List.map", "( fun a -> ", (chooseConvTo t) "a", ")", a, ")", ")"]
  | TyInt _
  | TyFloat _
    -> lam a. a

  sem generate (env : GenerateEnv) =
  | TmExtBind t ->
    match tyTm (TmExtBind t) with ty then
      let types = sfold_Type_Type (lam acc. lam a. cons a acc) [] ty in
      match types with [rettype] ++ argtypes then
        let args = map (lam i. concat "a" (int2string i)) (create (length argtypes) (lam i. addi i 1)) in
        let begin_s = strJoin " " ["let __internal", strJoin " " args, "="] in
        let argconvs = map chooseConvFrom argtypes in
        let call_s = strJoin " " (map (lam ac. join ["(", (ac.1 ac.0), ")"]) (zip args argconvs)) in
        let unconv_body_s = strJoin " " ["(", "(", tmSeq2String t.e, ")", call_s, ")"] in
        let conv_ret_s = strJoin " " ["(", (chooseConvTo rettype) unconv_body_s, ")"] in
        let full_s = strJoin " " [begin_s, conv_ret_s, "in __internal"] in
          OTmExprExt { expr = full_s }
      else never
    else never
end
