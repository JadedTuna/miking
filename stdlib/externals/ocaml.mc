include "ocaml/ast.mc"
include "ast.mc"

lang ExternalsOCaml = OCamlExternal + ExternalsAst + SeqTypeAst
  sem chooseConvFrom =
  -- list of lists
  | TySeq {ty = TySeq t} -> lam a. strJoin " " ["(", "List.map", "( fun a -> ", (chooseConvFrom (TySeq t)) "a", ")", "(", "Boot.Intrinsics.Mseq.Helpers.to_list", a, ")", ")"]
  -- list of base types
  | TySeq _ -> lam a. strJoin " " ["(", "Boot.Intrinsics.Mseq.Helpers.to_list", a, ")"]

  sem chooseConvTo =
  | TySeq _ -> lam a. strJoin " " ["(", "Boot.Intrinsics.Mseq.Helpers.of_list", a, ")"]

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
