include "ocaml/ast.mc"
include "ast.mc"

lang ExternalsOCaml = OCamlExternal + ExternalsAst
  sem generate (env : GenerateEnv) =
  --| TmExtBind t -> printLn "-- NOTICE --"; printLn (expr2str t.e); (generate env t.e)
  --| TmExtBind t -> OTmExprExt { expr = (join t.e) }
  | TmExtBind t -> OTmExprExt { expr = (tmSeq2String t.e) }
  --| TmExtBind t -> OTmExprExt { expr = (expr2str t.e) }
end
