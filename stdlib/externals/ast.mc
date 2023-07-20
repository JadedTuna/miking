include "mexpr/keyword-maker.mc"
include "mexpr/ast.mc"
include "mexpr/type-annot.mc"

lang ExternalsAst =
  KeywordMaker + PrettyPrint + SeqAst + ConstAst + CharAst + LamTypeAnnot
  syn Expr =
  | TmExtBind {e: Expr, tyExpr: Expr, deps: [String], info: Info}

  -- state that this is a keyword
  sem isKeyword =
  | TmExtBind _ -> true

  sem getDeps =
  | TmSeq t ->
    map tmSeq2String t.tms

  -- handle externalbind symbol as a keyword
  sem matchKeywordString (info: Info) =
  | "externalbind" ->
    Some (3, lam lst. TmExtBind {
      e = get lst 0,
      tyExpr = get lst 1,
      deps = getDeps (get lst 2),
      info = info
    })

  -- provide cases for convenience functions
  sem tyTm =
    -- this expects tyExpr to be a lambda, and returns the parameter's type
    -- e.g.: lam a : Float -> Float -> Float. ()
    -- this would return Float -> Float -> Float
    | TmExtBind {tyExpr = TmLam {ty = TyArrow {from = from}}} ->
      from

  sem infoTm =
  | TmExtBind t -> t.info

  sem withType (ty : Type) =
  | TmExtBind t -> TmExtBind {t with e = withType ty t.e}

  sem typeCheckExpr (env : TCEnv) =
  | TmExtBind t ->
    let e = typeCheckExpr env t.e in
    let tyExpr = typeCheckExpr env t.tyExpr in
    TmExtBind {t with e = e, tyExpr = tyExpr}

  sem typeAnnotExpr (env : TypeEnv) =
  | TmExtBind t ->
    let e = typeAnnotExpr env t.e in
    let tyExpr = typeAnnotExpr env t.tyExpr in
    TmExtBind {t with e = e, tyExpr = tyExpr}

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmExtBind t ->
    match printParen indent env t.e with (env, e) in
    (env, join ["externalbind", pprintNewline indent , e])

  sem tmSeq2String =
  | TmSeq t ->
    let extract_char = lam e.
      match e with TmConst {val = CChar c} then
        Some c.val
      else None ()
    in
    match optionMapM extract_char t.tms with Some str then
      str
    else never

  sem collectDeps env =
  | TmExtBind t ->
    collectDeps {env with deps = (setUnion (setOfSeq cmpString t.deps) env.deps)} t.e
  | t -> sfold_Expr_Expr (collectDeps) env t
end
