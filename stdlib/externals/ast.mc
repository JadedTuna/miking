include "mexpr/keyword-maker.mc"
include "mexpr/ast.mc"
include "mexpr/type-annot.mc"
include "stringid.mc"

include "ocaml/ast.mc"

lang ExternalsAst =
  KeywordMaker + PrettyPrint + SeqAst + ConstAst + CharAst + LamTypeAnnot + RecordAst
  syn Expr =
  | TmExtBind {e: Expr, ty: Type, deps: [String], info: Info}

  -- state that this is a keyword
  sem isKeyword =
  | TmExtBind _ -> true

  sem getDeps =
  | TmSeq t ->
    map tmSeq2String t.tms

  -- handle externalbind symbol as a keyword
  sem matchKeywordString (info: Info) =
  | "externalbind" ->
    Some (1, lam lst.
      match (get lst 0) with TmRecord {bindings = b} then
        let e = mapFindExn (stringToSid "expr") b in
        -- this expects type_ to be a lambda, and returns the parameter's type
        -- e.g.: lam a : Float -> Float -> Float. ()
        -- this would give Float -> Float -> Float
        match (mapFindExn (stringToSid "type_") b) with TmLam {tyAnnot = ty} in
        let deps = mapFindExn (stringToSid "deps") b in
          TmExtBind {
            e = e,
            ty = ty,
            deps = getDeps deps,
            info = info
          }
      else never
    )

  -- provide cases for convenience functions
  sem tyTm =
    | TmExtBind {ty = ty} -> ty

  sem infoTm =
  | TmExtBind t -> t.info

  sem withType (ty : Type) =
  | TmExtBind t -> TmExtBind {t with e = withType ty t.e}

  sem typeCheckExpr (env : TCEnv) =
  | TmExtBind t ->
    let e = typeCheckExpr env t.e in
    TmExtBind {t with e = e}

  sem typeAnnotExpr (env : TypeEnv) =
  | TmExtBind t ->
    let e = typeAnnotExpr env t.e in
    TmExtBind {t with e = e}

  sem smapAccumL_Expr_Expr f acc =
  | TmExtBind t ->
    match f acc t.e with (acc, e) in
      (acc, TmExtBind {t with e = e})

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
