lang ExternalsAst =
  --KeywordMaker + MExprAst + MExprParser +
  --MExprPrettyPrint +
  --MExprSym
  --+ MExprEq + Eval +
  --PrettyPrint +
  --MExprTypeCheck + LamEval + OCamlTypePrettyPrint
  KeywordMaker + PrettyPrint + MExpr + MExprEq
  syn Expr =
  | TmExtBind {e: Expr, info: Info}

  -- state that this is a keyword
  sem isKeyword =
  | TmExtBind _ -> true

  -- handle externalbind symbol as a keyword
  sem matchKeywordString (info: Info) =
  | "externalbind" -> Some (1, lam lst. TmExtBind {e = get lst 0, info = info})

  -- provide cases for convenience functions
  -- TODO: provide an actual type
  sem tyTm =
  | TmExtBind t -> tyTm t.e

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

  sem pprintCode (indent : Int) (env : PprintEnv) =
  | TmExtBind t ->
    match printParen indent env t.e with (env, e) in
    (env, join ["externalbind", pprintNewline indent, e])

  sem smapAccumL_Expr_Expr f acc =
  | TmExtBind t ->
    match f acc t.e with (acc, e) in
    (acc, TmExtBind {t with e = e})

  -- Equality of the new terms
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmExtBind r ->
    match lhs with TmExtBind l then
      eqExprH env free l.e r.e
    else None ()
end
