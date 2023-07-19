include "mexpr/keyword-maker.mc"
include "mexpr/ast.mc"

lang ExternalsAst =
  KeywordMaker + PrettyPrint + SeqAst + ConstAst + CharAst
  syn Expr =
  | TmExtBind {e: Expr, ty: Type, info: Info}

  -- state that this is a keyword
  sem isKeyword =
  | TmExtBind _ -> true

  sem str2type =
  | "float" -> tyfloat_
  | "int" -> tyint_
  | "unit" -> tyunit_

  sem temporary =
  | TmSeq t ->
    let types = (map (lam a. str2type (tmSeq2String a)) t.tms) in
    match splitAt types (subi (length types) 1) with (argtypes, rettype) then
      foldr (lam a. lam b. tyarrow_ a b) (get rettype 0) argtypes
    else never

  -- handle externalbind symbol as a keyword
  sem matchKeywordString (info: Info) =
  | "externalbind" ->
    Some (2, lam lst. TmExtBind {
      e = get lst 0,
      ty = temporary (get lst 1),
      info = info
    })

  -- provide cases for convenience functions
  -- TODO: provide an actual type
  sem tyTm =
  --| TmExtBind t -> tyTm t.e
  | TmExtBind t -> t.ty

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

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmExtBind t ->
    match printParen indent env t.e with (env, e) in
    (env, join ["externalbind", pprintNewline indent , e])

  sem tmSeq2String =
  | TmSeq t ->
    let extract_char = lam e.
      match e with TmConst t1 then
        match t1.val with CChar c then
          Some c.val
        else None ()
      else None ()
    in
    match optionMapM extract_char t.tms with Some str then
      str
    else never
end
