-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--

include "ast.mc"
include "eq.mc"
include "info.mc"
include "pprint.mc"
include "const-transformer.mc"
include "string.mc"
include "stringid.mc"
include "builtin.mc"
include "seq.mc"
include "name.mc"

let gstr = lam t. lam n. bootParserGetString t n
let gname = lam t. lam n. nameNoSym (bootParserGetString t n)
let gint = lam t. lam n. bootParserGetInt t n
let gfloat = lam t. lam n. bootParserGetFloat t n
let glistlen = lam t. lam n. bootParserGetListLength t n

lang BootParser = MExprAst + ConstTransformer

  type BootParserParseMCoreFileArg = {
    -- Should we keep utests
    keepUtests : Bool,

    -- Prune external dependent utests
    pruneExternalUtests : Bool,

    -- Warn on pruned external dependent utests
    pruneExternalUtestsWarning : Bool,

    -- Run dead code elimination
    eliminateDeadCode : Bool,

    -- Exclude pruning of utest for externals with whose dependencies are met on
    -- this system.
    externalsExclude : [String],

    -- Additional keywords
    keywords : [String],

    -- Allow free variables
    allowFree : Bool,

    -- The builtins to replace with constants
    builtin : [(String,Const)]
  }

  type BootParserParseMExprStringArg = {

    -- Additional keywords
    keywords : [String],

    -- Allow free variables
    allowFree : Bool,

    -- The builtins to replace with constants
    builtin : [(String,Const)]
  }

  sem _defaultBootParserParseMCoreFileArg : () -> BootParserParseMCoreFileArg
  sem _defaultBootParserParseMCoreFileArg =
  | _ -> {
    keepUtests = true,
    pruneExternalUtests = false,
    pruneExternalUtestsWarning = true,
    eliminateDeadCode = true,
    externalsExclude = [],
    keywords = [],
    allowFree = false,
    builtin = builtin
  }

  sem _defaultBootParserParseMExprStringArg : () -> BootParserParseMExprStringArg
  sem _defaultBootParserParseMExprStringArg =
  | _ -> {
    keywords = [],
    allowFree = false,
    builtin = builtin
  }

  -- Parse a complete MCore file, including MLang code
  -- This function returns the final MExpr AST. The MCore
  -- file can refer to other files using include statements
  sem parseMCoreFile (arg : BootParserParseMCoreFileArg) =
  | filename ->
    let t =
      bootParserParseMCoreFile
        (
          arg.keepUtests,
          arg.pruneExternalUtests,
          arg.externalsExclude,
          arg.pruneExternalUtestsWarning,
          arg.eliminateDeadCode,
          arg.allowFree
        )
        arg.keywords
        filename
    in
    constTransform arg.builtin (matchTerm t (bootParserGetId t))

  -- Parses an MExpr string and returns the final MExpr AST
  sem parseMExprString (arg : BootParserParseMExprStringArg) =
  | str ->
    let t =
      bootParserParseMExprString
        (
          arg.allowFree,
        )
        arg.keywords
        str
    in
    constTransform arg.builtin (matchTerm t (bootParserGetId t))

  sem parseMExprStringKeywords (keywords : [String]) =
  | str ->
    parseMExprString
      { _defaultBootParserParseMExprStringArg () with keywords = keywords }
      str

  -- Get term help function
  sem gterm (t:Unknown) =
  | n -> let t2 = bootParserGetTerm t n in
         matchTerm t2 (bootParserGetId t2)

  -- Match term from ID
  sem matchTerm (t:Unknown) =
  | 100 /-TmVar-/ ->
    TmVar {ident = gname t 0,
           ty = TyUnknown { info = ginfo t 0 },
           info = ginfo t 0,
           frozen = neqi (gint t 0) 0}
  | 101 /-TmApp-/ ->
    TmApp {lhs = gterm t 0,
           rhs = gterm t 1,
           ty = TyUnknown { info = ginfo t 0 },
           info = ginfo t 0}
  | 102 /-TmLam-/ ->
    TmLam {ident = gname t 0,
           tyAnnot = gtype t 0,
           tyParam = TyUnknown { info = ginfo t 0 },
           ty = TyUnknown { info = ginfo t 0 },
           info = ginfo t 0,
           body = gterm t 0}
  | 103 /-TmLet-/ ->
    TmLet {ident = gname t 0,
           tyAnnot = gtype t 0,
           tyBody = TyUnknown { info = ginfo t 0 },
           body = gterm t 0,
           inexpr = gterm t 1,
           ty = TyUnknown { info = ginfo t 0 },
           info = ginfo t 0}
  | 104 /-TmRecLets-/ ->
    TmRecLets {bindings =
               create (glistlen t 0)
                      (lam n. {ident = gname t n,
                               tyAnnot = gtype t n,
                               tyBody = TyUnknown { info = ginfo t (addi n 1)},
                               body = gterm t n,
                               info = ginfo t (addi n 1)}),
               inexpr = gterm t (glistlen t 0),
               ty = TyUnknown { info = ginfo t 0 },
               info = ginfo t 0}
  | 105 /-TmConst-/ ->
    let c = gconst t 0 in
    TmConst {val = gconst t 0,
             ty = TyUnknown { info = ginfo t 0 },
             info = ginfo t 0}
  | 106 /-TmSeq-/ ->
    TmSeq {tms = create (glistlen t 0) (lam n. gterm t n),
           ty =  TyUnknown { info = ginfo t 0 },
           info = ginfo t 0}
  | 107 /-TmRecord-/ ->
    let lst = create (glistlen t 0) (lam n. (gstr t n, gterm t n)) in
    TmRecord {bindings =
                mapFromSeq cmpSID
                  (map (lam b : (String,Expr). (stringToSid b.0, b.1)) lst),
              ty = TyUnknown { info = ginfo t 0 },
              info = ginfo t 0}
  | 108 /-TmRecordUpdate-/ ->
    TmRecordUpdate {rec = gterm t 0,
                   key = stringToSid (gstr t 0),
                   value = gterm t 1,
                   ty = TyUnknown { info = ginfo t 0 },
                   info = ginfo t 0}
  | 109 /-TmType-/ ->
    TmType {ident = gname t 0,
            params = map (gname t) (range 1 (glistlen t 0) 1),
            tyIdent = gtype t 0,
            ty = TyUnknown { info = ginfo t 0 },
            inexpr = gterm t 0,
            info = ginfo t 0}
  | 110 /-TmConDef-/ ->
    TmConDef {ident = gname t 0,
              tyIdent = gtype t 0,
              ty = TyUnknown { info = ginfo t 0 },
              inexpr = gterm t 0,
              info = ginfo t 0}
  | 111 /-TmConApp-/ ->
    TmConApp {ident = gname t 0,
              body = gterm t 0,
              ty = TyUnknown { info = ginfo t 0 },
              info = ginfo t 0}
  | 112 /-TmMatch-/ ->
    TmMatch {target = gterm t 0,
             pat = gpat t 0,
             thn = gterm t 1,
             els = gterm t 2,
             ty = TyUnknown { info = ginfo t 0 },
             info = ginfo t 0}
  | 113 /-TmUtest-/ ->
    let tusing = match (glistlen t 0) with 4 then
                   Some (gterm t 3)
                 else None () in
    TmUtest {test = gterm t 0,
             expected = gterm t 1,
             next = gterm t 2,
             tusing = tusing,
             ty = TyUnknown { info = ginfo t 0 },
             info = ginfo t 0}
  | 114 /-TmNever-/ ->
    TmNever {ty = TyUnknown { info = ginfo t 0 },
             info = ginfo t 0}
  | 115 /-TmExt-/ ->
    TmExt {ident = gname t 0,
           tyIdent = gtype t 0,
           effect = neqi (gint t 0) 0,
           ty = TyUnknown { info = ginfo t 0 },
           inexpr = gterm t 0,
           info = ginfo t 0}

  -- Get type help function
  sem gtype(t:Unknown) =
  | n -> let t2 = bootParserGetType t n in
        matchType t2 (bootParserGetId t2)

  sem matchType (t:Unknown) =
  | 200 /-TyUnknown-/ ->
    TyUnknown {info = ginfo t 0}
  | 201 /-TyBool-/ ->
    TyBool {info = ginfo t 0}
  | 202 /-TyInt-/ ->
    TyInt {info = ginfo t 0}
  | 203 /-TyFloat-/ ->
    TyFloat {info = ginfo t 0}
  | 204 /-TyChar-/ ->
    TyChar {info = ginfo t 0}
  | 205 /-TyArrow-/ ->
    TyArrow {info = ginfo t 0,
             from = gtype t 0,
             to = gtype t 1}
  | 206 /-TySeq-/ ->
    TySeq {info = ginfo t 0,
           ty = gtype t 0}
  | 207 /-TyRecord-/ ->
    let lst = create (glistlen t 0) (lam n. (gstr t n, gtype t n)) in
    TyRecord {info = ginfo t 0,
              fields =
                mapFromSeq cmpSID
                  (map (lam b : (String,Type). (stringToSid b.0, b.1)) lst)}
  | 208 /-TyVariant-/ ->
    if eqi (glistlen t 0) 0 then
      TyVariant {info = ginfo t 0,
                 constrs = mapEmpty nameCmp}
    else error "Parsing of non-empty variant types not yet supported"
  | 209 /-TyCon-/ ->
    TyCon {info = ginfo t 0,
           ident = gname t 0}
  | 210 /-TyVar-/ ->
    TyVar {info = ginfo t 0,
           ident = gname t 0}
  | 211 /-TyApp-/ ->
    TyApp {info = ginfo t 0,
           lhs = gtype t 0,
           rhs = gtype t 1}
  | 212 /-TyTensor-/ ->
    TyTensor {info = ginfo t 0,
              ty = gtype t 0}
  | 213 /-TyAll-/ ->
    TyAll {info = ginfo t 0,
           ident = gname t 0,
           ty = gtype t 0,
           sort = PolyVar ()}


  -- Get constant help function
  sem gconst(t:Unknown) =
  | n -> let t2 = bootParserGetConst t n in
         matchConst t2 (bootParserGetId t2)

  -- Match constant from ID
  sem matchConst (t:Unknown) =
  | 300 /-CBool-/   -> CBool {val = eqi (gint t 0) 1 }
  | 301 /-CInt-/    -> CInt {val = gint t 0 }
  | 302 /-CFloat-/  -> CFloat {val = gfloat t 0}
  | 303 /-CChar-/   -> CChar {val = int2char (gint t 0)}
  | 304 /-Cdprint-/ -> CDPrint {}
  | 305 /-Cerror-/  -> CError {}

  -- Get pattern help function
  sem gpat (t:Unknown) =
  | n -> let t2 = bootParserGetPat t n in
         matchPat t2 (bootParserGetId t2)

  -- Match pattern from ID
  sem matchPat (t:Unknown) =
  | 400 /-PatNamed-/ ->
    PatNamed {ident = strToPatName (gstr t 0),
            info = ginfo t 0,
            ty = tyunknown_}
  | 401 /-PatSeqTot-/ ->
    PatSeqTot {pats = create (glistlen t 0) (lam n. gpat t n),
             info = ginfo t 0,
             ty = tyunknown_}
  | 402 /-PatSeqEdge-/ ->
    let len = glistlen t 0 in
    PatSeqEdge {prefix = create len (lam n. gpat t n),
              middle = strToPatName (gstr t 0),
              postfix = create (glistlen t 1) (lam n. gpat t (addi n len)),
              info = ginfo t 0,
              ty = tyunknown_}
  | 403 /-PatRecord-/ ->
    let lst = create (glistlen t 0) (lam n. (gstr t n, gpat t n)) in
    PatRecord {bindings =
               mapFromSeq cmpSID
                 (map (lam b : (String,Pat). (stringToSid b.0, b.1)) lst),
               info = ginfo t 0,
               ty = tyunknown_}
  | 404 /-PatCon-/ ->
     PatCon {ident = gname t 0,
             subpat = gpat t 0,
             info = ginfo t 0,
             ty = tyunknown_}
 | 405 /-PatInt-/ ->
     PatInt {val = gint t 0,
             info = ginfo t 0,
             ty = tyint_}
 | 406 /-PatChar-/ ->
     PatChar {val = int2char (gint t 0),
              info = ginfo t 0,
              ty = tychar_}
 | 407 /-PatBool-/ ->
     PatBool {val = eqi (gint t 0) 1,
              info = ginfo t 0,
              ty = tybool_}
 | 408 /-PatAnd-/ ->
     PatAnd {lpat = gpat t 0,
             rpat = gpat t 1,
             info = ginfo t 0,
             ty = tyunknown_}
 | 409 /-PatOr-/ ->
     PatOr {lpat = gpat t 0,
            rpat = gpat t 1,
            info = ginfo t 0,
            ty = tyunknown_}
 | 410 /-PatNot-/ ->
     PatNot {subpat = gpat t 0,
             info = ginfo t 0,
             ty = tyunknown_}

  -- Get info help function
  sem ginfo (t:Unknown) =
  | n -> let t2 = bootParserGetInfo t n in
         matchInfo t2 (bootParserGetId t2)

  -- Match info from ID
  sem matchInfo (t:Unknown) =
  | 500 /-Info-/ ->
      Info {filename = gstr t 0,
            row1 = gint t 0,
            col1 = gint t 1,
            row2 = gint t 2,
            col2 = gint t 3}
  | 501 /-NoInfo-/ ->
      NoInfo {}


  sem strToPatName =
  | "" ->  PWildcard ()
  | x -> PName (nameNoSym x)

end

let defaultBootParserParseMCoreFileArg =
  use BootParser in _defaultBootParserParseMCoreFileArg ()

let defaultBootParserParseMExprStringArg =
  use BootParser in _defaultBootParserParseMExprStringArg ()

lang BootParserTest = BootParser + MExprPrettyPrint + MExprEq
end

mexpr
use BootParserTest in


-- Tests where strings of MExpr text is parsed and then pretty printed again.
-- All terms are tested in this way.
let norm : String -> String = lam str.
  filter (lam x. not (or (or (eqChar x ' ') (eqChar x '\n')) (eqChar x '\t'))) str in

let parseMExprStringKeywords = lam ks.
  parseMExprString { defaultBootParserParseMExprStringArg with keywords = ks }
in

-- Test the combination of parsing and pretty printing
let parse = lam ks. lam s. expr2str (parseMExprStringKeywords ks s) in
let lside : [String] -> String -> String = lam ks. lam s. norm (parse ks s) in
let lsideClosed = lside [] in
let rside : String -> String = norm in

-- Test that info gives the right columns and rows
let l_info : [String] -> String -> Info = lam ks. lam s.
  infoTm (parseMExprStringKeywords ks s) in
let l_infoClosed = l_info [] in
let r_info : Int -> Int -> Int -> Int -> Info = lam r1. lam c1. lam r2. lam c2.
      Info {filename = "internal", row1 = r1, col1 = c1, row2 = r2, col2 = c2} in

-- TmVar
let s = "_asdXA123" in
utest lside ["_asdXA123"] s with rside s in
utest match parseMExprStringKeywords [""] "#var\"\"" with TmVar r
      then nameGetStr r.ident else "ERROR" with "" in

-- TmApp
let s = "f x" in
utest lside ["f", "x"] s with rside s in
utest l_info ["f1", "f2", "foo"] "   (foo f1) f2  " with r_info 1 4 1 14 in

-- TmLam
let s = "lam x. lam y. x" in
utest lsideClosed s with rside s in
let s = "(lam x. lam y. x) z1 z2" in
utest lside ["z1", "z2"] s with rside s in
utest l_info ["_aas_12"] "  _aas_12 " with r_info 1 2 1 9 in

-- TmLet, TmLam
let s = "let y = lam x.x in y" in
utest lsideClosed s with rside s in
utest l_infoClosed "  \n lam x.x" with r_info 2 1 2 8 in
utest match parseMExprStringKeywords [] s with TmLet r then infoTm r.body else NoInfo ()
with r_info 1 8 1 15 in
utest l_info ["y"] "  let x = 4 in y  " with r_info 1 2 1 14 in
let s = "(printLn x); 10" in
utest lside ["printLn", "x"] s with rside s in

-- TmRecLets, TmLam
let s = "recursive let x = lam x.x in x" in
utest lsideClosed s with rside s in
let s = "recursive let x = lam x.x let y = lam x. x in y" in
utest lsideClosed s with rside s in
let s = "   recursive let x = 5 \n let foo = 7 in x " in
utest l_infoClosed s with r_info 1 3 2 15 in
utest
  match parseMExprStringKeywords [] s with TmRecLets r then
    let fst : RecLetBinding = head r.bindings in
    fst.info
  else never
with r_info 1 13 1 22 in

-- TmConst
let s = "true" in
utest lsideClosed s with rside s in
let s = "false" in
utest lsideClosed s with rside s in
let s = "123" in
utest lsideClosed s with rside s in
let s = "17." in
utest lsideClosed s with rside s in
let s = "'a'" in
utest lsideClosed s with rside s in
utest l_infoClosed " true " with r_info 1 1 1 5 in
utest l_infoClosed "  false " with r_info 1 2 1 7 in
utest l_infoClosed " 1234 " with r_info 1 1 1 5 in
utest l_infoClosed " 123.121 " with r_info 1 1 1 8 in
utest l_infoClosed "\n  'A' " with r_info 2 2 2 5 in

-- TmSeq
let s = "\"\"" in
utest lsideClosed s with rside s in
let s = "[3,4,1123,21,91]" in
utest lsideClosed s with rside s in
let s = "[[1],[12,42311],[23,21,91]]" in
utest lsideClosed s with rside s in
let s = "\"Hello world\"" in
utest lsideClosed s with rside s in
utest l_infoClosed "  [12,2,1] " with r_info 1 2 1 10 in

-- TmRecord
let s = "{}" in
utest lsideClosed s with rside s in
let s = "{a = 5}" in
utest lsideClosed s with rside s in
let s = "{bar = \"Hello\", foo = 123}" in
let t = urecord_ [("bar", str_ "Hello"), ("foo", int_ 123)] in
utest parseMExprStringKeywords [] s with t using eqExpr in
utest l_infoClosed " {} " with r_info 1 1 1 3 in
utest l_infoClosed " {foo = 123} " with r_info 1 1 1 12 in

-- TmRecordUpdate
let s = "{a with foo = 5}" in
utest lside ["a"] s with rside s in
let s = "{{bar='a', foo=7} with bar = 'b'}" in
let t = recordupdate_ (urecord_ [("bar", char_ 'a'), ("foo", int_ 7)]) "bar" (char_ 'b') in
utest parseMExprStringKeywords [] s with t using eqExpr in
utest l_info ["foo"] " {foo with a = 18 } " with r_info 1 1 1 19 in

-- NOTE(caylak, 2021-03-17): Commented out because test fails since parsing of TyVariant is not supported yet
-- TmType
let s = "type Foo=<> in x" in
--utest lsideClosed s with rside s in
utest l_infoClosed "  type Bar in () " with r_info 1 2 1 13 in

-- TmConDef
let s = "con Foo in x" in
utest lside ["x"] s with rside s in
let s = "con Foo : Int -> Tree in x" in
utest lside ["x"] s with rside s in
utest l_infoClosed "  con Bar in 10 " with r_info 1 2 1 12 in

-- TmConApp
let s = "Foo {a = 5}" in
utest lside ["Foo"] s with rside s in
utest l_info ["Foo"] "  Foo {foo = 7, b = 3} " with r_info 1 2 1 22 in

-- TmMatch, PatNamed
let s =  "match 5 with x then x else 2" in
utest lsideClosed s with rside s in
let s = "match foo with _ then 7 else 2" in
utest lside ["foo"] s with rside s in
utest l_infoClosed "match [4] with x then x else [] " with r_info 1 0 1 31 in
let s = " match bar with Foo {a = x} then x else 2" in
utest match parseMExprStringKeywords ["Foo", "bar"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 16 1 27 in

-- TmMatch, PatSeqTot, PatSeqEdge
let s = "match x with \"\" then x else 2" in
utest lside ["x"] s with rside s in
let s = "match x with [x,y,z] then x else 2" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 20 in
let s = " match x with [a] ++ v ++ [x,y,z] then x else 2" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 14 1 33 in
let s = "match x with \"\" ++ x ++ [y] then x else x" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 27 in
let s = "match x with [z] ++ x ++ \"\" then z else 2" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 27 in

--TmMatch, PatRecord
let s = "match x with {} then x else 2" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 15 in
let s = "match x with {bar=_, foo=x} then x else 2" in
let t = match_ (var_ "x")
               (prec_ [("bar", pvarw_), ("foo", pvar_ "x")])
               (var_ "x") (int_ 2) in
utest parseMExprStringKeywords ["x"] s with t using eqExpr in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 27 in

--TmMatch, PatCon
let s = "match x with Foo {foo = x} then x else 100" in
utest lside ["x", "Foo"] s with rside s in
utest match parseMExprStringKeywords ["x", "Foo"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 26 in

--TmMatch, PatInt, PatBool, PatChar
let s = "match x with [1,2,12] then x else x" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 21 in
let s = "match x with 'A' then x else x" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 16 in
let s = "match x with [true,false] then x else x" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 25 in

-- TmMatch, PatAnd, PatOr, PatNot
let s = "match x with 1 & x then x else x" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 18 in
let s = "match x with 1 | x then x else x" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 18 in
let s = "match x with !y then x else x" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 13 1 15 in
let s = "match 1 with (a & b) | (!c) then x else x" in
utest lside ["x"] s with rside s in
utest match parseMExprStringKeywords ["x"] s with TmMatch r then infoPat r.pat else NoInfo ()
with r_info 1 14 1 26 in

-- TmUtest
let s = "utest lam x.x with 4 in 0" in
utest lsideClosed s with rside s in
utest l_infoClosed "\n utest 3 with 4 in () " with r_info 2 1 2 18 in

-- TmNever
let s = "never" in
utest lsideClosed s with rside s in
utest l_infoClosed "  \n  never " with r_info 2 2 2 7 in

-- TmExt
let s = "external y : Int in 1" in
utest lsideClosed s with rside s in
utest l_infoClosed "   \n  external y : Int in 1" with r_info 2 2 2 23 in

let s = "external y! : Int in 1" in
utest lsideClosed s with rside s in
utest l_infoClosed "   \n  external y! : Int in 1" with r_info 2 2 2 24 in

-- TyUnknown
let s = "let y:Unknown = lam x.x in y" in
utest lsideClosed s with rside "let y = lam x.x in y" in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 13 in
let s = "lam x:Int. lam y:Char. x" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] " \n lam x:Int. lam y:Char. x" with TmLam l then infoTy l.tyAnnot else NoInfo ()
with r_info 2 7 2 10 in

-- TyInt
let s = "let y:Int = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 9 in

-- TyFloat
let s = "let y:Float = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 11 in

-- TyChar
let s = "let y:Char = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 10 in

-- TyArrow
let s = "let y:Int->Int = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 14 in

-- Nested TyArrow
let s = "let y:[Float]->Int = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 18 in

-- TySeq
let s = "let y:[Int] = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 11 in

-- Nested TySeq
let s = "let y:[{a:{a_1:Int,a_2:Float},b:{b_1:[Char],b_2:Float}}]= lam x.x in y" in
let recTy = tyseq_ (tyrecord_ [
  ("a", tyrecord_ [
    ("a_1", tyint_),
    ("a_2", tyfloat_)]),
  ("b", tyrecord_ [
    ("b_1", tystr_),
    ("b_2", tyfloat_)])]) in
let typedLet = lam letTy.
  bind_ (let_ "y" letTy (ulam_ "x" (var_ "x")))
        (var_ "y") in
utest parseMExprStringKeywords [] s with typedLet recTy using eqExpr in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 56 in

-- TyTensor
let s = "let y:Tensor[Int] = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 17 in

-- Nested TyTensor
let s = "let y:{a:Tensor[Char],b:Float}= lam x.x in y" in
let recTy = tyseq_ (tyrecord_ [
    ("a", tytensor_ tychar_),
    ("b", tyfloat_)
  ])
in
let typedLet = lam letTy.
  bind_ (let_ "y" letTy (ulam_ "x" (var_ "x")))
        (var_ "y") in
utest parseMExprStringKeywords [] s with typedLet recTy using eqExpr in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 30 in

-- TyRecord
let s = "let y:{a:Int,b:[Char]} = lam x.x in y" in
let recTy = tyrecord_ [("a", tyint_), ("b", tystr_)] in
utest parseMExprStringKeywords [] s with typedLet recTy using eqExpr in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 22 in

-- Nested TyRecord
let s = "let y:{a:{a_1:Int,a_2:Float},b:{b_1:[Char],b_2:Float}} = lam x.x in y" in
let recTy = tyrecord_ [
  ("a", tyrecord_ [
    ("a_1", tyint_),
    ("a_2", tyfloat_)]),
  ("b", tyrecord_ [
    ("b_1", tystr_),
    ("b_2", tyfloat_)])] in
utest parseMExprStringKeywords [] s with typedLet recTy using eqExpr in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 54 in

-- TyVariant
let s = "let y:<> = lam x.x in y" in
-- NOTE(caylak,2021-03-17): Parsing of TyVariant is not supported yet
--utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 8 in

-- TyVar
let s = "let y:_asd = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 10 in

-- TyAll
let s = "let y:all x.x = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 13 in

-- Nested TyAll
let s = "let y:all x.(all y.all z.z)->all w.w = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 36 in

-- TyCon
let s = "let y:Foo = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 6 1 9 in

-- TyApp
let s = "let y:(Int->Int)Int = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 7 1 19 in

-- Nested TyApp
let s = "let y:((Int->Int)Int->Int)Int = lam x.x in y" in
utest lsideClosed s with rside s in
utest match parseMExprStringKeywords [] s with TmLet l then infoTy l.tyAnnot else NoInfo ()
with r_info 1 8 1 29 in

-- Allow free variables
utest
  match parseMExprString
   { defaultBootParserParseMExprStringArg with allowFree = true } "x"
  with TmVar r then nameGetStr r.ident else "ERROR"
with "x" in

()
