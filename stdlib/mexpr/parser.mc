-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--

include "string.mc"
include "seq.mc"
include "mexpr/ast.mc"
include "mexpr/info.mc"


let tabSpace = 2

-- Base language for whitespace and comments (WASC) parsing
lang WSACParser
  sem eatWSAC (p : Pos) =
end

-- Eats whitespace
lang WhitespaceParser = WSACParser
  sem eatWSAC (p : Pos)  =
  | " " ++ xs -> eatWSAC (advanceCol p 1)  xs
  | "\t" ++ xs -> eatWSAC (advanceCol p tabSpace) xs
  | "\n" ++ xs -> eatWSAC (advanceRow p 1) xs
  | "\r" ++ xs -> eatWSAC p xs
  | x -> {str = x, pos = p}
end

let _ = use WhitespaceParser in
  utest eatWSAC (initPos "") "  foo"
    with {str = "foo", pos = (posVal "" 1 2)} in
  utest eatWSAC (initPos "") " \tfoo"
    with {str = "foo", pos = (posVal "" 1 3)} in
  utest eatWSAC (initPos "") " \n    bar "
    with {str = "bar ", pos = (posVal "" 2 4)} in
  ()

-- Eat line comments of the form --
lang LineCommentParser = WSACParser
  sem eatWSAC (p : Pos)  =
  | "--" ++ xs ->
    recursive
    let remove = lam p. lam str.
      match str with "\n" ++ xs then eatWSAC (advanceRow p 1) xs else
      match str with [x] ++ xs then remove (advanceCol p 1) xs else
      eatWSAC p str
    in remove p xs
end

-- Eat multiline comment of the form *-  -*
lang MultilineCommentParser = WSACParser
  sem eatWSAC (p : Pos) =
  | "/-" ++ xs ->
    recursive
    let remove = lam p. lam str. lam d.
      match str with "/-" ++ xs then remove (advanceCol p 2) xs (addi d 1) else
      match str with "\n" ++ xs then remove (advanceRow p 1) xs d else
      match str with "-/" ++ xs then
        if eqi d 1 then eatWSAC (advanceCol p 2) xs
        else remove (advanceCol p 2) xs (subi d 1) else
      match str with [_] ++ xs then remove (advanceCol p 1) xs d else
      if eqi d 0 then eatWSAC p str else posErrorExit p "Unmatched multiline comments."
    in remove (advanceCol p 2) xs 1
end

-- Commbined WSAC parser for MExpr
lang MExprWSACParser = WhitespaceParser + LineCommentParser + MultilineCommentParser

let _ = use MExprWSACParser in
  utest eatWSAC (initPos "") " --foo \n  bar "
    with {str = "bar ", pos = posVal "" 2 2} in
  utest eatWSAC (initPos "") " /- foo -/ bar"
    with {str = "bar", pos = posVal "" 1 11} in
  utest eatWSAC (initPos "") " /- foo\n x \n -/ \nbar "
    with {str = "bar ", pos = posVal "" 4 0} in
  utest eatWSAC (initPos "") " /- x -- y /- foo \n -/ -/ !"
    with {str = "!", pos = posVal "" 2 7} in
  ()



-- Top of the expression parser. Connects WSAC with parsing of other non terminals
lang ExprParser = WSACParser
  sem parseExpr (p: Pos) =
  | s ->
    let r1 = parseExprMain p 0 s in
    let r2 = eatWSAC r1.pos r1.str in
    if eqi (length r2.str) 0 then r1.val
    else posErrorExit r2.pos "Parse error. Unknown characters."
  
  sem parseExprMain (p: Pos) (prec: Int) =
  | s ->
    let r1 = eatWSAC p s in
    let exp = parseExprImp r1.pos r1.str in
    let r2 = eatWSAC exp.pos exp.str in
    parseInfix r2.pos prec exp r2.str 

  sem parseInfix (p: Pos) (prec: Int) (exp: Expr) =

  sem parseExprImp (p: Pos) =
  | _ -> posErrorExit p "Parse error. Unknown character sequence."
end


-- Include this fragment if there are no infix operations
lang ExprParserNoInfix = ExprParser
  sem parseInfix (p: Pos) (prec: Int) (exp: Expr) =
  | _ -> exp
end

-- Parses an identfier that starts with a lower-case letter or a '_' if parameter
-- 'upper' is false, and starts with an upper-case letter if 'upper' is true.
-- The rest of the string can contain both upper and lower-case letters.
-- If no identifier, the 'val' field contains an empty string.
let parseIdent = lam upper. lam p. lam str.
  recursive
  let work = lam acc. lam first. lam p. lam str.
    match str with [x] ++ xs then
      let c = char2int x in
      let m1 = or (not first) upper in
      let m2 = or (not first) (not upper) in
      if or (or (and m1 (isUpperAlpha x))
                (and m2 (or (eqChar x '_') (isLowerAlpha x))))
	    (and (not first) (isDigit x))	    
      then work (snoc acc x) false (advanceCol p 1) xs
      else {val = acc, pos = p, str = str}
    else {val = acc, pos = p, str = str}
  in work "" true p str

utest parseIdent false (initPos "") "+"
  with {val = "", str = "+", pos = posVal "" 1 0}
utest parseIdent false (initPos "") "a "
  with {val = "a", str = " ", pos = posVal "" 1 1}
utest parseIdent false (initPos "") "ba"
  with {val = "ba", str = "", pos = posVal "" 1 2}
utest parseIdent false (initPos "") "_asd "
  with {val = "_asd", str = " ", pos = posVal "" 1 4}
utest parseIdent true (initPos "") "_asd "
  with {val = "", str = "_asd ", pos = posVal "" 1 0}
utest parseIdent false (initPos "") "Asd12 "
  with {val = "", str = "Asd12 ", pos = posVal "" 1 0}
utest parseIdent true (initPos "") "Asd12 "
  with {val = "Asd12", str = " ", pos = posVal "" 1 5}


-- Parse identifier
lang IdentParser = ExprParser 
  sem parseExprImp (p: Pos) =
  | (['_' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' |
      'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' |
      'x' | 'y' | 'z' ] ++ s) & xs ->
    let r = parseIdent false p xs in
    nextIdent p r.str r.val 

  sem nextIdent (p: Pos) (xs: string) =
end


-- Parsing of boolean literals
lang BoolParser = ExprParser + IdentParser + ConstAst + BoolAst
  sem nextIdent (p: Pos) (xs: String) =
  | "true" ->
      let p2 = advanceCol p 4 in
      {val = TmConst {val = CBool {val = true}, fi = makeInfo p p2},
       pos = p2, str = xs}
  | "false" ->
      let p2 = advanceCol p 5 in
      {val = TmConst {val = CBool {val = false}, fi = makeInfo p p2},
       pos = p2, str = xs}
end


-- Parsing of an unsigned integer
lang UIntParser = ExprParser + ConstAst + IntAst
  sem parseExprImp (p : Pos) =
  | (['0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'] ++ s) & xs -> 
    recursive
    let work = lam p2. lam str. lam num.
      match str with [x] ++ xs then
        let c = char2int x in
        if and (geqi c 48) (leqi c 57)
        then work (advanceCol p2 1) xs (snoc num x)
        else {val = TmConst {val = CInt {val = string2int num}, fi = makeInfo p p2},
              pos = p2, str = str}
      else {val = TmConst {val = CInt {val = string2int num}, fi = makeInfo p p2},
            pos = p2, str = str}
    in work (advanceCol p 1) s ([head xs])
end


-- Fragment for simple parsing of keyword
lang KeywordUtils = WSACParser
  sem matchKeyword (keyword: String) (p: Pos) =
  | s ->
     let r = eatWSAC p s in
     if isPrefix eqc keyword r.str then
       let l = length keyword in
       {pos = advanceCol r.pos l, str = slice r.str l (subi (length r.str) l)}
     else
       posErrorExit r.pos (join ["Unknown character. Expected '", keyword, "'."])
end


-- Parsing if expressions
lang IfParser = ExprParser + IdentParser + KeywordUtils +  MatchAst + BoolPat
  sem nextIdent (p: Pos) (xs: String) =
  | "if" ->
     let e1 = parseExprMain (advanceCol p 2) 0 xs in
     let r1 = matchKeyword "then" e1.pos e1.str  in
     let e2 = parseExprMain r1.pos 0 r1.str in
     let r2 = matchKeyword "else" e2.pos e2.str  in
     let e3 = parseExprMain r2.pos 0 r2.str in
     {val = TmMatch {target = e1.val, pat = PBool {val = true, fi = NoInfo ()},
                     thn = e2.val, els = e3.val, fi = makeInfo p e3.pos},
      pos = e3.pos,
      str = e3.str}
 end



-- Parse parentheses
lang ParenthesesParser = ExprParser + KeywordUtils
  sem parseExprImp (p: Pos) =
  | "(" ++ xs ->
    let e = parseExprMain (advanceCol p 1) 0 xs in
    let r = matchKeyword ")" e.pos e.str in
    {val = e.val, pos = r.pos, str = r.str}
end

-- Parses a sequence of 
lang SeqParser = ExprParser + KeywordUtils + SeqAst
  sem parseExprImp (p: Pos) =
  | "[" ++ xs -> 
      recursive let work = lam acc. lam first. lam p2. lam str.
        let r = eatWSAC p2 str in
  	match r.str with "]" ++ xs then
	  {val = TmSeq{tms = acc, fi = makeInfo p (advanceCol r.pos 1)},
	   pos = advanceCol r.pos 1, str = xs}
	else
	  let r2 = if first then r else matchKeyword "," r.pos r.str in
	  let e = parseExprMain r2.pos 0 r2.str in
	  work (snoc acc e.val) false e.pos e.str  
      in work [] true (advanceCol p 1) xs
end


-- Matches a character (including escape character). 
let matchChar : Pos -> String -> {val: Char, pos: Pos, str: String} =
  lam p. lam str.
  let ret = lam c. lam s. lam n. {val = c, pos = (advanceCol p n), str = s} in
    match str with "\\" ++ xs then
      match xs with "\\" ++ xs then ret '\\' xs 2 else
      match xs with "n" ++ xs then ret '\n' xs 2 else
      match xs with "t" ++ xs then ret '\t' xs 2 else
      match xs with "\"" ++ xs then ret '\"' xs 2 else 
      match xs with "'" ++ xs then ret '\'' xs 2 else
      posErrorExit (advanceCol p 1) "Unknown escape character."
    else match str with [x] ++ xs then ret x xs 1
    else posErrorExit p "Unexpected end of file."
    -- TODO (David, 2020-09-27): Shoud we allow newlines etc. inside strings
    -- TODO (David, 2020-09-27): Add all other relevant escape characters

-- Parses strings, including escape characters
lang StringParser = ExprParser + SeqAst + CharAst
  sem parseExprImp (p: Pos) =
  | "\"" ++ xs ->
    recursive let work = lam acc. lam p2. lam str.
      match str with "\"" ++ xs then
        {val = TmSeq {tms = acc, fi = makeInfo p (advanceCol p2 1)},
	 pos = advanceCol p2 1, str = xs}
      else	  
        let r =  matchChar p2 str in
        let v = TmConst {val = CChar {val = r.val}, fi = makeInfo p2 r.pos} in
	work (snoc acc v) r.pos r.str
    in
      work [] (advanceCol p 1) xs
end

-- Parses character literals
lang CharParser = ExprParser + KeywordUtils + CharAst
  sem parseExprImp (p: Pos) =
  | "\'" ++ xs ->
      let r =  matchChar (advanceCol p 1) xs in
      let r2 = matchKeyword "\'" r.pos r.str in
      {val = TmConst {val = CChar {val = r.val}, fi = makeInfo p r2.pos},
       pos = r2.pos, str = r2.str}
end



-- Parse variable
lang VarParser = ExprParser + IdentParser + VarAst
  sem nextIdent (p: Pos) (xs: string) =
  | x ->
      let p2 = advanceCol p (length x) in
      {val = TmVar {ident = nameNoSym x, fi = makeInfo p p2},
       pos = p2, str = xs}
end


-- Parsing of a lambda
lang FunParser = ExprParser + IdentParser + KeywordUtils + FunAst 
  sem nextIdent (p: Pos) (xs: String) =
  | "lam" ->
    let r = eatWSAC (advanceCol p 3) xs in
    let r2 = parseIdent false r.pos r.str in
    let r3 = matchKeyword "." r2.pos r2.str  in
    let e = parseExprMain r3.pos 0 r3.str in
    {val = TmLam {ident = nameNoSym r2.val, tpe = None (),
                  body = e.val, fi = makeInfo p e.pos},
     pos = e.pos, str = e.str}
    -- TODO (David, 2020-09-27): Add parsing of type     
end


-- Parsing let expressions
lang LetParser = ExprParser + IdentParser + KeywordUtils + LetAst
  sem nextIdent (p: Pos) (xs: String) =
  | "let" ->
    let r = eatWSAC (advanceCol p 3) xs in
    let r2 = parseIdent false r.pos r.str in
    let r3 = matchKeyword "=" r2.pos r2.str in
    let e1 = parseExprMain r3.pos 0 r3.str in
    let r4 = matchKeyword "in" e1.pos e1.str in
    let e2 = parseExprMain r4.pos 0 r4.str in
    {val = TmLet {ident = nameNoSym r2.val, body = e1.val,
                  inexpr = e2.val, fi = makeInfo p e2.pos},
     pos = e2.pos, str = e2.str}
end


-- General fragment for handling infix operations
lang ExprInfixParser = ExprParser
  syn Associativity = 
  | LeftAssoc ()
  | RightAssoc ()

  sem parseInfix (p: Pos) (prec: Int) (exp: Expr) =
  | str ->
    let r = eatWSAC p str in
    match parseInfixImp r.pos r.str with Some op then
      if geqi op.prec prec then
        let prec2 = match op.assoc with LeftAssoc ()
                    then addi op.prec 1
                    else op.prec in
        let exp2 = parseExprMain op.pos prec2 op.str in 
        let exp3 = {val = op.val exp.val exp2.val,
                    pos = exp2.pos, str = exp2.str} in
	parseInfix exp3.pos prec exp3 exp3.str
      else exp
    else exp
    
  sem parseInfixImp (p: Pos) =
end


-- This parser should be used if juxtaposition is NOT used
lang ExprInfixParserClosed = ExprInfixParser
  sem parseInfixImp (p: Pos) =
  | _ -> None ()
end

-- This parser should be used for application using juxaposition 
lang ExprInfixParserJuxtaposition = ExprInfixParser + AppAst
  sem parseInfixImp (p: Pos) =
  | str ->
    Some {
      val = lam x. lam y.
        TmApp {lhs = x, rhs = y, fi = mergeInfo (info x) (info y)},
      pos = p, str = str, assoc = LeftAssoc (), prec = 50}
end


lang MExprParserBase = BoolParser + UIntParser + IfParser + 
                       ParenthesesParser + MExprWSACParser +
		       SeqParser + 
		       StringParser + CharParser +
		       VarParser + FunParser + LetParser

lang MExprParser = MExprParserBase + ExprParserNoInfix




mexpr



use MExprParser in

-- Unsigned integer
utest parseExprMain (initPos "file") 0 "  123foo" with
      {val = TmConst {val = CInt {val = 123}, fi = infoVal "file" 1 2 1 5},
       pos = posVal "file" 1 5, str = "foo"} in
      
--If expression
utest (parseExprMain (initPos "") 0 "  if 1 then 22 else 3").pos
  with posVal "" 1 21 in
-- Boolean literal 'true'
utest parseExpr (initPos "f") " true " with
      TmConst {val = CBool {val = true}, fi = infoVal "f" 1 1 1 5} in 
-- Boolean literal 'false'
utest parseExpr (initPos "f") " true " with
      TmConst {val = CBool {val = true}, fi = infoVal "f" 1 1 1 5} in 
-- Parentheses 
utest parseExpr (initPos "") " ( 123) " with
      TmConst {val = CInt {val = 123}, fi = infoVal "" 1 3 1 6} in
-- Sequences
utest parseExpr (initPos "") "[]" with
      TmSeq {tms = [], fi = infoVal "" 1 0 1 2} in
utest parseExpr (initPos "") " [ ] " with
      TmSeq {tms = [], fi = infoVal "" 1 1 1 4} in
utest parseExprMain (initPos "") 0 " [ 17 ] " with
      let v = TmConst {val = CInt {val = 17}, fi = infoVal "" 1 3 1 5} in
      {val = TmSeq {tms = [v], fi = infoVal "" 1 1 1 7},
       pos = posVal "" 1 7, str = " "} in
utest parseExpr (initPos "") " [ 232 , ( 19 ) ] " with
      let v1 = TmConst {val = CInt {val = 232}, fi = infoVal "" 1 3 1 6} in
      let v2 = TmConst {val = CInt {val = 19}, fi = infoVal "" 1 11 1 13} in
      TmSeq {tms = [v1,v2], fi = infoVal "" 1 1 1 17} in 
-- Strings
let makeChar = lam k. lam c. lam n.
    TmConst {val = CChar {val = c}, fi = infoVal "" 1 n 1 (addi n k)} in
let mkc = makeChar 1 in
let mkc2 = makeChar 2 in
utest parseExpr (initPos "") " \"Foo\" " with
  let str = [mkc 'F' 2, mkc 'o' 3, mkc 'o' 4] in
  TmSeq {tms = str, fi = infoVal "" 1 1 1 6} in
utest parseExpr (initPos "") " \" a\\\\ \\n\" " with
  let str = [mkc ' ' 2, mkc 'a' 3, mkc2 '\\' 4, mkc ' ' 6, mkc2 '\n' 7] in
  TmSeq {tms = str, fi = infoVal "" 1 1 1 10} in
-- Chars
utest parseExprMain (initPos "") 0 " \'A\' " with
  {val = TmConst {val = CChar {val = 'A'}, fi = infoVal "" 1 1 1 4},
   pos = posVal "" 1 4, str = " "} in
utest parseExpr (initPos "") " \'\\n\' " with
  TmConst {val = CChar {val = '\n'}, fi = infoVal "" 1 1 1 5} in 
-- Var
utest (parseExprMain (initPos "") 0 " _xs ").pos with posVal "" 1 4 in
utest (parseExprMain (initPos "") 0 " fOO_12a ").pos with posVal "" 1 8 in
-- Lambda
utest (parseExprMain (initPos "") 0 " lam x . x ").pos
  with posVal "" 1 10 in
-- Let
utest (parseExprMain (initPos "") 0 "  let x = 5 in 8 ").pos
  with posVal "" 1 16 in 


()