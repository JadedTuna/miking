include "seq.mc"
include "char.mc"
include "mexpr/info.mc"

type Highlight
-- A section of the code inside the area to be highlighted, but that
-- is itself irrelevant. Optional, sections between other highlighted
-- sections are irrelevant by default.
con Irrelevant : Info -> Highlight
-- A section that is inside the highlighted area and is relevant,
-- i.e., it should be highlighted in some way.
con Relevant : Info -> Highlight
-- Text to be added in the highlighted section, even though it is not
-- present in the original input. If `ensureSurroundedBySpaces` is
-- true then the added content will additionally be surrounded by a
-- space on either side, from the original input if possible,
-- otherwise added.
con Added : {content : String, ensureSurroundedBySpaces : Bool} -> Highlight

-- Highlighting can be configured using the functions in this config.
type HighlightConfig =
  { beforeSection : String -> String  -- Called for the part before the section *on the same line* as the beginning
  , afterSection : String -> String   -- Called for the part after the section *on the same line* as the end
  , irrelevant : String -> String
  , relevant : String -> String
  , added : String -> String
  }

type InnerHighlight
con IBefore : () -> InnerHighlight
con IAfter : () -> InnerHighlight
con IRelevant : () -> InnerHighlight
con IIrrelevant : () -> InnerHighlight
con IAdded : {ensureSurroundedBySpaces : Bool} -> InnerHighlight

type HPos = { row : Int, col : Int }
type HInput = { pos : HPos, rest : String }

let _advanceRow
  : HPos -> HPos
  = lam pos. {{ pos with row = addi pos.row 1 } with col = 0}
let _advanceCol
  : HPos -> HPos
  = lam pos. { pos with col = addi pos.col 1 }
let _hposLessThan
  : HPos -> HPos -> Bool
  = lam a. lam b. or (lti a.row b.row) (and (eqi a.row b.row) (lti a.col b.col))
let _advanceInput
  : HInput -> Option (Char, HInput)
  = lam input.
    switch input.rest
    case "\n" ++ rest then Some ('\n', {pos = _advanceRow input.pos, rest = rest})
    case [c] ++ rest then Some (c, {pos = _advanceCol input.pos, rest = rest})
    case [] then None ()
    end
let _splitInput
  : HPos -> HInput -> (String, HInput)
  = lam target. lam input.
    recursive let work = lam acc. lam input: HInput.
      if _hposLessThan input.pos target then
        match _advanceInput input with Some (c, input) then
          work (snoc acc c) input
        else (acc, input)
      else (acc, input)
    in work "" input

let _getRange
  : Highlight
  -> Option (HPos, HPos)
  = lam h.
    switch h
    case Irrelevant (Info x) then Some ({row = x.row1, col = x.col1}, {row = x.row2, col = x.col2})
    case Relevant (Info x) then Some ({row = x.row1, col = x.col1}, {row = x.row2, col = x.col2})
    case Added _ then None ()
    end

-- Take a sequence of sections to be highlighted (positioned through
-- `Info` values) belonging to a single file, in order, then produce a
-- highlighted version of that section of the input file.
let formatHighlights
  : HighlightConfig
  -> String  -- File content
  -> [Highlight]
  -> String  -- Highlighted section after processing
  = lam config. lam content. lam highlights.
    let contentTooShort = lam. error "The file isn't long enough, some of the highlight is outside" in
    let input: HInput = { rest = content, pos = { row = 1, col = 0} } in
    let startPos: HPos =
      match findMap _getRange highlights with Some (startPos, _)
      then startPos
      else error "This highlight list doesn't have any info fields in it" in
    let endPos: HPos =
      match findMap _getRange (reverse highlights) with Some (_, endPos)
      then endPos
      else error "This highlight list doesn't have any info fields in it" in

    -- NOTE(vipa, 2022-03-04): Identify the sections and their content
    match _splitInput { startPos with col = 0 } input with (_, input) in
    match _splitInput startPos input with (before, input) in
    let sections = [(before, IBefore ())] in
    recursive let work = lam sections. lam input. lam highlights.
      match highlights with [h] ++ highlights then
        match _getRange h with Some (startPos, endPos) then
          match _splitInput startPos input with (irr, input) in
          match _splitInput endPos input with (sec, input) in
          let label =
            switch h
            case Relevant _ then IRelevant ()
            case Irrelevant _ then IIrrelevant ()
            case _ then error "impossible"
            end in
          work (concat sections [(irr, IIrrelevant()), (sec, label)]) input highlights
        else match h with Added x then
          work (snoc sections (x.content, IAdded {ensureSurroundedBySpaces = x.ensureSurroundedBySpaces})) input highlights
        else never
      else (sections, input)
    in
    match work sections input highlights with (sections, input) in
    match _splitInput (_advanceRow endPos) input with (after, _) in
    let after = match after with after ++ "\n" then after else after in
    let sections = snoc sections (after, IAfter ()) in

    let sections = filter (lam x. match x with ([_] ++ _, _) then true else false) sections in

    -- NOTE(vipa, 2022-03-04): Format and concatenate the
    -- sections. This isn't just a concatMap because we need to fix
    -- padding for `Added` sections.
    recursive let work = lam acc. lam needsPreSpace. lam sections.
      match sections with [(content, label)] ++ sections then
        let needsPadding = match label with (IAdded {ensureSurroundedBySpaces = true}) then true else false in
        let needsPostSpace =
          match sections with [([c] ++ _, _)] ++ _
          then if isWhitespace c then false else true
          else false in
        let pre = if and needsPadding needsPreSpace then config.irrelevant " " else "" in
        let post = if and needsPadding needsPostSpace then config.irrelevant " " else "" in
        let f = switch label
          case IBefore _ then config.beforeSection
          case IAfter _ then config.afterSection
          case IRelevant _ then config.relevant
          case IIrrelevant _ then config.irrelevant
          case IAdded _ then config.added
          end in
        let nextNeedsPreSpace =
          match concat content post with _ ++ [c] in
          if isWhitespace c then false else true in
        work (join [acc, pre, f content, post]) nextNeedsPreSpace sections
      else acc
    in
    work "" false sections

let terminalHighlightAddedConfig: HighlightConfig =
  { beforeSection = lam str. concat "[0m" str
  , afterSection = lam str. concat "[0m" str
  , irrelevant = lam str. concat "[0m" str
  , relevant = lam str. concat (concat "[37m" str) "[0m"
  , added = lam str. concat (concat "[31m" str) "[0m"
  }

let terminalHighlightErrorConfig: HighlightConfig =
  { beforeSection = lam str. concat "[0m" str
  , afterSection = lam str. concat "[0m" str
  , irrelevant = lam str. concat "[0m" str
  , relevant = lam str. concat (concat "[31m" str) "[0m"
  , added = lam str. concat (concat "[31m" str) "[0m"
  }

mexpr

let content = join
  [ "let a = 1 in\n"
  , "let x = match a with\n"
  , "  | 1 -> match x with\n"
  , "    | \"blub\" -> 1\n"
  , "  | 2 -> 2\n"
  ] in

let config =
  { beforeSection = lam str. join ["<bef>", str, "</bef>"]
  , afterSection = lam str. join ["<aft>", str, "</aft>"]
  , irrelevant = lam str. join ["<irr>", str, "</irr>"]
  , relevant = lam str. join ["<rel>", str, "</rel>"]
  , added = lam str. join ["<new>", str, "</new>"]
  } in

let highlights =
  [ Relevant (infoVal "test" 2 8 2 13)
  , Relevant (infoVal "test" 2 16 2 20)
  , Irrelevant (infoVal "test" 3 8 3 9)
  , Added {content = "(", ensureSurroundedBySpaces = true}
  , Relevant (infoVal "test" 3 9 3 14)
  ] in

utest formatHighlights config content highlights
with
  let content: String = join
    [ "<bef>let x = </bef><rel>match</rel><irr> a </irr><rel>with</rel><irr>\n"
    , "  | 1 -></irr><irr> </irr><new>(</new><irr> </irr><rel>match</rel><aft> x with</aft>"
    ]
  in content in

()