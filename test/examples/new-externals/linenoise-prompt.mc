include "common.mc"
include "string.mc"

let readline = externalbind {
  type_ = lam a : String -> String. (),
  expr = "fun s ->
  let s = Boot.Intrinsics.Mseq.Helpers.to_utf8 s in
  Boot.Intrinsics.Mseq.Helpers.of_utf8 (
  match LNoise.linenoise s with
    | None -> \"\"
    | Some v -> v)",
  deps = ["linenoise"]
}

mexpr
let result : String = readline ">> " in
printLn (concat "You wrote: " result)

