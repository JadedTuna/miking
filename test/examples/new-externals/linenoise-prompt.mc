include "common.mc"
include "string.mc"

let readline = externalbind {
  type_ = lam a : String -> String. (),
  expr = "fun s -> match LNoise.linenoise s with
    | None -> \"\"
    | Some v -> v",
  deps = ["linenoise"]
}

mexpr
let result : String = readline ">> " in
printLn (concat "You wrote: " result)

