include "common.mc"
include "string.mc"

let prompt = externalbind "
    let foo s = Boot.Intrinsics.Mseq.Helpers.of_utf8 (
        match LNoise.linenoise (Boot.Intrinsics.Mseq.Helpers.to_utf8 s) with
        | None -> \"\"
        | Some v -> v
    ) in foo" (lam a : String -> String. ()) ["linenoise"]

mexpr
let result : String = prompt ">> " in
printLn (concat "You wrote: " result)

