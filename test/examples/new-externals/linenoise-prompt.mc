include "common.mc"
include "string.mc"

let prompt = externalbind "let foo() = LNoise.linenoise \">> \" in foo" ["unit", "unit"] ["linenoise"]

mexpr
prompt ()