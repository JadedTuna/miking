include "common.mc"
include "string.mc"

-- syntax: externalbind <OCaml code> <types>
-- ["float", "float", "float"] corresponds to float -> float -> float
-- supported types: float, int, unit

-- bind ext_pow to Float.pow, with OCaml type float -> float -> float
let ext_pow = externalbind "Float.pow" ["float", "float", "float"]
let ext_rnd_init = externalbind "Random.self_init" ["unit", "unit"]
let ext_rnd_int = externalbind "Random.int" ["int", "int"]

-- input arguments are ignored, 9 and 2 are used instead
let ext_pow_9_2 = externalbind "let foo a b = Float.pow 9.0 2.0 in foo" ["float", "float", "float"]

mexpr
print "ext_pow 2.0 4.0 = "; printLn (float2string (ext_pow 2.0 4.0));
printLn "ext_rnd_init ()"; ext_rnd_init ();
print "ext_rnd_int 29 = "; printLn (int2string (ext_rnd_int 29));
print "ext_pow_9_2 5.0 3.0 = "; printLn (float2string (ext_pow_9_2 5.0 3.0))