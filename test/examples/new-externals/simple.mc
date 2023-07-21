include "common.mc"
include "string.mc"

-- syntax: externalbind <OCaml code> <types>
-- ["float", "float", "float"] corresponds to float -> float -> float
-- supported types: float, int, unit

-- bind ext_pow to Float.pow, with OCaml type float -> float -> float
let ext_pow = externalbind {
  type_ = lam a : Float -> Float -> Float. (),
  expr = "Float.pow",
  deps = []
}
let ext_rnd_init = externalbind {
  type_ = lam a : () -> (). (),
  expr = "Random.self_init",
  deps = []
}
let ext_rnd_int = externalbind {
  type_ = lam a : Int -> Int. (),
  expr = "Random.int",
  deps = []
}

-- input arguments are ignored, 9 and 2 are used instead
let ext_pow_9_2 = externalbind {
  type_ = lam a : Float -> Float -> Float. (),
  expr = "let foo a b = Float.pow 9.0 2.0 in foo",
  deps = []
}

mexpr
print "ext_pow 2.0 4.0 = "; printLn (float2string (ext_pow 2.0 4.0));
printLn "ext_rnd_init ()"; ext_rnd_init ();
print "ext_rnd_int 29 = "; printLn (int2string (ext_rnd_int 29));
print "ext_pow_9_2 5.0 3.0 = "; printLn (float2string (ext_pow_9_2 5.0 3.0))
