include "common.mc"
include "string.mc"

-- NOTE: currently doesn't work, while type conversion functions
-- are developed

let ext_flatten = externalbind {
  type_ = lam a : [[[[Int]]]] -> [Int]. (),
  expr = "fun a -> List.flatten (List.flatten (List.flatten a))",
  deps = []
}

mexpr
map (lam a : Int. printLn (int2string a)) (ext_flatten [
  [
    [[1, 2], [3, 4], [5, 6]],
    [[7, 8], [9, 10]]
  ],
  [
    [[11, 12], [13, 14], [15, 16]],
    [[17, 18], [19, 20]]
  ]
])
