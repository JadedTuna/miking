open Intrinsics

let ext_map = Hashtbl.create 1024

let add_external id e =
  let id = Mseq.Helpers.to_utf8 id in
  Hashtbl.add ext_map id (Obj.magic e)

(* Should fetch the residual through the interface *)
let get_external id =
  let id = Mseq.Helpers.to_utf8 id in
  let module M = (val Inter.get_plugin id) in
  M.residual ()

let call_external id args =
  (* ignore mock argument, used in boot to rewrap *)
  let args = Mseq.reverse (Mseq.tail (Mseq.reverse args)) in
  let ext = get_external id in
  (* to mitigate [ignored-extra-argument] for a*)
  let [@warning "-20"] apply = fun f a -> Obj.magic ((Obj.magic f) a) in
  let result = Mseq.Helpers.fold_left apply ext args in
  result

let load_libraries dyn_ext_file =
  let dyn_ext_file = Mseq.Helpers.to_utf8 dyn_ext_file in
  try
    Dynlink.loadfile dyn_ext_file
  with
  | Dynlink.Error (Dynlink.Linking_error (_,
     Dynlink.Uninitialized_global str)) ->
       Printf.eprintf "ERROR: Module \"%s\" must be fully initialized before %s\
       can be loaded\n" str dyn_ext_file;
       Printf.eprintf "HINT: let bars = peval bar in () \n\
       ===>> let f = lam. let bars = peval bar in () in f ()\n";
       exit 1
