open Intrinsics

val add_external : int Mseq.t -> 'a -> unit

val get_external : int Mseq.t -> 'a

val call_external : int Mseq.t -> 'b Mseq.t -> 'a

val load_libraries : int Mseq.t -> unit
