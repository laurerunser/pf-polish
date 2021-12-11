open Polish

module NameTable : Map.S with type key = name
(* The environment variables will be stored in a NameTable which is an assoc
table indexed by strings. This is more efficient than simple assoc lists. *)

val eval_block : program -> int NameTable.t -> int NameTable.t
(* [eval_block p env] evaluates the program p with the current environment
variables in env. It returns the updated environment variables. *)                                                                                   
(*
For reference : the other functions in that module are described below :

val eval_expr : expr -> int NameTable.t -> int
                              
val eval_cond : cond -> int NameTable.t -> bool                    

val print_value_expr : expr -> int NameTable.t -> unit

val read_value : name -> int NameTable.t -> int NameTable.t
*)
