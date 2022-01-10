open Polish

val simplify_block : program -> int NameTable.t -> program
(*[simplify_block block env] returns the simplified block.
  env contains the current values of the variables that have
  been set to an int. *)

val simplify_expr : expr -> int NameTable.t -> expr
(*[simplify_expr e env] returns the simplified expression. *)