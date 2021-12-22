open Polish
module NameTable : Map.S with type key = name

val simplify_block : program -> int NameTable.t -> program
