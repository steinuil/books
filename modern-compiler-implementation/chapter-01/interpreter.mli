val maxargs : Grammar.stm -> int

val interp : Grammar.stm -> unit

val interp' : (string -> unit) -> Grammar.stm -> unit
