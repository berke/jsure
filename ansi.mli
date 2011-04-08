(* Ansi *)

val control : bool -> unit

val black : int
val red : int
val green : int
val yellow : int
val blue : int
val magenta : int
val cyan : int
val white : int
val uncoloured : int

val foreground : int -> string
val background : int -> string
val move_to : out_channel -> int -> unit
val none : unit -> string
