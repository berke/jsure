(* Ansi *)

open Pffpsf;;

let enabled    = ref true;;

let control x = enabled := x

let filter u = if !enabled then u else "";;

let black      = 0;;
let red        = 1;;
let green      = 2;;
let yellow     = 3;;
let blue       = 4;;
let magenta    = 5;;
let cyan       = 6;;
let white      = 7;;
let uncoloured = 8;;

let foreground_array = [|
  "\027[30m";
  "\027[31m";
  "\027[32m";
  "\027[33m";
  "\027[34m";
  "\027[35m";
  "\027[36m";
  "\027[37m";
  "\027[0m";
|];;

let foreground i = filter foreground_array.(i);;

let background_array = [|
  "\027[40m";
  "\027[41m";
  "\027[42m";
  "\027[43m";
  "\027[44m";
  "\027[45m";
  "\027[46m";
  "\027[47m";
  "\027[0m";
|];;

let background i = filter background_array.(i);;

let move_to oc x = if !enabled then fp oc "\r\027[%dC\027[K" x else ();;
let up () = filter "\027[1A"
let ceol () = filter "\027[K"
let home () = filter "\r"
let none () = filter "\027[0m"
;;
