(* Opt *)

open Source;;

let dump_ast = ref false;;
let dump_raw_ast = ref false;;
let dump_env = ref false;;
let just_syntax = ref false;;
let dont_catch = ref false;;
let list_props = ref false;;
let quiet = ref false;;
let no_warnings = ref false;;
let warnify : string list ref = ref [];;
let ignorify : string list ref = ref [];;
let minefield : string option ref = ref None;;
let forbidden_props_file : string option ref = ref None;;
let check = ref false;;
let typecheck = ref false;;
let mine_distance = ref 5.0;;
let unused_ident_regexp = ref "^..*_$";;
let safe_property_regexp = ref "^[a-zA-Z_][a-zA-Z0-9_]*$";;
let generate = ref false;;

let toplevel_bindings  = ref Err;;
let assigning_to_args  = ref Wrn;;
let unused_vars        = ref Err;;
let unused_funs        = ref Err;;
let unused_args        = ref Wrn;;
let using_unused       = ref Err;;
let uninitialized_vars = ref Wrn;;
let shadowing_args     = ref Wrn;;
let unreachable_code   = ref Wrn;;
let dangling_commas    = ref Wrn;;
let bad_regexps        = ref Err;;

let error_visualization   = ref Txt;;
let warning_visualization = ref Txt;;

let info_color        = ref Ansi.green;;
let warning_color     = ref Ansi.yellow;;
let error_color       = ref Ansi.red;;
let line_number_color = ref Ansi.blue;;
let code_color        = ref Ansi.white;;
let code_hl_color     = ref Ansi.red;;

let cache : string option ref = ref None;;
