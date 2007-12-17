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
let check = ref true;;
let typecheck = ref false;;
let mine_distance = ref 5.0;;
let unused_ident_regexp = ref "^_.*_$";;
let safe_property_regexp = ref "[a-zA-Z_][a-zA-Z0-9_]*";;
let filename_regexp = ref ".*\\.js";;
let generate = ref false;;
let read_from_stdin = ref false;;
let line_number_offset = ref 0;;

let undefined_variables  = ref Ign;;
let unbound_variables    = ref Wrn;;
let toplevel_bindings    = ref Ign;;
let assigning_to_args    = ref Ign;;
let unused_vars          = ref Wrn;;
let unused_funs          = ref Wrn;;
let unused_args          = ref Wrn;;
let using_unused         = ref Wrn;;
let uninitialized_vars   = ref Wrn;;
let shadowing_args       = ref Wrn;;
let unreachable_code     = ref Wrn;;
let dangling_commas      = ref Err;;
let bad_regexps          = ref Err;;

let error_visualization   = ref Txt;;
let warning_visualization = ref Txt;;

let info_color        = ref Ansi.green;;
let warning_color     = ref Ansi.yellow;;
let error_color       = ref Ansi.red;;
let line_number_color = ref Ansi.blue;;
let code_color        = ref Ansi.uncoloured;;
let code_hl_color     = ref Ansi.red;;

let cache : string option ref = ref None;;
