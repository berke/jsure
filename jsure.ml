(* JSure *)

open Pffpsf;;

module Specs =
  struct
    open Arg;;
    open Opt;;
    open Source;;
    
    let aor x y = x := Some y;;

    let color x =
      Symbol(["black";"red";"green";"yellow";"blue";"magenta";"cyan";"white";"none"],
      begin fun y ->
        x :=
          match y with
            | "black"   -> Ansi.black
            | "red"     -> Ansi.red
            | "green"   -> Ansi.green
            | "yellow"  -> Ansi.yellow
            | "blue"    -> Ansi.blue
            | "magenta" -> Ansi.magenta
            | "cyan"    -> Ansi.cyan
            | "white"   -> Ansi.white
            | "none"    -> Ansi.uncoloured
            | _         -> raise (Bad "Bad color")
      end)
    ;;
    
    let treatment x =
      Symbol(["i";"w";"e"],
      begin function
        |"i" -> x := Ign
        |"w" -> x := Wrn
        |"e" -> x := Err
        |_ -> raise (Bad "Bad option")
      end)
    ;;

    let visualization x =
      Symbol(["t";"p"],
      begin function
        |"t" -> x := Txt
        |"p" -> x := Pos
        |_ -> raise (Bad "Bad option")
      end)
    ;;

    let specs =
      align [
        "-cache",
        String(aor cache),
        " Set database name for parsing cache";

        "-just-syntax",
        Set just_syntax,
        " Just check the syntax of input files";

        "-list-props",
        Set list_props,
        " List properties accessed";

        "-minefield",
        String(aor minefield),
        "<file> Attempt to detect misspellings of identifiers contained in this file";

        "-mine-distance",
        Set_float mine_distance,
        "<edits> Set minimum distance around mines";

        "-quiet",
        Set quiet,
        " Suppress informative messages";

        "-no-warnings",
        Set no_warnings,
        " Suppress warnings";

        "-errors",
        visualization error_visualization,
        " Show error positions or text bodies";

        "-warnings",
        visualization warning_visualization,
        " Show error warnings or text bodies";

        "-warnify",
        String(fun x -> warnify := x :: !warnify),
        "<prefix> Transform errors into warnings for files whose names start with the given prefix";

        "-ignorify",
        String(fun x -> ignorify := x :: !ignorify),
        "<prefix> Ignore errors and warnings for files whose names start with the given prefix";

        "-typecheck",
        Set typecheck,
        " Attempt to typecheck source files";

        "-forbidden-properties",
        String(aor forbidden_props_file),
        "<file> Load list of forbidden properties from this file";

        "-no-check",
        Clear check,
        " Disable basic semantic checks";

        "-check",
        Set check,
        " Perform basic semantic checks (on by default)";

        "-filenames",
        Set_string filename_regexp,
        "<regexp> Regexp describing valid source filenames";

        "-stdin",
        Set read_from_stdin,
        " Read Javascript source from stdin";

        "-unused-ident-regexp",
        Set_string unused_ident_regexp,
        "<regexp> Regular expression for lexically describing identifiers supposed to be unused";

        "-dangling-commas",
        treatment dangling_commas,
        " Dangling commas in object and array litterals";

        "-toplevel-bindings",
        treatment toplevel_bindings,
        " Setting global properties (without a \"var\")";

        "-assigning-to-args",
        treatment assigning_to_args,
        " Assigning to arguments";

        "-uninitialized-vars",
        treatment uninitialized_vars,
        " Uninitialized variables";

        "-unused-args",
        treatment unused_args,
        " Unused arguments";

        "-using-unused",
        treatment using_unused,
        " Using variables or arguments lexically declared to be unused";

        "-unused-vars",
        treatment unused_vars,
        " Unused variables";

        "-unused-funs",
        treatment unused_funs,
        " Unused functions";

        "-shadowing-args",
        treatment shadowing_args,
        " Arguments shadowed by variables";

        "-unreachable-code",
        treatment unreachable_code,
        " Unreachable code";

        "-bad-regexps",
        treatment bad_regexps,
        " Bad regular expression litterals";

        "-dump-env",
        Set dump_env,
        " Dump inferred environments while checking (very verbose)";

        "-dump-ast",
        Set dump_ast,
        " Dump AST";

        "-dump-raw-ast",
        Set dump_raw_ast,
        " Dump raw AST (marshalled Ocaml data structure)";

        "-dont-catch",
        Set dont_catch,
        " Don't catch internal errors (useful for debugging with OCAMLRUNPARAM=b)";

        "-generate",
        Set generate,
        " Generate clean JS output";

        "-info-color",
        color info_color,
        " Info color";

        "-warning-color",
        color warning_color,
        " Warning color";

        "-error-color",
        color error_color,
        " Error color";

        "-number-color",
        color line_number_color,
        " Line number color";

        "-code-color",
        color code_color,
        " Code color";

        "-hl-color",
        color code_hl_color,
        " Code highlight color";

        "-version",
        Unit(fun () ->
          let (v1,v2,v3) = Version.version in
          pf "jsure version %d.%d.%d\n" v1 v2 v3;
          exit 0),
        " Display Jsure version"
      ]
    ;;
  end
;;

exception Error of int;;

let argv_from_string u =
  let module G = Genlex in
  let lex = G.make_lexer ["=";";"] in
  let s = Stream.of_string u in
  let t = lex s in
  let r = ref [] in
  let loop1 = parser
    | [< '(G.String u|G.Ident u) >] -> u
    | [< '(G.Float f) >] -> sf "%f" f
    | [< '(G.Int i) >] -> sf "%d" i
    | [< '(G.Char c) >] -> sf "%c" c
  in
  let add_option u =
    for j = 0 to String.length u - 1 do
      if u.[j] = '_' then u.[j] <- '-'
    done;
    r := ("-"^u) :: !r
  in
  let rec loop0 = parser
    | [< 'G.Ident u; s >] ->
        begin
          match s with parser
          | [< 'G.Kwd"="; s >] -> add_option u; loop2 s
          | [< 'G.Kwd";"; s >] -> add_option u; loop0 s
        end
    | [< >] -> ()
  and loop2 = parser
    | [< '(G.Kwd";"); s >] -> loop0 s
    | [< y = loop1; s >] ->
        r := y :: !r;
        loop2 s
    | [< >] -> ()
  in
  try
    loop0 t;
    Stream.empty t;
    Array.of_list (List.rev !r)
  with
  | _ -> raise (Error(Stream.count s))
;;

let _ =
  let sources = ref [] in
  let current = ref 0 in
  let arg0 = [|Sys.argv.(0)|] in
  let argr = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let arg_from_env name =
    try
      let u = Sys.getenv name in
      try
        argv_from_string u
      with
      | Error i ->
          Printf.eprintf "Bad %s environment variable value at character %d.\n%!" name i;
          exit 2
    with
    | Not_found -> [||]
  in
  let arg_pre = arg_from_env "JSURE_BEFORE"
  and arg_post = arg_from_env "JSURE"
  in
  let argv = Array.concat [arg0; arg_pre; argr; arg_post] in
  begin
    try
      Arg.parse_argv
        ~current
        argv
        Specs.specs
        (fun x -> sources := x :: !sources)
        (Printf.sprintf "Usage: %s [options] <sources>" (Filename.basename Sys.argv.(0)));
    with
    | Arg.Help msg ->
        Printf.printf "%s%!" msg;
        exit 0
    | Arg.Bad msg ->
        Printf.printf "%s%!" msg;
        exit 2
  end;
  if !sources = [] && not !Opt.read_from_stdin then
    begin
      Printf.printf "No source files - maybe you should try the -stdin option?\n%!";
      exit 1
    end;
  let res = Process.sources stdout !sources in
  if not !Opt.no_warnings then
    List.iter
      begin fun w ->
        Printf.printf "%sWARNING: %s%s\n" Ansi.foreground.(!Opt.warning_color) w Ansi.none;
      end
      res.Process.res_warnings;
  let errors = ref false in
  List.iter
    begin fun e ->
      errors := true;
      Printf.printf "%sERROR: %s%s\n" Ansi.foreground.(!Opt.error_color) e Ansi.none;
    end
    res.Process.res_errors;
  exit (if !errors then 1 else 0)
;;
