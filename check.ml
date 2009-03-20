(* Check *)

open Conduit;;
open Pffpsf;;
open Ast;;
open Source;;
exception Error of string

module SS = Set.Make(String);;
module SM = Map.Make(String);;
module PNM = Map.Make(struct type t = property_name let compare = compare end);;

let max_depth = 5;;

type t =
  | T_Function
  | T_Variable
  | T_Argument
  | T_Undefined
;;

type value = {
  mutable v_value : t;
  v_level : int;
  mutable v_usage : int;
  v_start : int;
  v_end : int;
  mutable v_warned_unused : bool;
}

type scope =
  | S_Toplevel
  | S_Function of string option
;;

type 'a dictionary = 'a SM.t ref;;

type env = {
  env_bindings : value dictionary option;
  env_with : bool;
  env_obj : value dictionary option;
  env_start : int;
  env_end : int;
  env_scope : scope;
  env_level : int;
  mutable env_source : Source.source option
};;

exception Error of string;;
exception At of int * int * exn;;

let create_dict () = ref SM.empty;;
let copy_dictionary d = ref !d;;
let copy_dictionary_option = function
| None -> None
| Some d -> Some(copy_dictionary d)
;;
(*** is_supposedly_unused *)
let is_supposedly_unused =
  let rex = lazy (Str.regexp !Opt.unused_ident_regexp) in
  fun n -> Str.string_match (Lazy.force rex) n 0
;;
(* ***)
(*** rex_options_rx *)
let rex_options_rx1 = Str.regexp "^[giwm]*$";;
let rex_options_rx2 = Str.regexp "^.*\\(g.*g\\|i.*i\\|m.*m\\).*$";;
(* ***)
(*** copy_env *)
let copy_env env = {
  env with
  env_bindings = copy_dictionary_option env.env_bindings;
  env_obj = copy_dictionary_option env.env_obj;
  env_level = env.env_level + 1
};;
(* ***)
(*** check *)
let check ~dump_cd ~info_cd ~warn ~warn_cd ~error ~error_cd sources =
  let cd = dump_cd in

  let oc = cd.cd_out_channel in
  let print = cd.cd_print in
  let env = {
    env_bindings = Some(create_dict ());
    env_obj = Some(create_dict ());
    env_start = 0;
    env_end = 0;
    env_with = false;
    env_source = None;
    env_scope = S_Toplevel;
    env_level = 0
  }
  in
  let informer v f env x =
    f begin fun cd oc ->
        match env.env_source with
        | None -> cd.cd_print oc "In *UNKNOWN LOCATION*: %s" x
        | Some s ->
            match v with
            | Pos -> cd.cd_print oc "In file %S at %a: %s" s.s_file (scribe_position s.s_liner cd) (env.env_start, env.env_end) x
            | Txt ->
                (* Show source... *)
                cd.cd_print
                  oc
                  "In file %S at %a: %s:\n%s"
                  s.s_file
                  (scribe_position s.s_liner cd)
                  (env.env_start, env.env_end)
                  x
                  (Excerpt.excerpt s.s_liner s.s_text env.env_start env.env_end)
      end
  in
  let warn env x =
    match env.env_source with
    | None -> informer !Opt.warning_visualization error_cd env x
    | Some s ->
      if s.s_ignorify then
        ()
      else
        informer !Opt.warning_visualization warn_cd env x
  and error env x =
    match env.env_source with
    | None -> informer !Opt.error_visualization error_cd env x
    | Some s ->
      if s.s_ignorify then
        ()
      else
        if s.s_warnify then
          informer !Opt.warning_visualization warn_cd env x
        else
          informer !Opt.error_visualization error_cd env x
  in
  (* Create default objects *)
  (*** dump_env *)
  let dump_env env = 
    cd.cd_print oc "{ with=%b %d--%d level=%d \n" env.env_with env.env_start env.env_end env.env_level;
    begin
      match env.env_bindings with
      | None -> cd.cd_print oc "  (no bindings)\n"
      | Some d ->
        SM.iter
          begin fun n v ->
            cd.cd_print oc "    %s : %s, level=%d usage=%d\n"
              n
              (match v.v_value with T_Function -> "fun" | T_Argument -> "arg" | T_Variable -> "var" | T_Undefined -> "und")
              v.v_level
              v.v_usage
          end
          !d
    end;
    cd.cd_print oc "}\n";
  in
  (* ***)
  (*** treatment *)
  let treatment var env u =
    match !var with
    | Ign -> ()
    | Wrn -> warn env u
    | Err -> error env u
  in
  (* ***)
  (*** find *)
  let find env name =
    match env.env_bindings with
    | Some e -> SM.find name !e
    | None -> raise Not_found
  in
  (* ***)
  (*** assign *)
  let assign env name value =
    if !Opt.dump_env then
      begin
        print oc "Assigning %s in environment:\n" name;
        dump_env env
      end;
    match env.env_bindings with
    | Some d ->
        d := SM.add name
              { v_value = value;
                v_usage = 0;
                v_level = env.env_level;
                v_start = env.env_start;
                v_end = env.env_end;
                v_warned_unused = false } !d
    | None -> ()
  in
  (* ***)
  (*** finish_env *)
  let finish_env env =
    match env.env_bindings with
    | None -> ()
    | Some d ->
        SM.iter
          begin fun n v ->
            if v.v_level = env.env_level && v.v_usage = 0 && not (is_supposedly_unused n) && not v.v_warned_unused then
              begin
                v.v_warned_unused <- true;
                let env = { env with env_start = v.v_start; env_end = v.v_end } in
                match v.v_value with
                | T_Argument -> treatment Opt.unused_args env (sf "Unused argument %S" n)
                | T_Undefined | T_Variable -> treatment Opt.unused_vars env (sf "Unused identifier %S" n)
                | T_Function -> treatment Opt.unused_funs env (sf "Unused function %S" n)
              end
          end
          !d
  in
  (* ***)
  let rec check_source_element env = function
    | St(start_pos, end_pos, s) ->
        let env' = { env with env_start = start_pos; env_end = end_pos } in
        check_statement ~toplevel:true env' s
    | FunDecl(start_pos, end_pos, func) ->
        let env' = { env with env_start = start_pos; env_end = end_pos } in
        begin
          match func with
          | (Some name, _, _) ->
              assign env' name T_Function;
              check_function env' func
          | (None, _, _) -> error env "Anonymous function declaration"
        end
  and check_function env (no, al, sl) =
    let env' = copy_env env in
    List.iter
      begin fun n ->
        assign env' n T_Argument;
      end
      al;
    List.iter (check_source_element env') sl;
    finish_env env'
  and check_statement_option env = function
    | None -> ()
    | Some s -> check_statement env s
  and check_expr_as_lhs ?(toplevel=false) env = function
    | V name ->
        begin
          try
            if is_supposedly_unused name then treatment Opt.using_unused env (sf "Supposedly unused identifier %S assigned to" name);
            let v = find env name in
            v.v_usage <- v.v_usage + 1;
            match v.v_value with
            | T_Function -> ()
            | T_Variable -> ()
            | T_Argument -> treatment Opt.assigning_to_args env (sf "Assigning to argument %S" name)
            | T_Undefined -> v.v_value <- T_Variable
          with
          | Not_found ->
              (* See if this is a top-level assignment *)
              if toplevel then
                treatment Opt.toplevel_bindings env (sf "Toplevel binding to %S" name)
              else
                if env.env_with then
                  ()
                else
                  treatment Opt.unbound_variables env (sf "Unbound variable %s" name)
        end
    | B(B_bracket, x1, x2) ->
      check_expr env x1;
      check_expr env x2;
      begin
        match x2 with
          | L(String _u) -> ()
          | _ -> ()
      end
    | _ -> warn env "Possibly invalid LHS"
  and check_expr ?(toplevel=false) (env : env) = function
    | Extra(start_pos, end_pos, DanglingComma) -> treatment Opt.dangling_commas { env with env_start = start_pos; env_end = end_pos } "Dangling comma"
    | Apply(x, xl) ->
      check_expr env x;
      List.iter (check_expr env) xl
    | Assign(x1, _, x2) ->
      check_expr_as_lhs ~toplevel env x1;
      check_expr env x2;
    | Sq xl -> List.iter (check_expr env) xl
    | L(Regexp(x, o)) ->
        begin
          try
            ignore (Ecmarex.parse x);
            if not (Str.string_match rex_options_rx1 o 0)
               or  (Str.string_match rex_options_rx2 o 0) then
              treatment Opt.bad_regexps env (sf "Bad regular expression options %S" o)
          with
          | Aurochs_pack.Aurochs.Parse_error n ->
              let msg =
                if n = 0 then
                  sf "%S  <-wtf?" x
                else
                  if n = String.length x then
                    sf "wtf?->  %S" x
                  else
                    sf "%S  <-wtf?->  %S" (String.sub x 0 n) (String.sub x n (String.length x - n))
              in
              treatment Opt.bad_regexps env (sf "Invalid regular expression: %s" msg)
        end
    | L(Float _) | L(Int _) | L(String _) | L(Bool _) | L Null | L Undefined  -> ()
    | Conditional(x1, x2, x3) ->
        check_expr env x1;
        check_expr env x2;
        check_expr env x3
    | V name ->
      begin
        try
          if is_supposedly_unused name then treatment Opt.using_unused env (sf "Supposedly unused identifier %S used" name);
          let v = find env name in
          v.v_usage <- 1 + v.v_usage;
          match v.v_value with
          | T_Undefined -> treatment Opt.uninitialized_vars env (sf "Variable %S may be used while undefined" name)
          | _ -> ()
        with
        | Not_found -> 
            treatment Opt.undefined_variables env (sf "Variable %s could be undefined" name)
      end
    | U(_, x) -> check_expr env x
    | B(_, x1, x2) ->
      begin
        check_expr env x1;
        check_expr env x2
      end
    | This -> ()
    | Array xl -> List.iter (check_expr env) xl
    | Object pl -> List.iter (fun (_, x) -> check_expr env x) pl
    | Function(start_pos, end_pos, func) -> check_function { env with env_start = start_pos; env_end = end_pos } func
  and check_statement ?(toplevel=false) env st =
    if !Opt.dump_env then
      begin
        print oc "Statement in environment:\n";
        dump_env env
      end;
    match st with
    | Position(start_pos, end_pos, s) ->
        let env' = { env with env_start = start_pos; env_end = end_pos } in
        check_statement ~toplevel env' s
    | Expr x -> check_expr ~toplevel env x
    | If(x, s, so) ->
        check_expr env x;
        check_statement env s;
        check_statement_option env so
    | Do(s, x) ->
        check_statement env s;
        check_expr env x
    | While(x, s) ->
        check_expr env x;
        check_statement env s
    | For(so1, so2, so3, s) ->
        check_statement_option env so1;
        check_statement_option env so2;
        check_statement_option env so3;
        check_statement env s
    | ForIn(lv, x, st) ->
        let env' = copy_env env in
        check_lhs_or_var ~forin:true env' lv;
        check_expr env' x;
        check_statement env' st
    | Throw x -> check_expr env x
    | Continue _ -> () (* XXX Check labels *)
    | Break _ -> () (* XXX Check labels *)
    | Labeled _ -> ()
    | Return None -> ()
    | Return(Some x) -> ignore (check_expr env x)
    | With(x, s) ->
        check_expr env x;
        let env' = copy_env env in
        let env' = { env' with env_with = true }  in
        check_statement env' s;
        finish_env env'
    | Variable vl -> List.iter (check_variable_declaration env) vl
    | Block sl ->
      let env' = copy_env env in
      List.iter (check_statement env') sl;
      finish_env env'
    | Nop -> ()
    | Try(s, catch, so) ->
        check_statement env s;
        begin
          match catch with
          | None -> ()
          | Some(n, s) ->
            let env' = copy_env env in
            assign env' n T_Argument;
            check_statement env' s;
            finish_env env'
        end;
        begin
          match so with
          | None -> ()
          | Some s -> check_statement env s
        end
     | Switch(x, cl) ->
       check_expr env x;
       List.iter
         begin fun (cl, s) ->
           List.iter
             begin function
             | Default -> ()
             | Case x -> check_expr env x
             end
             cl;
           check_statement env s
         end
         cl
  and check_lhs_or_var ?(forin=false) env = function
  | LHS x -> check_expr_as_lhs env x
  | Vars vl -> List.iter (check_variable_declaration ~forin env) vl
  and check_variable_declaration ?(forin=false) env (name, xo) =
    begin
      try
        let v = find env name in
        match v.v_value with
        | T_Argument -> treatment Opt.shadowing_args env (sf "Variable %s shadows argument of the same name" name)
        | _ -> ()
      with
      | Not_found -> ()
    end;
    assign env name
      begin match xo with
        | None ->
          if forin then
            T_Variable
          else
            T_Undefined
        | Some x -> check_expr env x; T_Variable
      end
  in
  List.iter
    begin fun s ->
      env.env_source <- Some s;
      try
        List.iter (check_source_element env) s.s_source
      with
      | At(start_pos, end_pos, Error e) ->
        error_cd (fun cd oc -> cd.cd_print oc "Error in file %S at %a: %s" s.s_file (scribe_position s.s_liner cd) (start_pos, end_pos) e)
    end
    sources
;;
(* ***)
