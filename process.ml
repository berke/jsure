(* Process *)

open Conduit;;
open Pffpsf;;
open Ast;;
open Source;;
exception Error of string

(*** result *)
type result = {
  mutable res_errors : string list;
  mutable res_warnings : string list
};;
(* ***)
(*** read_channel *)
let read_channel ic =
  let fd = Unix.descr_of_in_channel ic in
  let st = Unix.fstat fd in
  if st.Unix.st_kind = Unix.S_REG then
    begin
      let m = in_channel_length ic in
      let u = String.create m in
      really_input ic u 0 m;
      u
    end
  else
    begin
      let u = String.create 4096 in
      let b = Buffer.create 4096 in
      let rec loop () =
        let n = input ic u 0 (String.length u) in
        if n = 0 then
          Buffer.contents b
        else
          begin
            Buffer.add_substring b u 0 n;
            loop ()
          end
      in
      loop ()
    end
;;
(* ***)
(*** name_of_source *)
let name_of_source = function
  | `Stdin -> "*stdin*"
  | `File fn -> fn
;;
(* ***)
(*** read_source *)
let read_source = function
  | `Stdin -> read_channel stdin
  | `File fn ->
      let ic = open_in fn in
      let u = read_channel ic in
      close_in ic;
      u
;;
(* ***)
module SS = Set.Make(String);;
module SM = Map.Make(String);;

module PNS = Set.Make(struct type t = property_name let compare = compare end);;
module PNM = Map.Make(struct type t = property_name let compare = compare end);;

exception Exit;;

let position_of_info info = (info.i_start, info.i_end);;

let load_ast fn =
  let ic = open_in_bin fn in
  let t : program = Marshal.from_channel ic in
  close_in ic;
  t
;;

(*** load_strings_from_file *)
let load_strings_from_file fn =
  let ic = open_in fn in
  let set = ref SS.empty in
  try
    while true do
      let u = input_line ic in
      set := SS.add u !set
    done;
    assert false
  with
  | End_of_file ->
      close_in ic;
      !set
;;
(* ***)
(*** is_prefix *)
let is_prefix u v =
  let m = String.length u
  and n = String.length v
  in
  m <= n &&
    let rec loop i = i = m or u.[i] = v.[i] && loop (i + 1) in
    loop 0
;;
(* ***)
(*** need_to_warnify *)
let need_to_warnify fn = List.exists (fun prefix -> is_prefix prefix fn) !Opt.warnify;;
(* ***)
(*** need_to_ignorify *)
let need_to_ignorify fn = List.exists (fun prefix -> is_prefix prefix fn) !Opt.ignorify;;
(* ***)
(*** filename_rex *)
let filename_rex = lazy (Str.regexp !Opt.filename_regexp);;
(* ***)
(*** sources *)
let sources oc sl =
  let cd = stdoutcd in
  (* Check that all source files have a valid extension *)
  let result = { res_errors = []; res_warnings = [] } in
  let error u = result.res_errors <- u :: result.res_errors in
  let info u = if not !Opt.quiet then fp oc "%sINFO: %s%s\n%!" (Ansi.foreground !Opt.info_color) u (Ansi.none ()) in
  let warn u = result.res_warnings <- u :: result.res_warnings in
  let ignore_cd f = () in
  (*** error_cd *)
  let error_cd f =
    let b = Buffer.create 128 in
    let cd = conduit_of_buffer b in
    f cd cd.cd_out_channel;
    error (Buffer.contents b)
  in
  (* ***)
  (*** warn_cd *)
  let warn_cd f = warn (stringify f)
  in
  (* ***)
  (*** error_or_warn_cd *)
  let error_or_warn_cd fn =
    if need_to_ignorify fn then
      ignore_cd
    else
      if need_to_warnify fn then
        warn_cd
      else
        error_cd
  in
  (* ***)
  (*** info_cd *)
  let info_cd f = info (stringify f) in
  (* ***)
  let warn u = result.res_warnings <- u :: result.res_warnings in
  let have_errors () = result.res_errors <> [] in
  let exit_on_error () = if have_errors () then raise Exit in
  (*** check_extensions *)
  let check_extensions () =
    List.iter
      (fun fn ->
        if not (Str.string_match (Lazy.force filename_rex) fn 0) then
          error (sf "Filename %S does not conform to regexp %S." fn !Opt.filename_regexp))
      sl
  in
  (* ***)
  (*** compute_props *)
  let compute_props pg =
    let props = ref PNM.empty in
    (*** property *)
    let property info u =
      let set =
        try
         PNM.find u !props
        with
        | Not_found -> PS.empty
      in
      props := PNM.add u (PS.add (position_of_info info) set) !props
    in
    (* ***)
    (*** walk_expression *)
    let rec walk_expression assign info = function
      | Assign(x1, _, x2) -> walk_expression true info x1; walk_expression assign info x2
      | Sq xl -> List.iter (walk_expression assign info) xl
      | Function(_, _, (_, _, sl)) -> Ast.iter_over_expr_in_program info (walk_expression assign) sl
      | L _|V _|Extra _ -> ()
      | U(_, x) -> walk_expression assign info x
      | B(binop, x1, x2) ->
          walk_expression assign info x1;
          walk_expression assign info x2;
          begin
            match binop with
            | B_bracket ->
                begin
                  match x2 with
                  | L(String u) -> if assign (*or true*) then property info (PN_String u)
                  | _ -> ()
                end
            | _ -> ()
          end
       | Object pl ->
           List.iter
             begin fun (u, x) ->
               (*property u;*)
               walk_expression assign info x
             end
             pl
       | Array xl -> List.iter (walk_expression assign info) xl
       | Apply(x, xl) -> walk_expression assign info x; List.iter (walk_expression assign info) xl
       | Conditional(x1,x2,x3) ->
           walk_expression assign info x1;
           walk_expression assign info x2;
           walk_expression assign info x3
       | This -> ()
    in
    (* ***)
    Ast.iter_over_expr_in_program Ast.info0 (walk_expression false) pg;
    !props
  in
  (* ***)
  (*** list_props *)
  let list_props liner props =
    PNM.iter
      begin fun pn set ->
        stdoutcd.cd_print stdoutcd.cd_out_channel "  %a : %a\n" (scribe_property_name cd) pn (scribe_position_set liner cd) set
      end
      props
  in
  (* ***)
  (*** minefield *)
  let minefield =
    match !Opt.minefield with
    | None -> None
    | Some fn ->
      Some
        begin
          let mf = Minefield.create () in
          let set = 
            info (sf "Loading minefield from %S" fn);
            load_strings_from_file fn
          in
          SS.iter (fun u -> Minefield.add_word mf (String.lowercase u)) set;
          mf
        end
  in
  (* ***)
  (*** is_property_forbidden *)
  let is_property_forbidden =
    match !Opt.forbidden_props_file with
    | None -> fun _ -> false
    | Some fn ->
        let set =
          info (sf "Loading forbidden properties from %S" fn);
          load_strings_from_file fn
        in
        fun u -> SS.mem u set
  in
  (* ***)
  (*** is_misspelling *)
  let is_misspelling =
    match minefield with
    | None -> fun _ -> None
    | Some mf -> fun u ->
      let u = String.lowercase u in
      let m = !Opt.mine_distance *. log (float (String.length u)) /. log 2.0 in
      match Minefield.check_proximity mf u with
      | None -> None
      | Some(d,v) as x ->
        if d < m then
          x
        else
          None
  in
  (* ***)
  (*** check_forbidden_props *)
  let check_forbidden_props fn liner props =
    let report = error_or_warn_cd fn in
    PNM.iter
      begin fun pn set ->
        match pn with
        | PN_String u ->
            begin
              match is_misspelling u with
              | None -> ()
              | Some(d,v) ->
                warn_cd (fun cd oc -> 
                   cd.cd_print oc "Property %a is possible misspelling of %S (d=%f), used in file %S at %a"
                     (scribe_property_name cd) pn
                     v
                     d
                     fn
                     (scribe_position_set liner cd) set)
            end;
            if is_property_forbidden u then
              report (fun cd oc -> 
                 cd.cd_print oc "Forbidden property %a used in file %S at %a"
                   (scribe_property_name cd) pn
                   fn
                   (scribe_position_set liner cd) set)
        | _ -> ()
      end
      props
  in
  (* ***)
  let sources = ref [] in
  (*** check_file *)
  let cache = match !Opt.cache with
    | None -> None
    | Some fn -> Some(Cache.create ~version:Ast.version fn)
  in
  let check_source src =
    info "Loading";
    let fn = name_of_source src in
    let u = read_source src in
    let liner = Liner.create ~offset:!Opt.line_number_offset u in
    try
      (*** reparse *)
      let reparse () =
        info "Parsing";
        let t = Ecma.parse u in
        (*** !Opt.dump_ast *)
        if !Opt.dump_ast then
          begin
            let fn' = (Filename.chop_extension fn)^".xml" in
            info (sf "Dumping AST into file %S" fn');
            let oc = open_out fn' in
            Ecma.print_tree oc t;
            close_out oc
          end;
        (* ***)
        t
      in
      (* ***)
      (*** t *)
      let t =
        match cache with
        | None -> reparse ()
        | Some c ->
          try
            let t = Cache.get c fn u in
            info (sf "Cached parse tree for %S" fn);
            t
          with
          | Not_found ->
            let t = reparse () in
            Cache.set c fn u t;
            t
      in
      (* ***)
      if !Opt.just_syntax then
        info (sf "File %S has valid syntax" fn)
      else
        begin
          (*** pg *)
          let pg =
            begin
              info "Converting AST";
              try
                let pg = Convert.convert t in
                info "AST OK";
                (*** !Opt.dump_raw_ast *)
                if !Opt.dump_raw_ast then
                  begin
                    let fn' = (Filename.chop_extension fn)^".bin" in
                    info (sf "Dumping raw AST into file %S" fn');
                    let oc = open_out_bin fn' in
                    Marshal.to_channel oc pg [];
                    close_out oc
                  end;
                (* ***)
                pg
              with
              | (Convert.Bad_tree t) as x -> pf "Bad tree:\n%a\n" Ecma.print_tree t; raise x
              | (Convert.Bad_trees ts) as x ->
                  pf "Bad trees:\n";
                  List.iter (fun t -> Ecma.print_tree stdout t) ts;
                  raise x
            end
          in
          (* ***)
          let source = { s_file = fn;
                         s_text = u;
                         s_source = pg;
                         s_liner = liner;
                         s_warnify = need_to_warnify fn;
                         s_ignorify = need_to_ignorify fn }
          in
          sources := source :: !sources;
          let properties = compute_props pg in
          if !Opt.list_props then
            begin
              fp oc "Properties in file %S:\n" fn;
              list_props liner properties;
              fp oc "End of properties in file %S.\n%!" fn
            end;
          check_forbidden_props fn liner properties;
          if !Opt.generate then
            begin
              info "Generating";
              let fn' = (Filename.chop_extension fn)^".gen.js" in
              let oc = open_out fn' in
              let f = Format.formatter_of_out_channel oc in
              Generate.generate f pg;
              Format.fprintf f "@."
            end;
        end
    with
    | Aurochs_pack.Aurochs.Parse_error x ->
        error_cd
          (fun cd oc ->
            cd.cd_print
            oc
            "In file %S, parse error at %a:\n%s"
            fn
            (scribe_position liner cd)
            (x, x + 1)
            (Excerpt.excerpt liner u x (x + 1)))
    | x -> 
        if !Opt.dont_catch then
          raise x
        else
          error (sf "In file %S, unlocated parse error %s." fn (Printexc.to_string x))
  in
  (* ***)
  (*** close_cache *)
  let close_cache () =
    match cache with
    | None -> ()
    | Some c -> Cache.close c
  in
  (* ***)
  try
    check_extensions ();
    let sl = List.map (fun fn -> `File fn) sl in
    let sl =
      if !Opt.read_from_stdin then
        `Stdin :: sl
      else
        sl
    in
    exit_on_error ();
    List.iter (fun src ->
      let fn = name_of_source src in
      info (sf "Processing file %S" fn);
      check_source src;
      info (sf "Done with file %S" fn)) sl;
    if !Opt.typecheck then
      begin
        info "Typechecking";
        Eval.evaluate ~warn ~warn_cd ~error ~error_cd !sources
      end;
    if !Opt.check then
      begin
        info "Checking";
        Check.check ~dump_cd:cd ~info_cd ~warn ~warn_cd ~error ~error_cd !sources
      end;
    { res_errors = List.rev result.res_errors; res_warnings = List.rev result.res_warnings }
  with
  | x ->
    close_cache ();
    match x with
    | Exit -> result
    | _ -> raise x
;;
(* ***)
