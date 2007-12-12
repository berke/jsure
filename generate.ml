(* Generate *)

open Ast;;

let fp = Format.fprintf;;

type context =
| Inner
| Top
;;

let is_property_safe =
  let rex = lazy (Str.regexp !Opt.safe_property_regexp) in
  fun n -> Str.string_match (Lazy.force rex) n 0
;;

let generate f pg =
  let comma () =
    let first = ref true in
    fun () ->
      if !first then
        first := false
      else
        fp f ",@ "
  in
  let semi f colon =
    match colon with
    | None -> ()
    | Some c -> fp f "%c" c
  in
  let rec source_element = function
  | St(_, _, st) ->
      fp f "@\n";
      statement ~colon:';' st
  | FunDecl(_, _, (Some(n), al, sl)) ->
      fp f "function %s(" n;
      let c = comma () in
      List.iter (fun a -> c (); fp f "%s" a) al;
      fp f ")@ ";
      block sl
  | _ -> fp f "/* FunDecl */"
  and program pg = List.iter source_element pg
  and block sl =
    fp f "@[<v2>{@\n";
    program sl;
    fp f "@]@\n}"
  and statement_block ?colon sl =
    fp f "@[<hov2>{";
    List.iter (fun s -> statement ~colon:';' s) sl;
    fp f "@]@\n}%a" semi colon
  and statement ?colon = function
    | Throw x -> fp f "throw "; expression Inner x; fp f "%a@\n" semi colon
    | Labeled(lb, s) -> fp f "%s:@\n" lb; statement ?colon s
    | Break None -> fp f "break%a@\n" semi colon
    | Break(Some lb) -> fp f "break %s%a@\n" lb semi colon
    | Continue None -> fp f "continue%a@\n" semi colon
    | Continue(Some lb) -> fp f "break %s%a@\n" lb semi colon
    | With(x, s) ->
        fp f "with(";
        expression Top x;
        fp f ")@ ";
        statement ?colon s
    | While(x, s) ->
        fp f "while(";
        expression Top x;
        fp f ")@ ";
        statement ?colon s
    | Position(_,_,st) -> statement ?colon st
    | Expr x -> expression Top x; fp f "%a@\n" semi colon
    | Variable [] -> () (* XXX *)
    | Variable vl ->
        vars ?colon vl
    | If(x, s, so) ->
        fp f "@[<hov2>@[<h>if(";
        expression Top x;
        fp f ")@]@\n";
        begin match so with
          | None ->
              statement s;
              fp f "@]%a" semi colon
          | Some s ->
              statement ~colon:';' s;
              fp f "@]";
              fp f "@ else @[<v2>";
              statement s;
              fp f "@]"
        end
    | Nop -> ()
    | Return(Some x) -> fp f "return@ "; expression Inner x; fp f "%a@\n" semi colon
    | Return None -> fp f "return %a@\n" semi colon
    | ForIn(LHS x1, x2, st) ->
        fp f "@[<hov2>for(";
        expression Inner x1;
        fp f " in ";
        expression Inner x2;
        fp f ")";
        statement ?colon st;
        fp f "@\n@]"
    | ForIn(Vars vdl, x2, st) ->
        fp f "@[<hov2>for(";
        vars vdl;
        fp f " in ";
        expression Inner x2;
        fp f ")";
        statement ?colon st;
        fp f "@\n@]"
    | For(so1, so2, so3, s) ->
        fp f "for(";
        statement_option so1;
        fp f ";@ ";
        statement_option so2;
        fp f ";@ ";
        statement_option so3;
        fp f ")@ ";
        statement s
    | Block sl ->
        fp f "@\n";
        statement_block ?colon sl
    | Try(st, co, fo) ->
        fp f "@[<hov2>try";
        statement st;
        fp f "@\n";
        begin match co with
          | None -> ()
          | Some(a, st) ->
              fp f "@\ncatch(%s)" a;
              statement st
        end;
        begin match fo with
          | None -> ()
          | Some st ->
              fp f "@\nfinally";
              statement st
        end
    | Do(s, x) ->
        fp f "do";
        statement s;
        fp f "while(";
        expression Inner x;
        fp f ")%a" semi colon
     | Switch(x, cls) ->
        fp f "@[<hov2>switch(";
        expression Inner x;
        fp f ") {";
        List.iter
          begin fun (cl, s) ->
            fp f "@\n";
            List.iter clause cl;
            fp f "@\n";
            statement ~colon:';' s
          end
          cls;
        fp f "@\n}@]";
  and vars ?colon vl =
    fp f "@[<hov2>var ";
    let c = comma () in
    List.iter
      begin fun x ->
        c ();
        match x with
        | (n, None) -> fp f "@\n%s" n
        | (n, Some x) -> fp f "@\n%s = " n; expression Top x
      end
      vl;
    fp f "%a" semi colon
  and clause = function
    | Default -> fp f "@\ndefault:"
    | Case x -> fp f "@\ncase "; expression Inner x; fp f ":"
  and statement_option ?colon = function
    | None -> semi f colon
    | Some s -> statement ?colon s
  and expression context x =
    match context, x with
    | _, This -> fp f "this"
    | _, Extra _ -> ()
    | Top, Assign(x1, op, x2) ->
        fp f "@[<hov2>";
        expression Top x1;
        fp f " ";
        begin
          match op with
          | A_eq -> fp f "="
          | A_mul -> fp f "*="
          | A_div -> fp f "/="
          | A_mod -> fp f "%%="
          | A_add -> fp f "+="
          | A_sub -> fp f "-="
          | A_lsl -> fp f "<<="
          | A_lsr -> fp f ">>="
          | A_asr -> fp f ">>>="
          | A_and -> fp f "&="
          | A_or -> fp f "|="
          | A_xor -> fp f "^="
        end;
        fp f "@ ";
        expression Top x2;
        fp f "@]"
    | Top, Sq xl -> List.iter (expression Top) xl
    | _, L l -> litteral l
    | _, V n -> fp f "%s" n
    | Top, Apply(x1, xl) ->
        fp f "@[<hv2>";
        expression Inner x1;
        fp f "(";
        let c = comma () in
        List.iter (fun x -> c (); expression Inner x) xl;
        fp f ")@]"
    | _, Array xl ->
        let c = comma () in
        fp f "@[<h>[";
        List.iter (fun x -> c (); expression Inner x) xl;
        fp f "]@]";
    | Top, Conditional(x1, x2, x3) ->
        fp f "@[<h>";
        expression Top x1;
        fp f " ?@ ";
        expression Top x2;
        fp f " :@ ";
        expression Top x3;
        fp f "@]"
    | Top, Object pl ->
        let c = comma () in
        fp f "@\n@[<hov2>{";
        List.iter
          begin fun (pn, x) ->
            c ();
            fp f "@\n";
            begin
              match pn with
              | PN_Int x -> fp f "%d" x
              | PN_Float x -> fp f "%f" x
              | PN_String x when is_property_safe x -> fp f "%s" x
              | PN_String x -> fp f "%S" x
              | PN_Empty -> fp f "*empty*"
            end;
            fp f ": ";
            expression Inner x
          end
          pl;
        fp f "@\n@]}"
    | _, Function(_, _, func) -> generate_function func
    | _, B(B_bracket, x1, L(String u)) when is_property_safe u ->
        expression Top x1;
        fp f ".%s" u
    | Top, B(B_bracket, x1, x2) ->
        fp f "@[<h>";
        expression Inner x1;
        fp f "[";
        expression Top x2;
        fp f "]@]"
    | Top, B(op, x1, x2) ->
        fp f "@[<h>";
        expression Inner x1;
        let u =
          match op with
          | B_add -> "+"
          | B_mul -> "*"
          | B_sub -> "-"
          | B_div -> "/"
          | B_equal -> "=="
          | B_notequal -> "!="
          | B_physequal -> "==="
          | B_physnotequal -> "!=="
          | B_bitand -> "&"
          | B_bitor -> "|"
          | B_bitxor -> "^"
          | B_and -> "&&"
          | B_or -> "||"
          | B_le -> "<="
          | B_lt -> "<"
          | B_ge -> ">="
          | B_gt -> ">"
          | B_instanceof -> "instanceof"
          | B_in -> "in"
          | _ -> "/* operator */"
        in
        fp f " %s@ " u;
        expression Inner x2;
        fp f "@]"
    | Top, U(U_post_increment, x) -> expression Inner x; fp f " ++"
    | Top, U(U_post_decrement, x) -> expression Inner x; fp f " --"
    | Top, U(op, x) ->
        let u =
          match op with
          | U_new -> "new"
          | U_delete -> "delete"
          | U_typeof -> "typeof"
          | U_pre_increment -> "++"
          | U_pre_decrement -> "--"
          | U_plus -> "+"
          | U_minus -> "-"
          | U_not -> "!"
          | _ -> assert false
        in
        fp f "%s " u;
        expression Inner x
    | Inner, _ -> fp f "(@["; expression Top x; fp f "@])"
  and generate_function (no, al, sl) =
    fp f "function";
    begin
      match no with
      | None -> fp f "("
      | Some n -> fp f " %s(" n
    end;
    fp f "@[<h>";
    let c = comma () in
    List.iter
      begin fun n ->
        c ();
        fp f "%s" n
      end
      al;
    fp f "@])@ ";
    block sl
  and litteral = function
    | Float x -> fp f "%f" x
    | Int x -> fp f "%d" x
    | String s -> fp f "%S" s
    | Regexp(r, o) -> fp f "/%s/%s" r o
    | Bool b -> fp f "%b" b
    | Null -> fp f "null"
    | Undefined -> fp f "undefined"
  in
  program pg
;;
