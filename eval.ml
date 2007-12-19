(* Eval *)

open Conduit;;
open Pffpsf;;
open Ast;;
open Source;;
exception Error of string

module SS = Set.Make(String);;
module SM = Map.Make(String);;
module PNM = Map.Make(struct type t = property_name let compare = compare end);;

type 'func t =
| T_String of string option
| T_Float of float option
| T_Int of int32 option
| T_Regexp
| T_Bool of bool option
| T_Null
| T_Undefined
| T_Object of 'func dictionary option
| T_Component of 'func t option
| T_Array of 'func t array option
| T_Function of ('func * 'func dictionary) option
| T_Variable of string
| T_Property of 'func t * property_name
| T_Unknown
and 'func dictionary = 'func t PNM.t ref
;;

let max_depth = 5;;

let rec scribe_t ?(depth=0) cd oc x =
  if depth = max_depth then
    cd.cd_print oc ".."
  else
    match x with
    | T_String(Some u) -> cd.cd_print oc "<String %S>" u
    | T_String _ -> cd.cd_print oc "<String>"
    | T_Float _ -> cd.cd_print oc "<Float>"
    | T_Int _ -> cd.cd_print oc "<Int>"
    | T_Bool _ -> cd.cd_print oc "<Bool>"
    | T_Component(Some y) -> cd.cd_print oc "<Component %a>" (scribe_t ~depth:(depth + 1) cd) y
    | T_Component None -> cd.cd_print oc "<Component>"
    | T_Object(Some o) -> cd.cd_print oc "<Object %a>" (scribe_dictionary ~depth:(depth + 1) cd) o;
    | T_Object None -> cd.cd_print oc "<Object>"
    | T_Array _ -> cd.cd_print oc "<Array>"
    | T_Null -> cd.cd_print oc "<Null>"
    | T_Regexp -> cd.cd_print oc "<Regexp>"
    | T_Undefined -> cd.cd_print oc "<Undefined>"
    | T_Function(None) -> cd.cd_print oc "<Function>"
    | T_Function(Some((None, _, _), d)) -> cd.cd_print oc "<Function (anon) %a>" (scribe_dictionary ~depth:(depth + 1) cd) d;
    | T_Function(Some((Some n, _, _), d)) -> cd.cd_print oc "<Function %S %a>" n (scribe_dictionary ~depth:(depth + 1) cd) d;
    | T_Variable v -> cd.cd_print oc "<Var %s>" v
    | T_Property(v, pn) -> cd.cd_print oc "<Property %a.%a>" (scribe_t ~depth:(depth + 1) cd) v (scribe_property_name cd) pn
    | T_Unknown -> cd.cd_print oc "<Unknown>"
and scribe_dictionary ?(depth=0) cd oc o =
  if depth = max_depth then
    cd.cd_print oc ".."
  else
    PNM.iter
      begin fun x y ->
        cd.cd_print oc "%a: %a," (scribe_property_name  cd) x (scribe_t ~depth:(depth + 1) cd) y
      end
      !o
;;

type func = name option * name list * source_element list;;

type env = {
  env_bindings : func dictionary option;
  env_obj : func dictionary option;
  env_start : int;
  env_end : int;
  mutable env_filename : string
};;

exception Result of func t;;
exception Error of string;;
exception At of int * int * exn;;

let create_dict () = ref PNM.empty;;
let create_object () = T_Object(Some(create_dict ()));;
let copy_dictionary d = ref !d;;
let copy_dictionary_option = function
| None -> None
| Some d -> Some(copy_dictionary d)
;;
let copy_env env = { env with env_bindings = copy_dictionary_option env.env_bindings; env_obj = copy_dictionary_option env.env_obj };;

let evaluate ~warn ~warn_cd ~error ~error_cd sources =
  let env = {
    env_bindings = Some(ref PNM.empty);
    env_obj = Some(ref PNM.empty);
    env_start = 0;
    env_end = 0;
    env_filename = "*NONE*"
  }
  in
  (*** set_property *)
  let set_property d pn x =
    warn_cd (fun cd oc -> cd.cd_print oc "Assign %a <- %a" (scribe_property_name cd) pn (scribe_t cd) x);
    d := PNM.add pn x !d
  in
  (* ***)
  (*** get_property *)
  let get_property d pn =
    warn_cd (fun cd oc -> cd.cd_print oc "Get %a" (scribe_property_name cd) pn);
    try
      PNM.find pn !d
    with
    | Not_found ->
      match pn with
      | PN_String "prototype" -> create_object ()
      | _ -> T_Unknown
        (*raise (Error(stringify (fun cd oc -> cd.cd_print oc "Property %a not found" (scribe_property_name cd) pn)))*)
  in
  (* ***)
  (*** set_object_property *)
  let set_object_property o pn x =
    match o with
    | T_Function(Some(_, d)) | T_Object(Some d) -> set_property d pn x
    | T_Object None -> warn "Setting property on unknown object"
    | T_Unknown -> warn "Setting property on unknown value"
    | _ -> raise (Error("Attempt to set property on a non-object"))
  in
  (* ***)
  (*** assign *)
  let assign env name value =
    match env.env_bindings with
    | Some d -> set_property d name value
    | None -> ()
  in
  (* ***)
  (* Create default objects *)
  let find env name =
    match env.env_bindings with
    | Some e -> PNM.find name !e
    | None -> raise Not_found
  in
  let rec eval_source_element env = function
    | St(start_pos, end_pos, s) ->
        begin
          try
            eval_statement env s
          with
          | Error e as x -> raise (At(start_pos, end_pos, x))
        end
    | FunDecl(start_pos, end_pos, (None, _, _)) -> raise (At(start_pos, end_pos, (Error("Anonymous function declaration"))))
    | FunDecl(start_pos, end_pos, (Some name, args, body)) -> assign env (PN_String name) (T_Function(Some((Some name, args, body), create_dict ())))
  and check_numerics l =
    match List.fold_left unify T_Unknown l with
    | (T_Int _|T_Float _) as t -> t
    | _ -> raise (Error(sf "Not a number"))
  and eval_statement_option env = function
    | None -> ()
    | Some s -> eval_statement env s
  and unify y1 y2 =
    if y1 = y2 then
      y1
    else
      match y1, y2 with
      | T_String _, T_String _ -> T_String None
      | (T_Float _, (T_Float _|T_Int _)) | (T_Int _, T_Float _) -> T_Float None
      | T_Int _, T_Int _ -> T_Int None
      | T_Bool _, T_Bool _ -> T_Bool None
      | T_Null, T_Null -> T_Null
      | _, T_Undefined|T_Undefined, _ -> T_Undefined
      | T_Unknown, x|x, T_Unknown -> x
      | _, _ -> T_Unknown
  and eval_expr_as_lhs env = function
    | V name -> T_Variable name
    | B(B_bracket, x1, x2) ->
      let y1 = eval_expr env x1 in
      let y2 = eval_expr env x2 in
      begin
        match y2 with
        | T_String(Some u) -> T_Property(y1, PN_String u)
        | _ -> T_Unknown
      end
    | _ -> T_Unknown
  and eval_expr (env : env) = function
    | Extra _ -> T_Undefined
    | Apply(B (B_bracket, V "Class", L (String "create")), xl) ->
      create_object ()
    | Apply(B (B_bracket, V "Component", L (String "create")), xl) ->
      begin
        warn (sf "Found Component.create, %d args" (List.length xl));
        (*let y = eval_expr env x in
        T_Component(Some y)*)
        T_Component None
      end
    | Apply(x, xl) ->
      let y = eval_expr env x in
      begin
        match y with
        | T_Function None -> T_Unknown
        | T_Function(Some((no, nl, bd),d)) ->
          let xll = List.length xl
          and nll = List.length nl
          in
          if xll = nll then
            begin
              let env' = copy_env env in
              List.iter2
                begin fun n x ->
                  let y = eval_expr env x in
                  assign env' (PN_String n) y
                end
                nl
                xl;
              try
                List.iter (eval_source_element env') bd;
                T_Undefined
              with
              | Result t -> t
            end
          else
            T_Unknown (* raise (Error (sf "Wrong number of arguments, expected %d, got %d" nll xll)) *)
        | _ -> T_Unknown
        (*x -> raise (Error(stringify (fun cd oc -> cd.cd_print oc "Not a function (%a)" (scribe_t cd) x)))*)
      end
    | Assign(x1, op, x2) ->
      begin match op with
        | A_eq ->
          let y1 = eval_expr_as_lhs env x1 in
          let y2 = eval_expr env x2 in
          begin
            match y1 with
            | T_Variable v -> assign env (PN_String v) y2
            | T_Property(y, pn) ->
              warn (stringify (fun cd oc -> cd.cd_print oc "Setting property %a" (scribe_property_name cd) pn));
              set_object_property y pn y2
            | _ -> warn "Unresolved assignment"
          end;
          y2
        | _ -> T_Int None
      end
    | Sq xl ->
      let rec loop = function
      | [x] -> eval_expr env x
      | [] -> T_Undefined
      | x :: rest -> ignore (eval_expr env x); loop rest
      in
      loop xl
    | L(Float f) -> T_Float(Some f)
    | L(Int x) -> T_Int(Some x)
    | L(String u) -> T_String(Some u)
    | L(Regexp _) -> T_Regexp
    | L(Bool b) -> T_Bool(Some b)
    | L Null -> T_Null
    | L Undefined -> T_Undefined
    | Conditional(x1, x2, x3) ->
        ignore (eval_expr env x1);
        unify (eval_expr env x2) (eval_expr env x3)
    | V name ->
      begin
        try
          find env (PN_String name)
        with
        | Not_found ->
            warn (sf "Can't find variable %s" name);
            T_Unknown
      end
    | U(op, x) ->
      begin
        let y = eval_expr env x in
        match op,y with
        | (U_pre_increment | U_pre_decrement | U_post_increment | U_post_decrement | U_plus | U_minus | U_bitnot), _ -> check_numerics [y]
        | U_not, T_Bool(Some b) -> T_Bool(Some(not b))
        | U_not, T_Null -> T_Bool(Some true)
        | U_not, _ -> T_Bool None
        | U_delete, _ -> T_Null (* XXX *)
        | _, _ -> T_Unknown
      end
    | B(op, x1, x2) ->
      begin
        let y1 = eval_expr env x1
        and y2 = eval_expr env x2
        in
        match op,y1,y2 with
        | (B_mul | B_div | B_mod | B_sub | B_bitand | B_bitor | B_bitxor), _, _ -> check_numerics [y1]
        | (B_and | B_or | B_le | B_ge | B_lt | B_gt | B_equal | B_notequal | B_physequal | B_physnotequal | B_instanceof), _, _ -> T_Bool None
        | B_bracket, T_Object(Some d), T_String(Some u) ->
              get_property d (PN_String u)
        | _, _, _ -> T_Unknown
      end
    | This -> T_Object(env.env_obj)
    | Array xl -> T_Array(Some(Array.of_list (List.map (eval_expr env) xl)))
    | Object pl ->
      let dict = create_dict () in
      List.iter
        begin fun (pn, x) ->
          let y = eval_expr env x in
          set_property dict pn y
        end
        pl;
      T_Object(Some dict)
    | Function(_, _, (no, al, sl)) -> T_Function(Some( (no, al, sl), create_dict () ))
  and eval_statement : env -> st -> unit = fun env st -> match st with
    | Position(start_pos, end_pos, s) ->
        begin
          try
            eval_statement env s
          with
          | Error e as x -> raise (At(start_pos, end_pos, x))
        end
    | Expr x -> ignore (eval_expr env x)
    | If(x, s, so) ->
        let _ = eval_expr env x in
        eval_statement env s;
        eval_statement_option env so
    | Do(s, x) ->
        eval_statement env s;
        ignore (eval_expr env x)
    | While(x, s) ->
        ignore (eval_expr env x);
        eval_statement env s
    | For(so1, so2, so3, s) ->
        eval_statement_option env so1;
        eval_statement_option env so2;
        eval_statement_option env so3;
        eval_statement env s
    | Continue _ -> () (* XXX Check labels *)
    | Break _ -> () (* XXX Check labels *)
    | Return None -> ()
    | Return(Some x) -> ignore (eval_expr env x)
    | With(This, s) ->
        let env' = { env with env_bindings = env.env_obj }  in
        eval_statement env' s
    | With(x, s) ->
        () (* XXX *)
    | Variable vl -> List.iter (eval_variable_declaration env) vl
    | _ -> ()
  and eval_variable_declaration env (name, xo) =
    let y =
      match xo with
      | None -> T_Undefined
      | Some x -> eval_expr env x
    in
    assign env (PN_String name) y
  in
  List.iter
    begin fun s ->
      env.env_filename <- s.s_file;
      try
        List.iter (eval_source_element env) s.s_source
      with
      | At(start_pos, end_pos, Error e) ->
        error_cd (fun cd oc -> cd.cd_print oc "Error in file %S at %a: %s" s.s_file (scribe_position s.s_liner cd) (start_pos, end_pos) e)
    end
    sources
;;
