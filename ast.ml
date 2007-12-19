(* AST *)

open Conduit;;

type info = {
  i_start : int;
  i_end : int;
  i_extra : bool
};;

let version = 8;;

type program = source_element list
and source_element =
| St of int * int * st
| FunDecl of int * int * func
and block = st list
and st =
| Position of int * int * st
| Expr of expr
| If of expr * st * st option
| Do of st * expr
| While of expr * st
| For of st option * st option * st option * st
| Continue of label option
| Break of label option
| Return of expr option
| With of expr * st
| Labeled of label * st
| Switch of expr * (case_clause list * st) list
| Throw of expr
| Try of st * (arg * st) option * st option
| Variable of variable_declaration list
| Block of st list
| ForIn of lhs_or_var * expr * st
| Nop
and lhs_or_var =
| LHS of expr
| Vars of variable_declaration list
and variable_declaration = name * expr option
and case_clause = Default | Case of expr
and func = name option * name list * source_element list
and extra = int * int * extra_tag
and extra_tag =
| DanglingComma
and expr =
| Assign of expr * assignment_operator * expr
| Sq of expr list
| Function of int * int * func
| L of litteral
| U of unop * expr
| B of binop * expr * expr
| V of name
| Object of (property_name * expr) list
| Array of array_litteral
| Apply of expr * expr list
| Conditional of expr * expr * expr
| This
| Extra of extra
and property_name =
| PN_String of string
| PN_Float of float
| PN_Int of int32
| PN_Empty
and binop =
| B_mul
| B_div
| B_mod
| B_add
| B_sub
| B_le
| B_ge
| B_lt
| B_gt
| B_instanceof
| B_in
| B_equal
| B_lsr
| B_asr
| B_lsl
| B_notequal
| B_physequal
| B_physnotequal
| B_bitand
| B_bitor
| B_bitxor
| B_and
| B_or
| B_bracket
and unop =
| U_bitnot
| U_delete
| U_void
| U_typeof
| U_pre_increment
| U_pre_decrement
| U_post_increment
| U_post_decrement
| U_plus
| U_minus
| U_not
| U_new
and assignment_operator =
| A_eq
| A_mul
| A_div
| A_mod
| A_add
| A_sub
| A_lsl
| A_lsr
| A_asr
| A_and
| A_xor
| A_or
and litteral =
| Float of float
| Int of int32
| String of string
| Regexp of string * string
| Bool of bool
| Null
| Undefined
and array_litteral = expr list
and name = string
and arg = string
and label = string

let info0 = { i_start = 0; i_end = max_int; i_extra = true };;

let rec iter_over_expr_in_program info f p = List.iter (iter_over_expr_in_source_element info f) p
and iter_over_expr_in_source_element info f = function
| St(start_pos, end_pos, s) -> iter_over_expr_in_st { info with i_start = start_pos; i_end = end_pos } f s
| FunDecl(_,_,(_,_,sl)) -> iter_over_expr_in_program info f sl
and iter_over_expr_in_sto info f = function
| None -> ()
| Some s -> iter_over_expr_in_st info f s
and iter_over_expr_in_variable_declaration_list info f vl =
  List.iter (fun (_, xo) ->
    match xo with
    | None -> ()
    | Some x -> f info x) vl
and iter_over_expr_in_st info f = function
| Position(start_pos, end_pos, s) -> iter_over_expr_in_st { info with i_start = start_pos; i_end = end_pos } f s
| Expr x -> f info x
| If(x, s1, so) -> f info x; iter_over_expr_in_st info f s1;
  begin
    match so with
    | None -> ()
    | Some s -> iter_over_expr_in_st info f s
  end
| Do(s, x)|While(x,s)|With(x,s) -> iter_over_expr_in_st info f s; f info x
| For(so1, so2, so3, s) ->
    iter_over_expr_in_sto info f so1;
    iter_over_expr_in_sto info f so2;
    iter_over_expr_in_sto info f so3;
    iter_over_expr_in_st info f s
| Return(Some x)|Throw x -> f info x
| Continue _|Break _|Return None -> ()
| Labeled(_, s) -> iter_over_expr_in_st info f s
| Switch(x, cls) ->
    f info x;
    List.iter
      begin fun (cl, s) ->
        iter_over_expr_in_st info f s;
        List.iter
          begin function
            | Default -> ()
            | Case x -> f info x
          end
          cl
      end
      cls
| Try(s, aso, so) ->
    iter_over_expr_in_st info f s;
    begin
      match aso with
      | None -> ()
      | Some(_, s) -> iter_over_expr_in_st info f s
    end;
    begin
      match so with
      | None -> ()
      | Some s -> iter_over_expr_in_st info f s
    end
| Variable vl -> iter_over_expr_in_variable_declaration_list info f vl
| Block sl -> List.iter (iter_over_expr_in_st info f) sl
| ForIn(l, x, s) ->
    f info x;
    iter_over_expr_in_lhs info f l;
    iter_over_expr_in_st info f s
| Nop -> ()
and iter_over_expr_in_lhs info f = function
| LHS x -> f info x
| Vars vl -> iter_over_expr_in_variable_declaration_list info f vl
;;

(*** scribe_property_name *)
let scribe_property_name cd oc = function
| PN_String u -> cd.cd_print oc "%S" u
| PN_Float f -> cd.cd_print oc "%f" f
| PN_Int x -> cd.cd_print oc "%ld" x
| PN_Empty -> cd.cd_print oc "*empty*"
;;
(* ***)
