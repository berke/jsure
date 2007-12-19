(* Convert *)

open Aurochs_pack;;
open Peg;;
open Ast;;
open Ecma;;

exception Bad_tree of Ecma.tree;;
exception Bad_trees of Ecma.tree list;;

let int32_of_string u = Int64.to_int32 (Int64.of_string u);;

let rec convert = function
  | Node(N_Root, _, [Node(N_Program, _, sl)]) -> convert_source_elements sl
  | t -> raise (Bad_tree t)
and convert_source_elements sl = List.map convert_source_element sl
and convert_source_element = function
  | Node(N_St, [A_start, start_pos; A_end, end_pos], [st]) -> St(int_of_string start_pos, int_of_string end_pos, convert_statement st)
  | Node(N_FunDecl, [A_start, start_pos; A_name, name; A_end, end_pos], [Node(N_Args, [], al); Node(N_Body, _, sl)]) ->
      FunDecl(int_of_string start_pos, int_of_string end_pos, (Some name, List.map convert_arg al, convert_source_elements sl))
  | t -> raise (Bad_tree t)
and convert_arg = function
  | Node(N_Arg, [A_name, name], []) -> name
  | t -> raise (Bad_tree t)
and convert_block stl = Block(List.map convert_statement stl)
and convert_case = function
  | Node(N_Case, [], [Node(N_Clauses, [], clauses); Node(N_Body, [], body)]) ->
      (List.map convert_clause clauses, convert_block body)
  | t -> raise (Bad_tree t)
and convert_clause = function
  | Node(N_Default, [], []) -> Default
  | Node(N_When, [], [x]) -> Case(convert_expression x)
  | t -> raise (Bad_tree t)
and convert_statement = function
  | Node(N_Position, [A_start, start_pos; A_end, end_pos], [st]) -> Position(int_of_string start_pos, int_of_string end_pos, convert_statement st)
  | Node(N_Break, [], []) -> Break None
  | Node(N_Break, [A_label, label], []) -> Break(Some label)
  | Node(N_Continue, [], []) -> Continue None
  | Node(N_Label, [A_name, label], [Node(N_Body, [], [st])]) -> Labeled(label, convert_statement st)
  | Node(N_Continue, [A_label, label], []) -> Continue(Some label)
  | Node(N_Throw, [], [x]) -> Throw(convert_expression x)
  | Node(N_Switch, [], x :: cases) -> Switch(convert_expression x, List.map convert_case cases)
  | Node(N_With, [], [x; Node(N_Body, [], [s])]) -> With(convert_expression x, convert_statement s)
  | Node(N_EmptyStatement, [], []) -> Nop
  | Node(N_ExpressionStatement, [], [x]) -> Expr(convert_expression x)
  | Node(N_Block, [], sl) -> convert_block sl
  | Node(N_Return, [], [x]) -> Return(Some(convert_expression x))
  | Node(N_Return, [], []) -> Return None
  | Node(N_VariableStatement, [], vl) -> Variable(List.map convert_variable_declaration vl)
  | Node(N_If, [], [Node(N_Condition, [], [x]);
                    Node(N_True, [], [s1]);
                    Node(N_False, [], [s2])]) ->
      If(convert_expression x, convert_statement s1, Some(convert_statement s2))
  | Node(N_If, [], [Node(N_Condition, [], [x]); Node(N_True, [], [s])]) ->
      If(convert_expression x, convert_statement s, None)
  | Node(N_Do, [], [Node(N_Loop, [], [s]); Node(N_Condition, [], [x])]) ->
      Do(convert_statement s, convert_expression x)
  | Node(N_While, [], [Node(N_Condition, [], [x]); Node(N_Loop, [], [s])]) ->
      While(convert_expression x, convert_statement s)
  | Node(N_ForIn, [], [Node(N_Index, [], [x]);
                       Node(N_Container, [], [c]);
                       Node(N_Loop, [], [s])]) ->
      ForIn(convert_lhs_or_var x, convert_expression c, convert_statement s)
  | Node(N_For, [], z) ->
      begin
        let start, rest =
          match z with
          | Node(N_Start, [], [s]) :: rest -> Some(convert_statement s), rest
          | _ -> None, z
        in
        let condition, rest =
          match rest with
          | Node(N_Condition, [], [s]) :: rest -> Some(convert_statement s), rest
          | _ -> None, rest
        in
        let next, rest =
          match rest with
          | Node(N_Next, [], [s]) :: rest -> Some(convert_statement s), rest
          | _ -> None, rest
        in
        match rest with
        | [Node(N_Loop, [], [s])] -> For(start, condition, next, convert_statement s)
        | ts -> raise (Bad_trees ts)
      end
  | Node(N_Try, [], (Node(N_Block, [], block)) :: rest) as t ->
      let block = convert_block block in
      begin
        let catch, rest =
          match rest with
          | (Node(N_Catch, [], [Node(N_Arg, [A_name, name] , []); Node(N_Body, [], block)])) :: rest -> Some(name, convert_block block), rest
          | _ -> None, rest
        in
        let finally =
          match rest with
          | [Node(N_Finally, [], block)] -> Some(convert_block block)
          | [] -> None
          | _ -> raise (Bad_tree t)
        in
        Try(block, catch, finally)
      end
  | t -> raise (Bad_tree t)
and convert_lhs_or_var = function
  | Node(N_LHS, [], [x]) -> LHS(convert_expression x)
  | Node(N_Vars, [], vl) -> Vars(List.map convert_variable_declaration vl)
  | t -> raise (Bad_tree t)
and convert_variable_declaration = function
  | Node(N_VariableDeclaration, [A_name, name], []) -> (name, None)
  | Node(N_VariableDeclaration, [A_name, name], [x]) -> (name, Some(convert_expression x))
  | t -> raise (Bad_tree t)
and convert_expression = function
  | Node(N_Expression, [], xl) -> Sq(List.map convert_expression xl)
  | Node(N_Array, [], xl) -> Array(List.map convert_expression xl)
  | Node(N_Function, [A_start, start_pos; A_name, name; A_end, end_pos], [Node(N_Args, [], al); Node(N_Body, _, sl)]) ->
      Function(int_of_string start_pos, int_of_string end_pos, (Some name, List.map convert_arg al, convert_source_elements sl))
  | Node(N_Function, [A_start, start_pos; A_end, end_pos], [Node(N_Args, [], al); Node(N_Body, _, sl)]) ->
      Function(int_of_string start_pos, int_of_string end_pos, (None, List.map convert_arg al, convert_source_elements sl))
  | Node(N_String, [A_value, v], []) -> L(String v)
  | Node(N_Regexp, (A_body, body) :: options', []) as t ->
      let options = match options' with
        | [] -> ""
        | [A_options, options] -> options
        | _ -> raise (Bad_tree t)
      in
      L(Regexp(body, options))
  | Node(N_Integer, [A_value, n], []) -> L(Int(int32_of_string n))
  | Node(N_Float, [A_value, f], []) -> L(Float(float_of_string f))
  | Node(N_Shift, [], [x1;Node(N_Asr, _, _);x2]) -> B(B_asr, convert_expression x1, convert_expression x2)
  | Node(N_Shift, [], [x1;Node(N_Lsr, _, _);x2]) -> B(B_lsr, convert_expression x1, convert_expression x2)
  | Node(N_Shift, [], [x1;Node(N_Lsl, _, _);x2]) -> B(B_lsl, convert_expression x1, convert_expression x2)
  | Node(N_Add, [], [x1;x2]) -> B(B_add, convert_expression x1, convert_expression x2)
  | Node(N_Sub, [], [x1;x2]) -> B(B_sub, convert_expression x1, convert_expression x2)
  | Node(N_Mul, [], [x1;x2]) -> B(B_mul, convert_expression x1, convert_expression x2)
  | Node(N_Div, [], [x1;x2]) -> B(B_div, convert_expression x1, convert_expression x2)
  | Node(N_Mod, [], [x1;x2]) -> B(B_mod, convert_expression x1, convert_expression x2)
  | Node(N_Or, [], [x1;x2]) -> B(B_or, convert_expression x1, convert_expression x2)
  | Node(N_And, [], [x1;x2]) -> B(B_and, convert_expression x1, convert_expression x2)
  | Node(N_BitXor, [], [x1;x2]) -> B(B_bitxor, convert_expression x1, convert_expression x2)
  | Node(N_BitAnd, [], [x1;x2]) -> B(B_bitand, convert_expression x1, convert_expression x2)
  | Node(N_BitOr, [], [x1;x2]) -> B(B_bitor, convert_expression x1, convert_expression x2)
  | Node(N_Conditional, [], [x1;x2;x3]) -> Conditional(convert_expression x1, convert_expression x2, convert_expression x3)
  | Node(N_Assign, [], [x1;op;x2]) -> Assign(convert_expression x1, convert_assignment_operator op, convert_expression x2)
  | Node(N_True, [], []) -> L(Bool true)
  | Node(N_False, [], []) -> L(Bool false)
  | Node(N_Null, [], []) -> L Null
  | Node(N_This, [], []) -> This
  | Node(N_Undefined, [], []) -> L Undefined
  | Node(N_BitNot, [], [x]) -> U(U_bitnot, convert_expression x)
  | Node(N_Delete, [], [x]) -> U(U_delete, convert_expression x)
  | Node(N_Void, [], [x]) -> U(U_void, convert_expression x)
  | Node(N_TypeOf, [], [x]) -> U(U_typeof, convert_expression x)
  | Node(N_Plus, [], [x]) -> U(U_plus, convert_expression x)
  | Node(N_Minus, [], [x]) -> U(U_minus, convert_expression x)
  | Node(N_Not, [], [x]) -> U(U_not, convert_expression x)
  | Node(N_New, [], [x]) -> U(U_new, convert_expression x)
  | Node(N_Increment, [], [Node(N_Pre, [], []); x]) -> U(U_pre_increment, convert_expression x)
  | Node(N_Decrement, [], [Node(N_Pre, [], []); x]) -> U(U_pre_decrement, convert_expression x)
  | Node(N_Increment, [], [Node(N_Post, [], []); x]) -> U(U_post_increment, convert_expression x)
  | Node(N_Decrement, [], [Node(N_Post, [], []); x]) -> U(U_post_decrement, convert_expression x)
  | Node(N_Equality, [], [x1; Node(op, [], []); x2]) as t->
      let y1 = convert_expression x1 in
      let y2 = convert_expression x2 in
      B(
        begin
          match op with
          | N_Equal -> B_equal
          | N_NotEqual -> B_notequal
          | N_PhysicalEqual -> B_physequal
          | N_NotPhysicalEqual -> B_physnotequal
          | _ -> raise (Bad_tree t)
        end,
        y1,
        y2)
  | Node(N_Compare, [], [x1; Node(rel, [], []); x2]) as t->
      let y1 = convert_expression x1 in
      let y2 = convert_expression x2 in
      B(
        begin
          match rel with
          | N_Le -> B_le
          | N_Ge -> B_ge
          | N_Lt -> B_lt
          | N_Gt -> B_gt
          | N_In -> B_in
          | N_InstanceOf -> B_instanceof
          | _ -> raise (Bad_tree t)
        end,
        y1,
        y2)
  | Node(N_Access, [], x :: rest) ->
      let y = convert_expression x in
      convert_access y rest
  | Node(N_Var, [A_name, name], []) -> V name
  | Node(N_Object, [], pl) -> Object(List.map convert_property pl)
  | Node(N_DanglingComma, [A_start, start_pos; A_end, end_pos], []) -> Extra(int_of_string start_pos, int_of_string end_pos, DanglingComma)
  | t -> raise (Bad_tree t)
and convert_property_name = function
  | Node(N_Ident, [A_name, name], []) | Node(N_String, [A_value, name], []) -> PN_String name
  | Node(N_Integer, [A_value, value], []) -> PN_Int(int32_of_string value)
  | Node(N_Float, [A_value, value], []) -> PN_Float(float_of_string value)
  | t -> raise (Bad_tree t)
and convert_property = function
  | Node(N_Property, [], [pn; x]) -> (convert_property_name pn, convert_expression x)
  | Node(N_DanglingComma, [A_start, start_pos; A_end, end_pos], []) -> (PN_Empty, Extra(int_of_string start_pos, int_of_string end_pos, DanglingComma))
  | t -> raise (Bad_tree t)
and convert_assignment_operator = function
  | Node(N_Eq, [], []) -> A_eq
  | Node(N_Mul, [], []) -> A_mul
  | Node(N_Div, [], []) -> A_div
  | Node(N_Mod, [], []) -> A_mod
  | Node(N_Add, [], []) -> A_add
  | Node(N_Sub, [], []) -> A_sub
  | Node(N_Lsl, [], []) -> A_lsl
  | Node(N_Lsr, [], []) -> A_lsr
  | Node(N_Asr, [], []) -> A_asr
  | Node(N_And, [], []) -> A_and
  | Node(N_Xor, [], []) -> A_xor
  | Node(N_Or, [], []) -> A_or
  | t -> raise (Bad_tree t)
and convert_access y = function
  | [] -> y
  | Node(N_Apply, [], args) :: rest -> convert_access (Apply(y, List.map convert_expression args)) rest
  | Node(N_Bracket, [], [x]) :: rest -> convert_access (B(B_bracket, y, convert_expression x)) rest
  | Node(N_Dot, [A_field, field], []) :: rest -> convert_access (B(B_bracket, y, L(String field))) rest
  | ts -> raise (Bad_trees ts)
;;
