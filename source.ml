(* Source *)

open Ast;;
open Conduit;;

type treatment = Ign|Wrn|Err;;
type view = Pos|Txt;;

type source = {
  s_file : string;
  s_text : string;
  s_source : program;
  s_liner : Liner.t;
  s_ignorify : bool;
  s_warnify : bool
};;

type position = int * int;;

module PS = Set.Make(struct type t = position let compare = compare end);;

(*** scribe_position *)
let scribe_position liner cd oc (i,j) =
  let j = j - 1 in
  let l_i = Liner.position_to_line liner i
  and l_j = Liner.position_to_line liner j
  in
  let (s_i, e_i) = Liner.line_to_range liner l_i
  and (s_j, _) = Liner.line_to_range liner l_j
  in
  let c_i = i - s_i
  and c_j = j - s_j
  in
  if l_i = l_j then
    if j = e_i - 1 then
      cd.cd_print oc "line %d" (l_i + 1)
    else
      if c_i = c_j then
        cd.cd_print oc "line %d, column %d" (l_i + 1) c_i
      else
        cd.cd_print oc "line %d, columns %d to %d" (l_i + 1) c_i c_j
  else
    cd.cd_print oc "line %d, column %d to line %d, column %d" (l_i + 1) c_i (l_j + 1) c_j
;;
(* ***)
(*** scribe_position_set *)
let scribe_position_set liner cd oc set =
  cd.cd_print oc "{";
  let first = ref true in
  PS.iter
    begin fun pos ->
      if !first then
        first := false
      else
        cd.cd_print oc ";";

      cd.cd_print oc " %a" (scribe_position liner cd) pos
    end
    set;
  cd.cd_print oc " }"
;;
(* ***)
