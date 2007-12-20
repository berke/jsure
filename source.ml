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
(*** slice_left *)
let slice_left u i =
  let m = String.length u in
  if i <= 0 then
    ("", u)
  else
    if i >= m then
      (u, "")
    else
      (String.sub u 0 i, String.sub u i (m - i))
;;
(* ***)
(*** slice_right *)
let slice_right u i =
  let m = String.length u in
  if i <= 0 then
    (u, "")
  else
    if i >= m then
      ("", u)
    else
      (String.sub u 0 (m - i), String.sub u (m - i) i)
;;
(* ***)
(*** extract_lines *)
let extract_lines liner text start_pos end_pos =
  let l_i = Liner.position_to_line liner start_pos
  and l_j = Liner.position_to_line liner (end_pos - 1)
  in
  let m = l_j - l_i + 1 in
  Array.init m
    begin fun i ->
      let l = l_i + i in
      let (x, y) = Liner.line_to_range liner l in
      let u = String.sub text x (y - x) in
      let (prefix, u) =
        if l = l_i then
          let c_i = start_pos - x in
          slice_left u c_i
        else
          "", u
      in
      let (u, suffix) =
        if l = l_j then
          let c_j = y - end_pos in
          slice_right u c_j
        else
          u, ""
      in
      (l, prefix, u, suffix)
    end
(* ***)
