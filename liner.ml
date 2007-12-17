(* Liner *)

(*** t *)
type t = {
  l_length : int;
  l_table : (int * int) array;
  l_offset : int
};;
(* ***)
(*** create *)
let create ?(offset=0) u =
  let m = String.length u in
  let t = ref [] in
  let line = ref 0 in
  for i = 0 to m do
    if i < m && u.[i] = '\n' or i = m then
      begin
        t := (!line, i) :: !t;
        incr line
      end
  done;
  { l_length = m;
    l_table  = Array.of_list (List.rev !t);
    l_offset = offset }
;;
(* ***)
(*** line_to_range *)
let line_to_range l ?(offset=l.l_offset) i =
  let i = i - offset in
  let (_, end_pos) = l.l_table.(i) in
  if i = 0 then
    (0, end_pos)
  else
    let (_, start_pos) = l.l_table.(i - 1) in
    (start_pos + 1, end_pos)
;;
(* ***)
(*** position_to_line *)
let position_to_line l j =
  let m = Array.length l.l_table in
  let in_line i =
    let (start_pos, end_pos) = line_to_range l ~offset:0 i in
    start_pos <= j && j <= end_pos
  in
  let rec loop i0 m =
    if m = 0 then
      raise Not_found
    else
      begin
        if m < 8 then
          if in_line i0 then
            i0
          else
            loop (i0 + 1) (m - 1)
        else
          let i = i0 + m / 2 in
          let (start_pos, end_pos) = line_to_range l ~offset:0 i in
          if start_pos <= j && j <= end_pos then
            i
          else
            if j < start_pos then
              loop i0 (m / 2)
            else
              loop (i + 1) (m - m / 2 - 1)
      end
  in
  l.l_offset + loop 0 m
;;
(* ***)
(*** test *)
let test () =
  let u = "alpha\nbeta\ngamma delta\nepsilon\n\nphi\n" in
  let l = create u in
  for i = 0 to String.length u - 1 do
    let j = position_to_line l i in
    let (s,e) = line_to_range l j in
    Printf.printf "%d -> #%d [%d,%d]\n%!" i j s e
  done
;;
(* ***)
