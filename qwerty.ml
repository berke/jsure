(* Qwerty *)
(* Written by Berke DURAK, released in the public Domain. *)

let row y0 x0 w u1 u2 =
  let m = String.length u1 in
  let r = ref [] in
  let _shift_distance = 10.0 in
  for i = 0 to m - 1 do
    r := (y0, x0 +. (float_of_int i) *. w, 0.0, u1.[i])::!r;
    r := (y0, x0 +. (float_of_int i) *. w, 10.0, u2.[i])::!r
  done;
  !r
;;

let qwerty_description = List.concat [
  row 0.0 0.0 1.7461
    "`1234567890-="
    "~!@#$%^&*()_+";
  [1.5,1.0,0.0,'\t'];
  row 1.5 2.8 1.7461 "qwertyuiop[]" "QWERTYUIOP{}";
  row 3.0 3.4 1.7461 "asdfghjkl;'\\" "ASDFGHJKL:\"|";
  row 4.5 2.8 1.7461 "<zxcvbnm,./" ">ZXCVBNM<>?";
  [6.0,12.5,0.0,' ']
];;

let qwerty_dummy = (20.0,4.0,0.5);;

let qwerty_map =
  let a = Array.make 256 qwerty_dummy in
  List.iter (fun (x,y,z,c) -> a.(Char.code c) <- (x,y,z)) qwerty_description;
  a
;;

let euclidian_distance (x1,y1,z1) (x2,y2,z2) =
  let f a1 a2 = (a1 -. a2) *. (a1 -. a2) in
  sqrt ((f x1 x2) +. (f y1 y2) +. (f z1 z2))
;;

let qwerty_insertion_cost = 4.0
let qwerty_deletion_cost = qwerty_insertion_cost
;;

let qwerty_distance c1 c2 =
  let p c = qwerty_map.(Char.code c) in
  match (c1,c2) with
    None,None -> 0.0
  | Some(_),None -> qwerty_deletion_cost
  | None,Some(_) -> qwerty_insertion_cost
  | Some(c1),Some(c2) ->
      euclidian_distance (p c1) (p c2)
;;
