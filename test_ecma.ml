module M = Ecma_parser;;

let read_file fn =
  let ic = open_in fn in
  let m = in_channel_length ic in
  let u = String.create m in
  really_input ic u 0 m;
  close_in ic;
  u
;;

let _ =
  let fn = Sys.argv.(1) in
  let u = read_file fn in
  let t = M.parse u in
  M.print_tree stdout t
;;

let parsing_prefixes u =
  let m = String.length u in
  for i = 1 to m - 1 do
    let v = String.sub u 0 i in
    try
      let _t = M.parse v in
      Printf.printf ">> %d\n%!" i
    with
    | _ -> ()
  done
;;
