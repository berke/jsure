(* Conduit *)

type 'channel conduit = {
  cd_out_channel : 'channel; 
  cd_print       : 'a . 'channel -> ('a, 'channel, unit) format -> 'a; 
  cd_flush       : 'channel -> unit; 
};;

let stdoutcd = {
  cd_out_channel = stdout;
  cd_print = Printf.fprintf;
  cd_flush = flush
};;

let stderrcd = {
  cd_out_channel = stderr;
  cd_print = Printf.fprintf;
  cd_flush = flush
};;

let conduit_of_channel oc = {
  cd_out_channel = oc;
  cd_print = Printf.fprintf;
  cd_flush = flush
};;

let conduit_of_buffer b =
  { cd_out_channel = b;
    cd_print = Printf.bprintf;
    cd_flush = ignore }
;;

let scribe_string cd oc u = cd.cd_print oc "%s" u;;

(*** stringify *)
let stringify f =
  let b = Buffer.create 128 in
  let cd = conduit_of_buffer b in
  f cd cd.cd_out_channel;
  Buffer.contents b
;;
(* ***)
