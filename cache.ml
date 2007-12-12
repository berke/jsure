(* Cache *)

type t = {
  c_db : Dbm.t;
  c_version : string
};;

let create ?(version=0) fn =
  let db = Dbm.opendbm fn [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o644 in
  let s = Printf.sprintf "%d;" version in
  { c_db = db;
    c_version = s }
;;

let digest_length = 16;;

(*** get *)
let get c name u =
  let data = Dbm.find c.c_db name in
  let digest = Digest.string u in
  let version = String.sub data 0 (String.length c.c_version) in
  if version <> c.c_version then raise Not_found;
  let digest' = String.sub data (String.length c.c_version) digest_length in
  if digest = digest' then
    Marshal.from_string data (String.length c.c_version + digest_length)
  else
    raise Not_found
;;
(* ***)
(*** set *)
let set c name u x =
  let digest = Digest.string u in
  let data = Marshal.to_string x [] in
  Dbm.replace c.c_db name (c.c_version ^ digest ^ data)
;;
(* ***)
(*** close *)
let close c =
  Dbm.close c.c_db
;;
(* ***)
