(* Minefield *)

module SS = Set.Make(String);;
module SM = Map.Make(String);;

type t = {
  mutable mf_words : int SM.t;
}

module L = Levenshtein.Levenshtein_string;;

let create () = { mf_words = SM.empty };;

let add_word mf u = mf.mf_words <- SM.add u 0 mf.mf_words;;

let check_proximity mf u =
  if SM.mem u mf.mf_words then
    None
  else
    begin
      let min_dist = ref max_float in
      let nearest_word = ref None in
      SM.iter
        begin fun v _ ->
          let d = L.distance u v in
          if d < !min_dist then
            begin
              min_dist := d;
              nearest_word := Some v
            end
        end
        mf.mf_words;
      match !nearest_word with
      | None -> None
      | Some v -> Some(!min_dist, v)
    end
;;
