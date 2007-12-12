(* Levenshtein *)
(* Written by Berke DURAK, released in the public Domain. *)

module type COSTS =
  sig
    type t
    val min : t -> t -> t
    val sum : t -> t -> t
    val zero : t
  end

module type WORDS =
  sig
    module K:COSTS
    type t
    type letter
    val length : t -> int
    val get : t -> int -> letter
    val distance : letter option -> letter option -> K.t
  end
      
module Make(A:WORDS) =
  struct
    module K = A.K
    let distance s t =
      let m = A.length s and
          n = A.length t in
      let a = Array.make ((m + 1) * (n + 1)) K.zero in
      let put i j x =
        begin
          assert (0 <= i && i <= m && 0 <= j && j <= n);
          a.((n + 1) * i + j) <- x
        end
      and take i j =
        begin
          assert (0 <= i && i <= m && 0 <= j && j <= n);
          a.((n + 1) * i + j)
        end
      in
      begin
        put 0 0 K.zero;
        for i = 1 to m do
          put i 0 (K.sum (take (i - 1) 0) (A.distance None (Some(A.get s (i - 1)))))
        done;
        for j = 1 to n do
          put 0 j (K.sum (take 0 (j - 1)) (A.distance (Some(A.get t (j - 1))) None))
        done;
        for i = 0 to m - 1 do
          for j = 0 to n - 1 do
            let x1 = K.sum (take i       (j + 1)) (A.distance (Some (A.get s i)) None)
            and x2 = K.sum (take (i + 1) j      ) (A.distance (Some (A.get t j)) None)
            and x3 = K.sum (take i       j      )       (A.distance (Some (A.get s i)) (Some (A.get t j)))
            in
            let x = K.min x1 (K.min x2 x3) in
            put (i + 1) (j + 1) x
          done
        done;
        take m n
      end
  end

module Float_cost =
  struct
    type t = float
    let min = min
    let sum = (+.)
    let zero = 0.0
  end

module String_words =
  struct
    module K = Float_cost
    type t = string
    type letter = char
    let length = String.length
    let get = String.get
    let distance = Qwerty.qwerty_distance
    (*let distance c1 c2 =
      match (c1,c2) with
      | None, None -> 0.0
      | Some _, None -> 1.0
      | None, Some _ -> 1.0
      | Some c1, Some c2 ->
         if c1 = c2 then
           0.0
         else
           1.0*)
  end
    
module Levenshtein_string = Make(String_words)
