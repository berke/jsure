(* Excerpt *)

open Pffpsf

(*** excerpt *)
let excerpt liner text start_pos end_pos =
  let lines = Source.extract_lines liner text start_pos end_pos in
  let b = Buffer.create 256 in
  Array.iter
    begin fun (l, u, v, w) ->
      if Buffer.length b > 0 then bf b "\n";
      bf b "%s%5d%s %s%s%s%s%s%s%s"
           (Ansi.foreground !Opt.line_number_color) (l + 1) (Ansi.none ())
           (Ansi.foreground !Opt.code_color) u
           (Ansi.foreground !Opt.code_hl_color) v
           (Ansi.foreground !Opt.code_color) w
           (Ansi.none ())
    end
    lines;
  Buffer.contents b
(* ***)
