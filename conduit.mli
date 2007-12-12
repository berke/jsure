(* Conduit *)
(* Copyright 2005-2007 Berke DURAK, INRIA Rocquencourt and The EDOS Project. *)
(* Released under the GNU LGPL version 2. *)

(** A conduit is a channel for using formatting functions on buffers or channels. *)
type 'a conduit = {
  cd_out_channel : 'a;                              (** Can be a buffer or a channel. *)
  cd_print : 'b. 'a -> ('b, 'a, unit) format -> 'b; (** The print function. *)
  cd_flush : 'a -> unit;                            (** The flush function. *)
}

val stdoutcd : out_channel conduit (** The conduit linked to standard output *)

val stderrcd : out_channel conduit (** The conduit linked to standard error *)

val conduit_of_channel : out_channel -> out_channel conduit (** Builds a conduit from an output channel. *)

val conduit_of_buffer : Buffer.t -> Buffer.t conduit (** Builds a conduit from a buffer. *)

val scribe_string : 'a conduit -> 'a -> string -> unit (** Writes a string into a conduit. *)

val stringify : (Buffer.t conduit -> Buffer.t -> unit) -> string
