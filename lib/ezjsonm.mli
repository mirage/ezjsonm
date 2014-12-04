(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** An easy interface on top of the [Jsonm] library.

    This version provides more convenient (but far less flexible)
    input and output functions that go to and from [string] values.
    This avoids the need to write signal code, which is useful for
    quick scripts that manipulate JSON.

    More advanced users should go straight to the [Jsonm] library and
    use it directly, rather than be saddled with the Ezjsonm interface
    below.
*)

(** {2 Basic types} *)

type t =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of t list
  | `O of (string * t) list ]
(** The type of JSON documents. *)

(** {2 Reading JSON documents} *)

val from_channel: in_channel -> t
(** Read a JSON document from an input channel. *)

val from_string: string -> t
(** Read a JSON document from a string. *)

val from_src: Jsonm.src -> t
(** Low-level function to read directly from an [Jsonm] source. *)

(** {2 Writing JSON documents} *)

val to_channel: ?minify:bool -> out_channel -> t -> unit
(** Write a JSON document to an output channel. *)

val to_buffer: ?minify:bool -> Buffer.t -> t -> unit
(** Write a JSON document to a buffer. *)

val to_string: ?minify:bool -> t -> string
(** Write a JSON document to a string. This goes via an intermediate
    buffer and so may be slow on large documents. *)

val to_dst: ?minify:bool -> Jsonm.dst-> t -> unit
(** Low-level function to write directly to a [Jsonm] destination. *)

(** {2 Constructors} *)

val unit: t
(** Same as [`Null]. *)

val bool: bool -> t
(** Same as [`Bool b]. *)

val string: string -> t
(** Same as [`String s]. *)

val strings: string list -> t
(** Same as [`A [`String s1; ..; `String sn]]. *)

val int: int -> t
(** Same as [`Float (int_of_string i)]. *)

val float: float -> t
(** Some as [`Float f]. *)

val list: ('a -> t) -> 'a list -> t
(** Build a list of values. *)

val option: ('a -> t) -> 'a option -> t
(** Either [`Null] or a JSON value. *)

val dict: (string * t) list -> t
(** Build a dictionnary. *)

val pair: ('a -> t) -> ('b -> t) -> ('a * 'b) -> t
(** Build a pair. *)

(** {2 Accessors} *)

exception Parse_error of t * string
(** All the following accessor functions expect the provided JSON
    document to be of a certain kind. In case this is not the case,
    [Parse_error] is raised. *)

val get_unit: t -> unit
(** Check that the JSON document is [`Null]. *)

val get_bool: t -> bool
(** Extract [b] from [`Bool b]. *)

val get_string: t -> string
(** Extract [s] from [`String s]. *)

val get_strings: t -> string list
(** Extract [s1;..;sn] from [`A [`String s1; ...; `String sn]]. *)

val get_int: t -> int
(** Extract an integer. *)

val get_float: t -> float
(** Extract a float. *)

val get_list: (t -> 'a) -> t -> 'a list
(** Extract elements from a list document.. *)

val get_option: (t -> 'a) -> t -> 'a option
(** Extract an optional document. *)

val get_dict: t -> (string * t) list
(** Extract the elements from a dictionnary document. *)

val get_pair:(t -> 'a) -> (t -> 'b) -> t -> ('a * 'b)
(** Extract the pairs. *)

(** {2 High-level functions} *)

val mem: t -> string list -> bool
(** Is the given path valid if the provided JSON document. *)

val find: t -> string list -> t
(** Find the sub-document adressed by the given path. Raise
    [Not_found] if the path is invalid. *)

val update: t -> string list -> t option -> t
(** Update the sub-document addressed by the given path. If the
    provided value is [None], then removes the sub-document. *)

val map: (t -> t option) -> t -> string list -> t
(** Apply a given function to a subdocument. *)

val encode_string: string -> t
(** Convert a (possibly non-valid UTF8) string to a JSON object.*)

val decode_string: t -> string option
(** Convert a JSON object to a (possibly non-valid UTF8)
    string. Return [None] if the JSON object is not a valid string. *)

val decode_string_exn: t -> string
(** Convert a JSON object to a (possibly non-valid UTF8) string. *)

val to_sexp: t -> Sexplib.Type.t
(** Convert a JSON object to an S-expression. *)

val of_sexp: Sexplib.Type.t -> t
(** Convert an S-expression to a JSON object. *)

(** {2 Error handling} *)

exception Parse_error of t * string

val parse_error: t -> ('a, unit, string, 'b) format4 -> 'a
(** Raise [Parse_error] *)
