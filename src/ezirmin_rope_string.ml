(*
 * Copyright (c) 2014 Benjamin Farinier <benjamin.farinier@ens-lyon.fr>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 KC Sivaramakrishnan <kc@kcsrk.info>
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


module Str : Ezirmin_rope.Container with type a = char and type t = string = struct
  include Irmin.Contents.String

  type a = char

  let empty = ""
  let length = String.length

  let set t i a =
    let s = Bytes.unsafe_of_string t in
    Bytes.set s i a;
    Bytes.unsafe_to_string s

  let get = String.get

  let insert t i s =
    assert (0 <= i && i <= String.length t);
    let left = String.sub t 0 i in
    let right = String.sub t i (String.length t - i) in
    String.concat "" [left; s; right]

  let delete t i j =
    assert (0 <= i && (i + j) <= String.length t);
    let left = String.sub t 0 i in
    let right = String.sub t (i + j) (String.length t - (i + j)) in
    String.concat "" [left; right]

  let append s t = String.concat "" [s; t]

  let concat sep list = String.concat sep list

  let split t i =
    assert (0 <= i && i <= String.length t);
    let left = String.sub t 0 i in
    let right = String.sub t i (String.length t - i) in
    (left, right)
end

module type S = sig
  include Ezirmin_repo.S

  type t
  type value = char
  type cont = string

  val create : unit -> t Lwt.t
  val make : string -> t Lwt.t
  val flush : t -> string Lwt.t

  val is_empty : t -> bool Lwt.t
  val length : t -> int Lwt.t

  val set : t -> pos:int -> char -> t Lwt.t
  val get : t -> pos:int -> char Lwt.t
  val insert : t -> pos:int -> string -> t Lwt.t
  val delete : t -> pos:int -> len:int -> t Lwt.t
  val append : t -> t -> t Lwt.t
  val concat : sep:t -> t list -> t Lwt.t
  val split : t -> pos:int -> (t * t) Lwt.t

  val write : ?message:string -> branch -> path:string list -> t -> unit Lwt.t
  val read  : branch -> path:string list -> t option Lwt.t
end


module Make(AO: Irmin.AO_MAKER)(S : Irmin.S_MAKER) = Ezirmin_rope.Make(AO)(S)(Str)
