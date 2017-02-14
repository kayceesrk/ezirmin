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

open Lwt.Infix

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

  type edit =
    | Ins of int * char
    | Del of int * char
    | Rep of int * char * char

  let string_of_edit = function
    | Ins (p,c) -> Printf.sprintf "Ins(%d,%c)" p c
    | Del (p,c) -> Printf.sprintf "Del(%d,%c)" p c
    | Rep (p,c,c') -> Printf.sprintf "Rep(%d,%c,%c)" p c c'

  let diff xs ys =
    let cache = Array.init (String.length xs+1)
      (fun _ -> Array.make (String.length ys+1) None)
    in
    let rec loop i j =
      let cache_i = Array.unsafe_get cache i in
      let min3 x y z =
        let m' (a,al) (b,bl) = if a < b then (a,al) else (b,bl) in
        m' (m' x y) z
      in
      match Array.unsafe_get cache_i j with
      | Some v -> v
      | None ->
          let res =
            begin match i,j with
            | 0,0 -> (0, [])
            | 0, j ->
                let d,e = loop 0 (j-1) in
                (d+1, (Ins (i,String.get ys (j-1))::e))
            | i, 0 ->
                let d,e = loop (i-1) 0 in
                (d+1, (Del(i-1,String.get xs (i-1))::e))
            | _ ->
                let xsim1 = String.get xs (i-1) in
                let ysim1 = String.get ys (j-1) in
                let d,e = loop (i-1) j in
                let r1 = (d+1, Del (i-1,xsim1)::e) in
                let d,e = loop i (j-1) in
                let r2 = (d+1, Ins (i,ysim1)::e) in
                let d,e = loop (i-1) (j-1) in
                let r3 =
                  if xsim1 = ysim1 then d,e
                  else (d+1, (Rep (i-1, xsim1, ysim1)::e))
                in
                min3 r1 r2 r3
            end
          in
          Array.unsafe_set cache_i j (Some res);
          res
    in
    let rec adjust o = function
      | [] -> []
      | (Ins (i, x))::rest -> (Ins (i+o,x)) :: (adjust (o+1) rest)
      | (Del (i, x))::rest -> (Del (i+o,x)) :: (adjust (o-1) rest)
      | (Rep (i,x,x'))::rest -> (Rep (i+o,x,x')) :: (adjust o rest)
    in
    let d,e = loop (String.length xs) (String.length ys) in
    d, adjust 0 (List.rev e)

  let index = function
    | Ins (i,_) -> i
    | Del (i,_) -> i
    | Rep (i,_,_) -> i

  let shift_edit o = function
    | Ins(i,x) -> Ins(i+o,x)
    | Del(i,x) -> Del(i+o,x)
    | Rep(i,x,x') -> Rep(i+o,x,x')

  let rec shift_patch acc o = function
    | [] -> List.rev acc
    | e::tl -> shift_patch (shift_edit o e::acc) o tl

  let offset = function
    | Ins _ -> 1
    | Del _ -> -1
    | Rep _ -> 0


  let transform p q =
    let cons2 (x,y) (xs,ys) = (x::xs, y::ys) in
    let min_char x y =
      if Char.compare x y < 0 then x else y
    in
    let rec go xs a ys b =
      match xs, a, ys, b with
      | [], _, [], _ -> ([], [])
      | xs, a, [], _ -> (shift_patch [] a xs, [])
      | [], _, ys, b -> ([], shift_patch [] b ys)
      | x::xs, a, y::ys, b ->
          if index x < index y then
            let p',q' = go xs a (y::ys) (b + offset x) in
            (shift_edit a x::p',q')
          else if index x > index y then
            let p',q' = go (x::xs) (a + offset y) ys b in
            (p',shift_edit b y::q')
          else begin
            match x,y with
            | _ when x = y -> go xs (a + offset y) ys (b + offset x)
            | Ins (i,nx), Ins (_, ny) ->
                let n = min_char nx ny in
                cons2 (Rep (i+a,ny,n), Rep (i+b,nx,n)) (go xs (a + offset y) ys (b + offset x))
            | Rep (i, _, nx), Rep (_, _, ny) ->
                let n = min_char nx ny in
                cons2 (Rep (i + a, ny, n), Rep (i + b, nx, n)) (go xs a ys b)
            | Ins _, _ ->
                let p',q' = go xs a (y::ys) (b + offset x) in
                (shift_edit a x::p',q')
            | _, Ins _ ->
                let p',q' = go (x::xs) (a + offset y) ys b in
                (p', shift_edit b y::q')
            | Rep (i,_,nx), Del _ ->
                let p',q' = go xs (a + offset y) ys b in
                (p', Del (i+b, nx)::q')
            | Del _, Rep (i, _, ny) ->
                let p',q' = go xs a ys (b + offset x) in
                (Del (i+a,ny)::p',q')
            | Del _, Del _ -> go xs (a + offset y) ys (b + offset x)
          end
    in
    go p 0 q 0

  let rec apply s = function
    | [] -> s
    | Ins(pos,c)::tl ->
        let s' = insert s pos (String.make 1 c) in
        apply s' tl
    | Rep(pos,x,x')::tl ->
        assert (get s pos = x);
        let s' = set s pos x' in
        apply s' tl
    | Del(pos,x)::tl ->
        assert (get s pos = x);
        let s' = delete s pos 1 in
        apply s' tl

  let merge: Path.t -> t option Irmin.Merge.t =
    let open Irmin.Merge.OP in
    let merge_rope path ~old r1 r2 =
      old () >>= function  (* FIXME *)
      | `Conflict _ | `Ok None -> conflict "merge"
      | `Ok (Some old) ->
          let _,p = diff old r1 in
          let _,q = diff old r2 in
          let _,q' = transform p q in
          let news = apply (apply old p) q' in
          ok news
    in
    fun path -> Irmin.Merge.option (module Irmin.Contents.String) (merge_rope path)
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
