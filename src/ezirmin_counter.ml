(* A last-write-wins register *)

open Irmin_unix
open Lwt.Infix

let return = Lwt.return

module Counter = struct

  module Path = Irmin.Path.String_list

  include Tc.Int

  let merge ~old t1 t2 =
    let open Irmin.Merge.OP in
    old () >>| fun old ->
    let old = match old with None -> 0 | Some o -> o in
    ok (t1 + t2 - old)

  let merge _ = Irmin.Merge.option (module Tc.Int) merge
end

module type S = sig
  include Ezirmin_repo.S
  val inc  : ?message:string -> ?by:int -> branch -> path:string list -> unit Lwt.t
  val dec  : ?message:string -> ?by:int -> branch -> path:string list -> unit Lwt.t
  val read : branch -> path:string list -> int Lwt.t
  val watch : branch -> path:string list -> (int -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
end

module Make(Backend : Irmin.S_MAKER) : S = struct

  module Repo = Ezirmin_repo.Make(Backend)(Counter)
  include Repo

  let head_name = "head"

  let modify ?(message="update") ?(by=1) t ~path tx =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= function
    | None -> Store.update (t message) head (tx by)
    | Some v -> Store.update (t message) head (tx by + v)

  let inc ?message ?by t ~path = modify ?message ?by t ~path (fun x -> x)
  let dec ?message ?by t ~path = modify ?message ?by t ~path (fun x -> -x)

  let read t ~path =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= function
    | None -> return 0
    | Some v -> return v

  let watch branch ~path callback =
    Store.watch_key (branch "watch") (path @ [head_name]) (function
      | `Added (_, v) -> callback v
      | `Updated (_,(_,v)) -> callback v
      | _ -> Lwt.return ())
end
