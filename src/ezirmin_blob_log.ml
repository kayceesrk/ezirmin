(* A last-write-wins register *)

open Irmin_unix
open Lwt.Infix

let return = Lwt.return

module Log(V: Tc.S0) = struct

  module Path = Irmin.Path.String_list

  module type TIME = module type of Ptime

  module Time = struct
    include Tc.Bin_prot0 (struct
      include Ptime
      let to_json v = Ezjsonm.float @@ to_float_s v
      let of_json v =
        match of_float_s @@ Ezjsonm.get_float v with
        | None -> failwith "Time.of_json"
        | Some t -> t
      let bin_size_t v = Bin_prot.Size.bin_size_float @@ to_float_s v
      let bin_write_t a ~pos c =
        Bin_prot.Write.bin_write_float a ~pos (to_float_s c)
      let bin_read_t a ~pos_ref =
        Bin_prot.Read.bin_read_float a ~pos_ref |> of_float_s |> function
        | None -> failwith "Time.bin_read_t"
        | Some t -> t
    end)
    include (Ptime : TIME with type t := t)
  end

  module K = Tc.Option(Tc.List(Tc.String))

  module Entry = struct
    include Tc.Pair (Time)(V)
    let compare (x, _) (y, _) = Time.compare x y
    let create message = (Ptime_clock.now (), message)
  end

  module S = Tc.List(Entry)
  include S

	(* Get the timestamp of the latest entry. *)
	let timestamp = function
		| [] -> Ptime.epoch
		| (timestamp, _ ) :: _ -> timestamp

	(* Compute the entries newer than the given timestamp. *)
	let newer_than timestamp entries =
		let rec aux acc = function
			| [] -> List.rev acc
			| (h, _) :: _ when h <= timestamp -> List.rev acc
			| h::t -> aux (h::acc) t
		in
		aux [] entries

	let merge_log _path ~old t1 t2 =
		let open Irmin.Merge.OP in
		old () >>| fun old ->
		let old = match old with None -> [] | Some o -> o in
		let ts = timestamp old in
		let t1 = newer_than ts t1 in
		let t2 = newer_than ts t2 in
		let t3 = List.sort Entry.compare (List.rev_append t1 t2) in
		ok (List.rev_append t3 old)

	let merge path = Irmin.Merge.option (module S) (merge_log path)
end

module type S = sig
  include Ezirmin_repo.S
  type elt
  val append : branch -> path:string list -> elt -> unit Lwt.t
  val read_all : branch -> path:string list -> elt list Lwt.t
  val watch  : branch -> path:string list -> (elt -> unit Lwt.t)
               -> (unit -> unit Lwt.t) Lwt.t
end

module Make(Backend : Irmin.S_MAKER)(V:Tc.S0) : S with type elt = V.t = struct

  module L = Log(V)
  module Repo = Ezirmin_repo.Make(Backend)(L)
  include Repo

  type elt = V.t
  let head_name = "head"

  let append t ~path v =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= function
    | None ->
        (try
          Store.update (t "update") head [L.Entry.create v]
        with e -> (print_string (Printexc.to_string e); raise e))
    | Some l -> Store.update (t "update") head (L.Entry.create v :: l)

  let read_all t ~path =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= function
    | None -> return []
    | Some v -> return @@ List.map (fun (_,m) -> m) v

  let watch branch ~path callback =
    let open L in
    Store.watch_key (branch "watch") (path @ [head_name]) (function
      | `Added (_, (_,m)::_) -> callback m
      | `Updated (_,(_,(_,m)::_)) -> callback m
      | _ -> Lwt.return ())
end
