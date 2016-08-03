(* Mergeable log on git *)

open Irmin_unix
open Lwt.Infix

module Config = struct
  let conf = Irmin_git.config ()
  let task = Irmin_unix.task
end

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

  type log_item =
    { time       : Time.t;
      value      : V.t;
      prev       : K.t }

  module Value = Tc.Biject
    (Tc.Pair (Tc.Pair(Time)(V))(K))
    (struct
      type t = log_item
      let to_t ((time,value), prev) =
        {time; value; prev}
      let of_t {time;value;prev} =
        (time,value),prev
    end)

  module Merge = Tc.List(Value)

  module C = struct
    type t =
      | Value of Value.t
      | Merge of Merge.t

    let to_json = function
      | Value i -> `O [ "value", Value.to_json i ]
      | Merge n -> `O [ "merge", Merge.to_json n ]

    let of_json = function
      | `O [ "value", j ] -> Value (Value.of_json j)
      | `O [ "merge", j ] -> Merge (Merge.of_json j)
      | j -> Ezjsonm.parse_error j "C.of_json"

    let equal x y = match x, y with
      | Value x, Value y -> Value.equal x y
      | Merge x, Merge y -> Merge.equal x y
      | _ -> false

    let hash = Hashtbl.hash
    let compare = compare

    (* FIXME: slow *)
    let to_string t = Ezjsonm.to_string (to_json t)
    let of_string s = of_json (Ezjsonm.from_string s)
    let write t buf =
      let str = to_string t in
      let len = String.length str in
      Cstruct.blit_from_string str 0 buf 0 len;
      Cstruct.shift buf len
    let read buf =
      Mstruct.get_string buf (Mstruct.length buf)
      |> of_string
    let size_of t =
      let str = to_string t in
      String.length str
  end

  include C

  let sort l =
    List.sort (fun i1 i2 -> Time.compare i2.time i1.time) l

  let merge : Path.t -> t option Irmin.Merge.t =
    let open Irmin.Merge.OP in
    let merge ~old v1 v2 =
      let lv1 = match v1 with
      | Value v -> [v]
      | Merge l -> l
      in
      let lv2 = match v2 with
      | Value v -> [v]
      | Merge l -> l
      in
      ok @@ Merge (sort @@ lv1 @ lv2)
      in fun _path -> Irmin.Merge.option (module C) merge

   let mk_value value prev =
     C.Value {time = Ptime_clock.now(); prev; value}

end

module type S = sig
  type repo
  type branch

  val init        : root:string -> bare:bool -> repo Lwt.t
  val master      : repo -> branch Lwt.t
  val get_branch  : repo -> branch_name:string -> branch Lwt.t
  val clone_force : branch -> string -> branch Lwt.t
  val merge_exn   : branch -> into:branch -> unit Lwt.t

  type elt
  type cursor

  val append     : branch -> path:string list -> elt -> unit Lwt.t
  val get_cursor : branch -> path:string list -> cursor option Lwt.t
  val read       : cursor -> num_items:int -> (elt list * cursor option) Lwt.t
  val read_all   : branch -> path:string list -> elt list Lwt.t
end

module Make(V:Tc.S0) : S with type elt = V.t = struct

  module L = Log(V)
  module Store = Irmin_git.FS(L)(Irmin.Ref.String)(Irmin.Hash.SHA1)

  type repo = Store.Repo.t
  type elt = V.t
  type branch = string -> Store.t
  type path = string list

  module PathSet = Set.Make(Irmin.Path.String_list)

  type cursor =
    { seen   : PathSet.t;
      cache  : L.log_item list;
      branch : branch }

  let init ~root ~bare =
    let config = Irmin_git.config ~root ~bare () in
    Store.Repo.create config

  let master = Store.master task
  let clone_force t name = Store.clone_force task (t "cloning") name
  let get_branch r ~branch_name = Store.of_branch_id task branch_name r
  let merge_exn b ~into = Store.merge_exn "" b ~into

  let append t ~path e =
    let index = path @ ["index"] in

    (* TODO: Optimize *)
    let get_filename e =
      Cstruct.create_unsafe (L.size_of e) |>
      L.write e |> Irmin.Hash.SHA1.digest |>
      Irmin.Hash.SHA1.to_hum
    in

    Store.read (t "read_exn") index >>= function
    | None -> Store.update (t "update index") index @@ L.mk_value e None
    | Some prev ->
        let prev_filename = path @ [get_filename prev] in
        Store.update (t "update prev") prev_filename prev >>= fun () ->
        Store.update (t "update index") index @@ L.mk_value e (Some prev_filename)

  let get_cursor branch ~path =
    let open L in
    let mk_cursor cache = Lwt.return @@ Some {seen = PathSet.singleton path; cache; branch} in
    Store.read (branch "read") (path @ ["index"]) >>= function
    | None -> Lwt.return None
    | Some (Value v) -> mk_cursor [v]
    | Some (Merge l) -> mk_cursor l

  let rec read_log cursor ~num_items acc =
    let open L in
    if num_items <= 0 then Lwt.return (acc, Some cursor)
    else begin
      match cursor.cache with
      | [] -> Lwt.return (acc, None)
      | {value; prev = None; _}::xs ->
          read_log {cursor with cache = xs} (num_items - 1) (value::acc)
      | {value; prev = Some path; _}::xs ->
          if PathSet.mem path cursor.seen then
            read_log {cursor with cache = xs} (num_items - 1) (value::acc)
          else
            let seen = PathSet.add path cursor.seen in
            Store.read_exn (cursor.branch "read") path >>= function
            | Value v ->
                read_log {cursor with seen; cache = sort (v::cursor.cache)}
                  (num_items - 1) (value::acc)
            | Merge l ->
                read_log {cursor with seen; cache = sort (l @ cursor.cache)}
                  (num_items - 1) (value::acc)
    end

  let read cursor ~num_items =
    read_log cursor num_items []

  let read_all branch ~path =
    get_cursor branch path >>= function
    | None -> Lwt.return []
    | Some cursor ->
        read cursor max_int >|= fun (log, _) ->
        log
end
