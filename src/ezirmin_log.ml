(* Mergeable log on git *)

open Irmin_unix
open Lwt.Infix

module Log(AO : Irmin.AO_MAKER)(V: Tc.S0) = struct

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

  module K = Irmin.Hash.SHA1

  type log_item =
    { time    : Time.t;
      message : V.t;
      prev    : K.t option}

  module Value = Tc.Biject
    (Tc.Pair (Tc.Pair(Time)(V))(Tc.Option(K)))
    (struct
      type t = log_item
      let to_t ((time,message), prev) =
        {time; message; prev}
      let of_t {time;message;prev} =
        (time,message),prev
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

  module Store = struct
    module S = AO(K)(C)
    include S
    let create () = create @@ Irmin_git.config ()
  end

  include K

  let append ?prev message =
    Store.create () >>= fun store ->
    Store.add store @@ C.Value {time = Ptime_clock.now(); prev; message}

  let read_key k =
    Store.create () >>= fun store ->
    Store.read_exn store k

  let sort l =
    List.sort (fun i1 i2 -> Time.compare i2.time i1.time) l

  let merge : Path.t -> t option Irmin.Merge.t =
    let open Irmin.Merge.OP in
    let merge' ~old v1 v2 =
      Store.create () >>= fun store ->
      Store.read store v1 >>= fun v1 ->
      Store.read store v2 >>= fun v2 ->
      let lv1 = match v1 with
      | None -> []
      | Some (C.Value v) -> [v]
      | Some (C.Merge lv) -> lv
      in
      let lv2 = match v2 with
      | None -> []
      | Some (C.Value v) -> [v]
      | Some (C.Merge lv) -> lv
      in
      Store.add store (C.Merge (sort @@ lv1 @ lv2)) >>= fun m ->
      ok m
      in fun _path -> Irmin.Merge.option (module K) merge'
end

module type S = sig
  include Ezirmin_repo.S
  type elt
  type cursor

  val append     : branch -> path:string list -> elt -> unit Lwt.t
  val get_cursor : branch -> path:string list -> cursor option Lwt.t
  val read       : cursor -> num_items:int -> (elt list * cursor option) Lwt.t
  val read_all   : branch -> path:string list -> elt list Lwt.t

  val watch : branch -> path:string list -> (elt -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
end

module Make(AOM : Irmin.AO_MAKER)(SM : Irmin.S_MAKER)(V:Tc.S0) : S with type elt = V.t = struct

  module L = Log(AOM)(V)

  module Repo = Ezirmin_repo.Make(SM)(L)
  include Repo

  module HashSet = Set.Make(Irmin.Hash.SHA1)

  type elt = V.t

  type cursor =
    { seen   : HashSet.t;
      cache  : L.log_item list;
      branch : branch }

  let head_name = "head"

  let append t ~path e =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= fun prev ->
    L.append ?prev e >>= fun v ->
    Lwt_log.debug_f "append.None" >>= fun () ->
    Store.update (t "Create head") head v

  let get_cursor branch ~path =
    let mk_cursor k cache = Lwt.return @@ Some {seen = HashSet.singleton k; cache; branch} in
    Store.read (branch "read") (path @ [head_name]) >>= function
    | None -> Lwt.return None
    | Some k ->
        L.read_key k >>= function
        | L.C.Value v -> mk_cursor k [v]
        | L.C.Merge l -> mk_cursor k l

  let rec read_log cursor ~num_items acc =
    let open L in
    if num_items <= 0 then Lwt.return (List.rev acc, Some cursor)
    else begin
      match cursor.cache with
      | [] -> Lwt.return (List.rev acc, None)
      | {message; prev = None; _}::xs ->
          Lwt_log.debug_f "read_log.Value.None" >>= fun () ->
          read_log {cursor with cache = xs} (num_items - 1) (message::acc)
      | {message; prev = Some pk; _}::xs ->
          if HashSet.mem pk cursor.seen then
            Lwt_log.debug_f "read_log.Value.Some.seen" >>= fun () ->
            read_log {cursor with cache = xs} (num_items - 1) (message::acc)
          else
            let seen = HashSet.add pk cursor.seen in
            L.read_key pk >>= function
            | L.C.Value v ->
                Lwt_log.debug_f "read_log.Value.Some.unseen.Value" >>= fun () ->
                read_log {cursor with seen; cache = sort (v::xs)}
                  (num_items - 1) (message::acc)
            | L.C.Merge l ->
                Lwt_log.debug_f "read_log.Value.Some.unseen.Merge" >>= fun () ->
                read_log {cursor with seen; cache = sort (l @ xs)}
                  (num_items - 1) (message::acc)
    end

  let read cursor ~num_items =
    read_log cursor num_items []

  let read_all branch ~path =
    get_cursor branch path >>= function
    | None -> Lwt.return []
    | Some cursor ->
        read cursor max_int >|= fun (log, _) ->
        log

  let watch branch ~path callback =
    let handle k = L.read_key k >>= function
    | L.C.Value {L.message} -> callback message
    | _ -> Lwt.return ()
    in
    Store.watch_key (branch "watch") (path @ [head_name]) (function
    | `Added (_, k) -> handle k
    | `Updated (_, (_,k)) -> handle k
    | _ -> Lwt.return ())
end
