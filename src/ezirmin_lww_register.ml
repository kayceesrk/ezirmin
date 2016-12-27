(* A last-write-wins register *)

open Irmin_unix
open Lwt.Infix

let return = Lwt.return

module Lww_reg(V: Tc.S0) = struct

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

  module M = Tc.Pair (Time)(V)
  include M

  let to_string t =
    match to_json t with
    | `O l -> Ezjsonm.to_string (`O l)
    | _ -> failwith "Lww_register.to_string"

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

  let compare (x,u) (y,v) =
    let r = Time.compare x y in
    if r = 0 then V.compare u v else r

  let get_contents (_,v) = v

  let mk_value v' = (Ptime_clock.now (), v')

  let merge : Path.t -> t option Irmin.Merge.t =
    let open Irmin.Merge.OP in
    let merge ~old v1 v2 =
      if compare v1 v2 > 0 then ok v1 else ok v2
    in fun _path -> Irmin.Merge.option (module M) merge
end

module type S = sig
  include Ezirmin_repo.S
  type value
  val read  : branch -> path:string list -> value option Lwt.t
  val write : branch -> path:string list -> value -> unit Lwt.t
  val watch : branch -> path:string list
              -> ([ `Added of value | `Removed of value | `Updated of value * value ] -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
end

module Make(Backend : Irmin.S_MAKER)(V:Tc.S0) : S with type value = V.t = struct

  module R = Lww_reg(V)
  module Repo = Ezirmin_repo.Make(Backend)(R)
  include Repo

  type value = V.t
  let head_name = "head"

  let read t ~path =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= function
    | None -> return None
    | Some v -> return @@ Some (R.get_contents v)

  let write t ~path v =
    let head = path @ [head_name] in
    Store.update (t "write") head (R.mk_value v)

  let watch branch ~path callback =
    let open R in
    Store.watch_key (branch "watch") (path @ [head_name]) (function
      | `Added (_, v) -> callback (`Added (R.get_contents v))
      | `Removed (_, v) -> callback (`Removed (R.get_contents v))
      | `Updated ((_,o),(_,n)) -> callback (`Updated (R.get_contents o, R.get_contents n)))

end
