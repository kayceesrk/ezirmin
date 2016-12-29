(*
 * Copyright (c) 2014 Benjamin Farinier <benjamin.farinier@ens-lyon.fr>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2016 KC Sivaramakrishnan <kc@kcsrk.info>
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


open Irmin_unix
open Lwt.Infix

let return = Lwt.return

let list_dedup ?(compare=Pervasives.compare) t =
  let t = List.sort compare t in
  let rec aux acc = function
    | []      -> List.rev acc
    | [x]     -> aux (x :: acc) []
    | x::(y::_ as tl) ->
      match compare x y with
      | 0 -> aux acc tl
      | _ -> aux (x :: acc) tl
  in
  aux [] t

exception Empty

type error = [ `Corrupted | `Invalid_access ]
exception Error of error

module Queue(AO : Irmin.AO_MAKER)(V : Tc.S0) = struct

  module Path = Irmin.Path.String_list
  module K = Irmin.Hash.SHA1

  module C = struct

    (*
     * Type of index, which are queue accessor.
     * 'push' is the number of push applied to the queue since its creation,
     * 'pop' is the number of pop applied to the queue since its creation,
     * 'top' is the key of the queue top element,
     * 'bottom' is the key of the queue bottom element.
    *)
    type index = {
      push  : int;
      pop   : int;
      top   : K.t;
      bottom: K.t;
    }

    module Index = Tc.Biject
        (Tc.Pair (Tc.Pair(Tc.Int)(Tc.Int))(Tc.Pair (K)(K)))
        (struct
          type t = index
          let to_t ((push, pop), (top, bottom)) = {push; pop; top; bottom}
          let of_t {push; pop; top; bottom} = (push, pop), (top, bottom)
        end)

    (*
     * Type of node, which are elements manipulated by queue operations.
     * 'next' is the optional key of a next element in the queue,
     * 'previous' is the optional key of a previous element in the queue,
     * 'elt' is the optional key of a elt associated to the node.
    *)
    type node = {
      next    : K.t option;
      previous: K.t option;
      elt     : K.t option;
      branch  : index option;
    }

    module KO = Tc.Option (K)
    module Node = Tc.Biject
        (Tc.Pair(Tc.Pair(KO)(KO))(Tc.Pair(KO)(Tc.Option(Index))))
        (struct
          type t = node
          let to_t ((next, previous), (elt, branch)) =
            {next; previous; elt; branch}
          let of_t {next; previous; elt; branch} =
            (next, previous), (elt, branch)
        end)

    (*
     * Type of store elements.
    *)
    type t =
      | Index of Index.t
      | Node  of Node.t
      | Elt   of V.t
    [@@deriving compare]

    let equal_node n1 n2 =
      Node.compare n1 n2 = 0

    let to_json = function
      | Index i -> `O [ "index", Index.to_json i ]
      | Node n  -> `O [ "node" , Node.to_json n ]
      | Elt e   -> `O [ "elt"  , V.to_json e ]

    let of_json = function
      | `O [ "index", j ] -> Index (Index.of_json j)
      | `O [ "node" , j ] -> Node (Node.of_json j)
      | `O [ "elt"  , j ] -> Elt (V.of_json j)
      | j -> Ezjsonm.parse_error j "C.of_json"

    let equal x y = match x, y with
      | Index x, Index y -> Index.equal x y
      | Node x, Node y -> Node.equal x y
      | Elt x, Elt y -> V.equal x y
      | _ -> false

    let hash = Hashtbl.hash

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
    let read t k = S.read t k
    let read_exn t k = S.read_exn t k
    let read_free t k = S.read_exn t k
    let add t v = S.add t v
  end

  (*
   * Type of a queue.
   * 'index' is the index of the queue in its store,
   * 'root' is the key of the 'empty' element of store.
  *)
  type queue = {
    index: C.Index.t;
    root : K.t;
  }

  module T = Tc.Biject (Tc.Pair (C.Index)(K))
      (struct
        type t = queue
        let to_t (index, root) = {index; root}
        let of_t {index; root} = (index, root)
      end)
  include T

  type elt = V.t

  let empty = {
    C.next = None;
    C.previous = None;
    C.elt = None;
    C.branch = None;
  }

  (*
   * Create a new queue in the store 'store'.
   * 'top' and 'bottom' are pointed on the 'empty' node.
  *)
  let create () =
    Store.create () >>= fun store ->
    Store.add store (C.Node empty) >>= fun root ->
    let index = {
      C.push = 0;
      C.pop = 0;
      C.top = root;
      C.bottom = root;
    } in
    return { index; root }

  let length t =
    return (t.index.C.push - t.index.C.pop)

  let is_empty t =
    return (t.index.C.push = t.index.C.pop)

  (*
   * Queues are implemented with two lists,
   * the push list, containing pushed elements,
   * and the pop list, containing elements to be poped.
   * 'normalise' flush the push list into the pop one.
  *)
  let normalize q =
    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    let apply k1 k2 queue old_node k_old_node k_new_node =
      k1 queue old_node k_old_node k_new_node k2
    in

    (*
     * Go through the pop list and call the continuation on the push list,
     * then rebuild it from its last element to its first element.
     * Not tail recursive.
    *)
    let rec from_top queue old_node k_old_node k_new_node k =

      (match old_node.C.next with
       | None -> (
           assert (C.equal_node old_node empty);
           k queue k_old_node k_new_node
         )
       | Some old_next -> (
           Store.read_exn store old_next >>= fun old_next ->
           match old_next with
           | C.Index _
           | C.Elt _ -> Lwt.fail (Error `Corrupted)
           | C.Node old_next -> from_top queue old_next k_old_node k_new_node k
         )
      ) >>= fun new_next ->
      match old_node.C.elt with
      | None -> return new_next
      | Some elt -> (
          Store.add store (C.Node new_next) >>= fun new_key_node ->
          let new_node = {
            C.next = Some new_key_node;
            C.previous = None;
            C.elt = Some elt;
            C.branch = None;
          } in return new_node
        )
    in

    (*
     * Go through the push list rebuilding its elements, then call the continuation.
     * Tail recursive.
    *)
    let rec from_bottom queue old_node new_node =

      match old_node.C.branch with
      | Some index -> (
          Store.read_exn store index.C.top >>= fun branch_top ->
          match branch_top with
          | C.Index _
          | C.Elt _ -> Lwt.fail (Error `Corrupted)
          | C.Node branch_top ->
            Store.read_exn store index.C.bottom >>= fun branch_bottom ->
            match branch_bottom with
            | C.Index _
            | C.Elt _ ->  Lwt.fail (Error `Corrupted)
            | C.Node branch_bottom ->
              let root = queue.root in
              let new_queue = {index; root} in
              apply from_top from_bottom new_queue branch_top
                branch_bottom new_node >>= fun node ->
              match old_node.C.previous with
              | None -> return new_node
              | Some old_previous -> (
                  Store.read_exn store old_previous >>= fun old_previous ->
                  match old_previous with
                  | C.Index _
                  | C.Elt _ -> Lwt.fail (Error `Corrupted)
                  | C.Node old_previous -> from_bottom queue old_previous node
                )
        )
      | None -> (
          match old_node.C.previous with
          | None -> (
              assert (C.equal_node old_node empty);
              return new_node;
            )
          | Some old_previous -> (
              Store.read_exn store old_previous >>= fun old_previous ->
              match old_previous with
              | C.Index _
              | C.Elt _ -> Lwt.fail (Error `Corrupted)
              | C.Node old_previous -> (
                  Store.add store (C.Node new_node) >>= fun key_node ->
                  let new_previous = {
                    C.next = Some key_node;
                    C.previous = None;
                    C.elt = old_node.C.elt;
                    C.branch = None;
                  } in from_bottom queue old_previous new_previous
                )
            )
        )
    in

    Store.read_exn store index.C.top >>= fun top_node ->
    match top_node with
    | C.Index _
    | C.Elt _ -> Lwt.fail (Error `Corrupted)
    | C.Node top_node ->
      Store.read_exn store index.C.bottom >>= fun bottom_node ->
      match bottom_node with
      | C.Index _
      | C.Elt _ -> Lwt.fail (Error `Corrupted)
      | C.Node bottom_node ->
        apply from_top from_bottom q top_node bottom_node empty >>= fun node ->
        Store.add store (C.Node node) >>= fun key_top ->
        let index = {
          C.push = index.C.push;
          C.pop = index.C.pop;
          C.top = key_top;
          C.bottom = root;
        } in
        return { index; root }

  (*
   * Add a new node in the push list, and move the index on.
   * The new index is NOT added in the store, ie the queue is NOT updated.
  *)
  let push q elt =

    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    Store.add store (C.Elt elt) >>= fun key_elt ->
    let node = {
      C.next = None;
      C.previous = Some index.C.bottom;
      C.elt = Some key_elt;
      C.branch = None;
    } in
    Store.add store (C.Node node) >>= fun key_node ->
    let index = {
      C.push = index.C.push + 1;
      C.pop = index.C.pop;
      C.top = index.C.top;
      C.bottom = key_node;
    } in
    return { index; root }

  let push_branch q branch =

    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    let node = {
      C.next = None;
      C.previous = Some index.C.bottom;
      C.elt = None;
      C.branch = Some branch;
    } in
    Store.add store (C.Node node) >>= fun key_node ->
    let index = {
      C.push = index.C.push;
      C.pop = index.C.pop;
      C.top = index.C.top;
      C.bottom = key_node;
    } in
    return { index; root }

  (*
   * Move the index of the queue to the next element.
   * The new index is NOT added in the store, ie the queue is NOT updated.
   * Return None if the queue is empty.
  *)
  let rec pop q =

    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    if index.C.push = index.C.pop then
      return None
    else
      Store.read_exn store  index.C.top >>= fun node ->
      match node with
      | C.Index _
      | C.Elt _ -> Lwt.fail (Error `Corrupted)
      | C.Node node ->
        match node.C.elt with
        | None -> normalize q >>= fun q -> pop q
        | Some elt ->
          Store.read_exn store elt >>= fun elt ->
          match elt with
          | C.Index _
          | C.Node _ -> Lwt.fail (Error `Corrupted)
          | C.Elt elt ->
            let key = (match node.C.next with
                | None -> root
                | Some key -> key) in
            let index = {
              C.push = index.C.push;
              C.pop = index.C.pop + 1;
              C.top = key;
              C.bottom = index.C.bottom;
            } in

            return (Some (elt, { index; root }))

  (*
   * Move the index of the queue to the next element.
   * The new index is NOT added in the store, ie the queue is NOT updated.
   * Raise Empty if the queue is empty.
  *)
  let rec pop_exn q =

    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    if index.C.push = index.C.pop then
      Lwt.fail Empty
    else
      Store.read_exn store index.C.top >>= fun node ->
      match node with
      | C.Index _
      | C.Elt _ -> Lwt.fail (Error `Corrupted)
      | C.Node node ->
        match node.C.elt with
        | None -> normalize q >>= fun q -> pop_exn q
        | Some elt ->
          Store.read_exn store elt >>= fun elt ->
          match elt with
          | C.Index _
          | C.Node _ -> Lwt.fail (Error `Corrupted)
          | C.Elt elt ->
            let key = (match node.C.next with
                | None -> root
                | Some key -> key) in
            let index = {
              C.push = index.C.push;
              C.pop = index.C.pop + 1;
              C.top = key;
              C.bottom = index.C.bottom;
            } in

            return (elt, { index; root })

    let merge: Path.t -> t option Irmin.Merge.t =

    let rec clean old q =
      if old.index.C.push > q.index.C.pop
      && q.index.C.push > q.index.C.pop then
        pop_exn q >>= fun (_, q) -> clean old q
      else return q
    in

    let rec equalize old q1 q2 =
      if K.(q1.index.C.top = q2.index.C.top
            && q1.index.C.bottom = q2.index.C.bottom)
      then
        create () >>= fun q2 -> return (q1, q2)
      else (
        if q2.index.C.pop > q1.index.C.pop
        && old.index.C.push > q1.index.C.pop
        && q1.index.C.push > q1.index.C.pop
        then
          pop_exn q1 >>= fun (_, q1) -> equalize old q1 q2
        else clean old q2 >>= fun q2 -> return (q1, q2))
    in


    let merge ~old q1 q2 =
      let open Irmin.Merge.OP in
      old () >>= function  (* FIXME *)
      | `Conflict _ | `Ok None -> conflict "merge"
      | `Ok (Some old) ->
        assert K.(q1.root = q2.root && old.root = q1.root);
        let root = q1.root in
        equalize old q1 q2 >>= fun (q1, q2) ->
        (if q2.index.C.push > q2.index.C.pop then
           push_branch q1 q2.index
         else return q1) >>= fun q ->
        let index = {
          C.push = q1.index.C.push + q2.index.C.push - old.index.C.push;
          C.pop = q1.index.C.pop + q2.index.C.pop - old.index.C.push;
          C.top = q.index.C.top;
          C.bottom = q.index.C.bottom;
        } in
        ok {index; root}
    in

    fun _path -> Irmin.Merge.option (module T) merge

  (*
   * Return all the keys (index, node or value) that are accessible.
   * Returned keys may be associated to unreadable value!
   * Should be only use by the GC.
  *)
  let list q list =

    Store.create () >>= fun store ->

    let add list = function
      | None -> list
      | Some opt -> opt :: list
    in

    let rec iter tmp_list res_list = match tmp_list with
      | [] -> return res_list
      | key :: tmp_list ->
        Store.read_exn store key >>= fun value ->
        match value with
        | C.Elt _       -> Lwt.fail (Error `Invalid_access)
        | C.Index index -> iter
                             (index.C.top :: index.C.bottom :: tmp_list)
                             (index.C.top :: index.C.bottom ::res_list)
        | C.Node node ->
          let res_list = add res_list node.C.next in
          let res_list = add res_list node.C.previous in
          let res_list = add res_list node.C.elt in
          let tmp_list = add tmp_list node.C.next in
          let tmp_list = add tmp_list node.C.previous in
          match node.C.branch with
          | None -> iter tmp_list res_list
          | Some index -> iter
                            (index.C.top :: index.C.bottom :: tmp_list)
                            (index.C.top :: index.C.bottom ::res_list)
    in

    iter list (q.root::list) >>= fun list ->
    return (list_dedup list)

end

module type S = sig
  include Ezirmin_repo.S

  type elt
  type cursor
  val create : branch -> path:string list -> unit Lwt.t
  val length : branch -> path:string list -> int Lwt.t
  val is_empty : branch -> path:string list -> bool Lwt.t
  val push : branch -> path:string list -> elt -> unit Lwt.t
  val pop_exn : branch -> path:string list -> elt Lwt.t
  val pop : branch -> path:string list -> elt option Lwt.t
  val to_list : branch -> path:string list -> elt list Lwt.t

  val get_cursor : branch -> path:string list -> cursor option Lwt.t
  val next_exn : cursor -> (elt * cursor) Lwt.t
  val next : cursor -> (elt * cursor) option Lwt.t
  val iter : branch -> path:string list -> f:(elt -> unit) -> unit Lwt.t

  val watch : branch -> path:string list -> (unit -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
end

module Make(AO : Irmin.AO_MAKER)(S : Irmin.S_MAKER)(V : Tc.S0) : S with type elt = V.t = struct

  module Q = Queue(AO)(V)

  module Repo = Ezirmin_repo.Make(S)(Q)
  include Repo

  type elt = V.t

  let head_name = "head"

  let create t ~path =
    let head = path @ [head_name] in
    Q.create () >>= fun q ->
    Lwt_log.debug_f "create.None" >>= fun () ->
    Store.update (t "create") head q

  let length t ~path =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= function
    | None -> return 0
    | Some q -> Q.length q

  let is_empty t ~path =
    length t ~path >>= function
    | 0 -> return true
    | _ -> return false

  let push t ~path e =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= (function
    | None -> Q.create ()
    | Some q -> return q) >>= fun q ->
    Q.push q e >>= fun q' ->
    Store.update (t "update") head q'

  let pop_exn t ~path =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= function
    | None -> Lwt.fail Empty
    | Some q -> Q.pop q >>= function
    | None -> Lwt.fail Empty
    | Some (v, q') ->
        Store.update (t "update") head q' >>= fun () ->
        return v

  let pop t ~path =
    let head = path @ [head_name] in
    Store.read (t "read") head >>= function
    | None -> return None
    | Some q -> Q.pop q >>= function
    | None -> return None
    | Some (v, q') ->
        Store.update (t "update") head q' >>= fun () ->
        return (Some v)

  type cursor = Q.t

  let get_cursor t ~path =
    let head = path @ [head_name] in
    Store.read (t "read") head

  let next c = Q.pop c

  let next_exn c = Q.pop_exn c

  let iter t ~path ~f =
    get_cursor t path >>= function
    | None -> return ()
    | Some c ->
        let rec loop c =
          next c >>= function
          | None -> return ()
          | Some (v, c) -> (f v; loop c)
        in
        loop c

  let to_list t ~path =
    let lr = ref [] in
    iter t path (fun e -> lr := e::!lr) >>= fun () ->
    return (List.rev !lr)

  let watch branch ~path callback =
    Store.watch_key (branch "watch") (path @ [head_name]) (fun _ -> callback ())
end
