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

module type Content = sig
  type atom
  include Irmin.Contents.S
    with module Path = Irmin.Path.String_list

  val empty : t
  val length : t -> int
  val set : t -> int -> atom -> t
  val get : t -> int -> atom
  val insert : t -> int -> t -> t
  val delete : t -> int -> int -> t
  val append : t -> t -> t
  val concat : t -> t list -> t
  val split : t -> int -> (t * t)
end

module Rope(AO : Irmin.AO_MAKER)(V : Content) = struct

  module Path = V.Path
  module K = Irmin.Hash.SHA1

  module C = struct

    (*
    * Type of a branch in the internal tree.
    * 'key' is the Irmin key of the subtree pointed to the branch,
    * 'min_depth' is the minimal depth of this subtree,
    * 'max_depth' the maximal one.
    *)
    type branch = {
      key : K.t;
      min_depth : int;
      max_depth : int;
    }

    module Branch = Tc.Biject
      (Tc.Pair (Tc.Pair(K)(Tc.Int))(Tc.Int))
      (struct
        type t = branch
        let to_t ((key, min_depth), max_depth) = {key; min_depth; max_depth}
        let of_t {key; min_depth; max_depth} = ((key, min_depth), max_depth)
      end)

    (*
    * Type of a node in the internal tree.
    * 'ind' is the index of the node, which is equal to length of the left subtree
    * 'len' is the length of the tree having this node as the root,
    * 'left' is the branch pointed to the left subtree,
    * 'right' the branch of the right one.
    *)
    type node = {
      ind : int;
      len : int;
      left : branch;
      right : branch;
    }

    module Node = Tc.Biject
      (Tc.Pair(Tc.Pair(Tc.Int)(Tc.Int))(Tc.Pair(Branch)(Branch)))
      (struct
        type t = node
        let to_t ((ind, len), (left, right)) =
          {ind; len; left; right}
        let of_t {ind; len; left; right} =
          (ind, len), (left, right)
      end)

    (*
    * Type of index, which is the accessor to the rope internal tree.
    * 'length' is the length of the whole rope,
    * 'root' is the root of the internal tree.
    *)
    type index = {
      length : int;
      root : K.t;
    }

    module Index = Tc.Biject
      (Tc.Pair(Tc.Int)(K))
      (struct
        type t = index
        let to_t (length, root) = {length; root}
        let of_t {length; root} = (length, root)
      end)

    type t =
      | Index of Index.t
      | Node of Node.t
      | Leaf of V.t
    [@@deriving compare]

    let equal a b =
      Pervasives.compare a b = 0

     let hash = Hashtbl.hash

     let to_json = function
       | Index i -> `O [ "index", Index.to_json i ]
       | Node n -> `O [ "node", Node.to_json n ]
       | Leaf v -> `O [ "leaf", V.to_json v ]

     let of_json = function
       | `O [ "index", j ] -> Index (Index.of_json j)
       | `O [ "node", j ] -> Node (Node.of_json j)
       | `O [ "leaf", j ] -> Leaf (V.of_json j)
       | j -> Ezjsonm.parse_error j "C.of_json"

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

  module Store_ = struct

    (*
     * Enhance functions of the IrminStore_ with statistic tracking.
     * Each IrminRope function has its own table which store the number of read and write.
     * These tables are indexed by an 'int' which is the size of the rope.
    *)

    module Table = Hashtbl.Make(struct
        type t = int
        let equal = (=)
        let hash x = x
      end )

    let to_alist t = Table.fold (fun k v l -> (k, v) :: l) t []

    type action = Set | Get | Insert | Delete | Append | Split | Merge | Other
    type ref_stats = {
      r_ops : int ref;
      r_reads : int ref;
      r_writes : int ref;
    }

    module S = AO(K)(C)
    include S

    let create () = create @@ Irmin_git.config ()

    let read t k =
      S.read t k

    let read_exn t k =
      S.read_exn t k

    let read_free t k =
      S.read_exn t k

    let add t v =
      S.add t v

  end

  let min_depth t = 1 + C.(min t.left.min_depth t.right.min_depth)
  let max_depth t = 1 + C.(max t.left.max_depth t.right.max_depth)
  let get_length = C.(function
    | Index index -> index.length
    | Node node -> node.len
    | Leaf leaf -> V.length leaf)



  (*
   * Following several construction functions.
   * The keyword 'create' means that a write is used during the construction.
   * Reversely, the keyword 'constr' signifies that no write is used.
  *)

  let create_branch store t =
    let open C in
    match t with
    | Leaf leaf ->
      Store_.add store t >>= fun key ->
      return { key; min_depth = 1; max_depth = 1}
    | Node node ->
      Store_.add store t >>= fun key ->
      return { key; min_depth = min_depth node; max_depth = max_depth node }
    | Index index -> assert false

  let constr_branch t key =
    let open C in
    match t with
    | Leaf leaf ->
      return { key; min_depth = 1; max_depth = 1 }
    | Node node ->
      return { key; min_depth = min_depth node; max_depth = max_depth node }
    | Index index -> assert false


  let make_node l_length r_length b_left b_right =
    let open C in
    assert (l_length >= 0);
    assert (r_length >= 0);
    return {
      ind = l_length;
      len = l_length + r_length;
      left = b_left;
      right = b_right
    }

  let create_node store t_left t_right =
    let l_length = get_length t_left in
    let	r_length = get_length t_right in
    create_branch store t_left >>= fun b_left ->
    create_branch store t_right >>= fun b_right ->
    make_node l_length r_length b_left b_right

  let constr_node (t_left, k_left) (t_right, k_right) =
    let l_length = get_length t_left in
    let	r_length = get_length t_right in
    constr_branch t_left k_left >>= fun b_left ->
    constr_branch t_right k_right >>= fun b_right ->
    make_node l_length r_length b_left b_right

  let crt_cns_node store t_left (t_right, k_right) =
    let l_length = get_length t_left in
    let	r_length = get_length t_right in
    create_branch store t_left >>= fun b_left ->
    constr_branch t_right k_right >>= fun b_right ->
    make_node l_length r_length b_left b_right

  let cns_crt_node store (t_left, k_left) t_right =
    let l_length = get_length t_left in
    let	r_length = get_length t_right in
    constr_branch t_left k_left >>= fun b_left ->
    create_branch store t_right >>= fun b_right ->
    make_node l_length r_length b_left b_right


  let create_index store t =
    let open C in
    match t with
    | Leaf leaf ->
      Store_.add store t >>= fun key ->
      return { length = V.length leaf; root = key }
    | Node node ->
      Store_.add store t >>= fun key ->
      return { length = node.len; root = key }
    | Index index -> return index

  let constr_index t key =
    let open C in
    match t with
    | Leaf leaf ->
      return { length = V.length leaf; root = key }
    | Node node ->
      return { length = node.len; root = key }
    | Index index -> return index


  (*
   * Internal tree rotation which is used to keep the tree balanced.
   * A tree rotation moves one node up in the tree and one node down.
  *)
  let rec rotate =

    let left_to_right store t =
      let open C in
      match t with
      | Leaf _ -> assert false
      | Index _ -> assert false
      | Node node ->
        Store_.read_exn store node.left.key >>= fun t_left ->
        Store_.read_exn store node.right.key >>= fun t_right ->
        match t_left with
        | Leaf _ -> assert false
        | Index _ -> assert false
        | Node n_left ->
          Store_.read_exn store n_left.left.key >>= fun t_lleft ->
          Store_.read_exn store n_left.right.key >>= fun t_lright ->
          constr_node (t_lright, n_left.right.key) (t_right, node.right.key) >>= fun new_right ->
          rotate store (Node new_right) >>= fun new_right ->
          cns_crt_node store (t_lleft, n_left.left.key) (Node new_right)
    in

    let right_to_left store t =
      let open C in
      match t with
      | Leaf _ -> assert false
      | Index _  -> assert false
      | Node node ->
        Store_.read_exn store node.left.key >>= fun t_left ->
        Store_.read_exn store node.right.key >>= fun t_right ->
        match t_right with
        | Leaf _ -> assert false
        | Index _ -> assert false
        | Node n_right ->
          Store_.read_exn store n_right.left.key >>= fun t_rleft ->
          Store_.read_exn store n_right.right.key >>= fun t_rright ->
          constr_node (t_left, node.left.key) (t_rleft, n_right.left.key) >>= fun new_left ->
          rotate store (Node new_left) >>= fun new_left ->
          crt_cns_node store (Node new_left) (t_rright, n_right.right.key)
    in

    fun store t ->
      let open C in
      match t with
      | Leaf _ -> assert false
      | Index _ -> assert false
      | Node node ->
        (*print_endline (Printf.sprintf "Left: %i %i, Right; %i %i"
          node.left.min_depth node.left.max_depth node.right.min_depth node.right.max_depth);*)
        if (node.right.max_depth - node.left.min_depth > node.left.min_depth) then
          right_to_left store t
        else if (node.left.max_depth - node.right.min_depth > node.right.min_depth) then
          left_to_right store t
        else return node



  (*
   * Create a new empty rope.
  *)
  let create () =
    Store_.create () >>= fun store ->
    create_index store (C.Leaf V.empty)


  (*
   * Create a new rope from a given container.
   * In order to reduce the number of read/write, the containers stored
   * in the leafs have a length equal to depth of the tree.
  *)
  let (make, make_internal) =

    let rec make_rec store depth cont =
      let length = V.length cont in
      if (length < 2 * depth) then return (C.Leaf cont)
      else
        let (c_left, c_right) = V.split cont (length / 2) in
        make_rec store (depth + 1) c_left >>= fun t_left ->
        make_rec store (depth + 1) c_right >>= fun t_right ->
        create_node store t_left t_right >>= fun node ->
        return (C.Node node)
    in
    (
      (
        fun cont ->
          Store_.create () >>= fun store ->
          make_rec store 1 cont >>= fun t ->
          create_index store t
      ),
      (
        fun store ?depth:(d=1) cont ->
          make_rec store d cont
      )
    )


  (*
   * Flush the rope into a container.
  *)
  let (flush, flush_free, flush_internal) =

    let rec flush_rec store list v =
      let open C in
      match v with
      | Index _ -> assert false
      | Leaf leaf -> return (leaf::list)
      | Node node ->
        Store_.read_exn store node.left.key >>= fun t_left ->
        Store_.read_exn store node.right.key >>= fun t_right ->
        flush_rec store list t_right >>= fun list ->
        flush_rec store list t_left
    in
    C.(
      (
        fun index ->
          Store_.create () >>= fun store ->
          Store_.read_exn store index.root >>= fun t ->
          flush_rec store [] t >>= fun list ->
          return (V.concat V.empty list)
      ),
      (
        fun index ->
          Store_.create () >>= fun store ->
          Store_.read_exn store index.root >>= fun t ->
          flush_rec store [] t >>= fun list ->
          return (V.concat V.empty list)
      ),
      (
        fun store ?list:(l=[]) t ->
          flush_rec store l t >>= fun list ->
          return (V.concat V.empty list)
      )
    )

  let is_empty index = return (index.C.length = 0)
  let length index = return index.C.length


  type choice = Left | Right

  (*
   * Set the value at the position 'i' in the rope to 'a'
  *)
  let set index i a =
    let open C in
    Store_.create () >>= fun store ->

    let choose node i =
      if (node.len < i || i < 0) then
        invalid_arg (Printf.sprintf "try to access the position %i in a rope of size %i" i node.len)
      else
      if (i < node.ind) then Left else Right
    in

    let rec set_rec i a = function
      | Index _ -> assert false
      | Leaf leaf ->
        let leaf = V.set leaf i a in
        return (Leaf leaf)
      | Node node ->
        match choose node i with
        | Left ->
          Store_.read_exn store node.left.key >>= fun t_left ->
          set_rec i a t_left >>= fun t_left ->
          create_branch store t_left >>= fun b_left ->
          make_node node.ind (node.len - node.ind) b_left node.right >>= fun node ->
          return (Node node)
        | Right ->
          Store_.read_exn store node.right.key >>= fun t_right ->
          set_rec (i - node.ind) a t_right >>= fun t_right ->
          create_branch store t_right >>= fun b_right ->
          make_node node.ind (node.len - node.ind) node.left b_right >>= fun node ->
          return (Node node)
    in

    Store_.read_exn store index.root >>= fun t ->
    set_rec i a t >>= fun t ->
    create_index store t


  (*
   * Return the value at the position 'i' in the rope.
  *)
  let get index i =
    let open C in
    Store_.create () >>= fun store ->

    let choose node i =
      if (node.len < i || i < 0) then
        invalid_arg (Printf.sprintf "try to access the position %i in a rope of size %i" i node.len)
      else
      if (i < node.ind) then Left else Right
    in

    let rec get_rec i = function
      | Index _ -> assert false
      | Leaf leaf -> return (V.get leaf i)
      | Node node ->
        match choose node i with
        | Left ->
          Store_.read_exn store node.left.key >>= fun t_left ->
          get_rec i t_left
        | Right ->
          Store_.read_exn store node.right.key >>= fun t_right ->
          get_rec (i - node.ind) t_right
    in

    Store_.read_exn store index.root >>= fun t ->
    get_rec i t

  (*
   * Insert the container 'cont' at the position 'i' in the rope.
   * Rebalancing is made from bottom to up when the rope is rebuild.
   * Maintain the 'tree depth/leaf length' invariant.
  *)
  let insert index i cont =
    let open C in
    Store_.create () >>= fun store ->

    let len = V.length cont in

    let choose node i =
      if (node.len < i || i < 0) then
        invalid_arg (Printf.sprintf "try to access the position %i in a rope of size %i" i node.len)
      else
      if (i < node.ind) then Left else Right
    in

    let rec insert_rec depth i = function
      | Index _ -> assert false
      | Leaf leaf ->
        let leaf = V.insert leaf i cont in
        let l_length = V.length leaf in
        if (l_length < 2 * depth) then return (Leaf leaf)
        else
          make_internal store ~depth leaf
      | Node node ->
        match choose node i with
        | Left ->
          Store_.read_exn store node.left.key >>= fun t_left ->
          insert_rec (depth + 1) i t_left >>= fun t_left ->
          create_branch store t_left >>= fun b_left ->
          make_node (node.ind + len) (node.len - node.ind) b_left node.right >>= fun node ->
          rotate store (Node node) >>= fun node ->
          return (Node node)
        | Right ->
          Store_.read_exn store node.right.key >>= fun t_right ->
          insert_rec (depth + 1) (i - node.ind) t_right >>= fun t_right ->
          create_branch store t_right >>= fun b_right ->
          make_node node.ind (node.len - node.ind + len) node.left b_right >>= fun node ->
          rotate store (Node node) >>= fun node ->
          return (Node node)
    in

    Store_.read_exn store index.root >>= fun t ->
    insert_rec 1 i t >>= fun t ->
    create_index store t



  (*
   * Delete the elements between the position 'i' and 'j'.
   * Rebalancing is made from bottom to up when the rope is rebuild.
   * Maintain the 'tree depth/leaf length' invariant.
  *)
  let delete index i j =
    let open C in
    Store_.create () >>= fun store ->

    let choose node i =
      if (node.len < i || i < 0) then
        invalid_arg (Printf.sprintf "try to access the position %i in a rope of size %i" i node.len)
      else
      if (i < node.ind) then Left else Right
    in

    let clean store depth = function
      | Index _ -> assert false
      | Leaf leaf -> return (Leaf leaf)
      | Node node ->
        if (node.ind < depth / 2 || node.len - node.ind < depth / 2) then
          flush_internal store (Node node) >>= fun cont ->
          make_internal store ~depth cont
        else
          rotate store (Node node)  >>= fun node ->
          return (Node node)
    in

    let rec delete_rec depth i = function
      | Index _ -> assert false
      | Leaf leaf ->
        let leaf = V.delete leaf i j in
        return (Leaf leaf)
      | Node node ->
        match (choose node i, choose node (i + j)) with
        | Left, Left ->
          Store_.read_exn store node.left.key >>= fun t_left ->
          delete_rec (depth + 1) i t_left >>= fun t_left ->
          clean store depth t_left >>= fun t_left ->
          create_branch store t_left >>= fun b_left ->
          make_node (node.ind - j) (node.len - node.ind) b_left node.right >>= fun node ->
          rotate store (Node node) >>= fun node ->
          return (Node node)
        | Left, Right ->
          flush_internal store (Node node) >>= fun cont ->
          let cont = V.delete cont i j in
          make_internal store ~depth cont >>= fun t ->
          return t
        | Right, Left -> assert false
        | Right, Right ->
          Store_.read_exn store node.right.key >>= fun t_right ->
          delete_rec (depth + 1) (i - node.ind) t_right >>= fun t_right ->
          clean store depth t_right >>= fun t_right ->
          create_branch store t_right >>= fun b_right ->
          make_node node.ind (node.len - node.ind - j) node.left b_right >>= fun node ->
          rotate store (Node node) >>= fun node ->
          return (Node node)
    in

    Store_.read_exn store index.root >>= fun t ->
    delete_rec 1 i t >>= fun t ->
    create_index store t



  (*
   * Append the two given ropes. If the first rope is longer than the second,
   * then the second rope is inserted at the right most position in the first rope.
   * Else the first one is inserted at the left most position in the second rope.
  *)
  let (append, append_internal) =
    let open C in
    let rec append_left store t = function
      | Index _ -> assert false
      | Leaf leaf ->
        create_node store t (Leaf leaf) >>= fun node ->
        rotate store (Node node) >>= fun node ->
        return (Node node)
      | Node node ->
        Store_.read_exn store node.left.key >>= fun t_left ->
        append_left store t t_left >>= fun t_left ->
        create_branch store t_left >>= fun b_left ->
        make_node (get_length t_left) (node.len - node.ind) b_left node.right >>= fun node ->
        rotate store (Node node) >>= fun node ->
        return (Node node)
    in

    let rec append_right store t = function
      | Index _ -> assert false
      | Leaf leaf ->
        create_node store (Leaf leaf) t >>= fun node ->
        rotate store (Node node) >>= fun node ->
        return (Node node)
      | Node node ->
        Store_.read_exn store node.right.key >>= fun t_right ->
        append_right store t t_right >>= fun t_right ->
        create_branch store t_right >>= fun b_right ->
        make_node node.ind (get_length t_right) node.left b_right >>= fun node ->
        rotate store (Node node) >>= fun node ->
        return (Node node)
    in

    (
      (fun index1 index2 ->

         Store_.create () >>= fun store ->
         Store_.read_exn store index1.root >>= fun t1 ->
         Store_.read_exn store index2.root >>= fun t2 ->
         if (index1.length = 0) then return index2
         else if (index2.length = 0) then return index1
         else (
           if (index1.length < index2.length) then
             append_left store t1 t2
           else
             append_right store t2 t1
         ) >>= fun node ->
           create_index store node
      ),
      (fun store t1 t2 ->
         let l1 = get_length t1 in
         let l2 = get_length t2 in
         if (l1 = 0) then return t2
         else if (l2 = 0) then return t1
         else if (l1 < l2) then
           append_left store t1 t2
         else
           append_right store t2 t1
      )
    )


  let concat t list =
    flush t >>= fun t ->
    Lwt_list.map_s flush list >>= fun list ->
    let cont = V.concat t list in
    make cont


  (*
   * Split the rope at the position 'i'.
   * The internal tree is recursevely cut into two forest.
   * Then these forest are concatenated in order to obtain the two new ropes.
  *)
  let split index i =
    let open C in
    Store_.create () >>= fun store ->
    create () >>= fun empty ->

    let choose node i =
      if (node.len < i || i < 0) then
        invalid_arg (Printf.sprintf "try to access the position %i in a rope of size %i" i node.len)
      else
      if (i < node.ind) then Left else Right
    in

    let rec split_rec i l_acc r_acc = function
      | Index _ -> assert false
      | Leaf leaf ->
        let (l_left, l_right) = V.split leaf i in
        append_internal store l_acc (Leaf l_left) >>= fun l_acc ->
        append_internal store (Leaf l_right) r_acc >>= fun r_acc ->
        return (l_acc, r_acc)
      | Node node ->
        Store_.read_exn store node.left.key >>= fun t_left ->
        Store_.read_exn store node.right.key >>= fun t_right ->
        match choose node i with
        | Left ->
          append_internal store t_right r_acc >>= fun r_acc ->
          split_rec i l_acc r_acc t_left
        | Right ->
          append_internal store l_acc t_left >>= fun l_acc ->
          split_rec (i - node.ind) l_acc r_acc t_right
    in

    Store_.read_exn store index.root >>= fun t ->
    split_rec i (Leaf V.empty) (Leaf V.empty) t >>= fun (l_acc, r_acc) ->
    create_index store l_acc >>= fun t_left ->
    create_index store r_acc >>= fun t_right ->
    return (t_left, t_right)


  type rope = C.index

  module T = Tc.Biject (C.Index)
    (struct
      type t = rope
      let to_t v = v
      let of_t v = v
    end)
  include T

  type atom = V.atom
  type content = V.t

  (*
   * Merge the given two ropes with the help of their common ancestor. The
   * algorithm tries to deduce the smallest subtree on which modifications
   * occur, and then applies the user provided merge function on it. If the
   * modifications appear in different subtrees, the merge is automatically
   * solved, without the call of the user merge function.
  *)
  let merge: Path.t -> t option Irmin.Merge.t =
    let open C in
    let open Irmin.Merge.OP in
    let merge_cont = V.merge in

    let merge_flush store path old t1 t2 =
      if (C.compare t1 t2 = 0) then ok t1
      else
        flush_internal store old >>= fun old ->
        flush_internal store t1 >>= fun cont1 ->
        flush_internal store t2 >>= fun cont2 ->
        let old () = ok (Some (Some old)) in
        merge_cont path old (Some cont1) (Some cont2) >>= fun res ->
        match res with
        | `Conflict _ | `Ok None -> conflict "merge"
        | `Ok (Some cont) -> ok (Leaf cont)
    in

    let merge_branch store old1 old2 ((b11, l11), (b12, l12)) ((b21, l21), (b22, l22)) =
      if (b11.key = b21.key) then (Some (b11, l11))
      else if (b12.key = b22.key) then (
        if (old1.key = b11.key) then (Some (b21, l21))
        else if (old1.key = b21.key) then (Some (b11, l11))
        else None
      )
      else if (b12.key = old2.key && b21.key = old1.key) then (Some (b11, l11))
      else if (b11.key = old1.key && b22.key = old2.key) then (Some (b21, l21))
      else None
    in

    let rec merge_node store path old n1 n2 =
      let ln1 = (n1.left, n1.ind) in
      let rn1 = (n1.right, n1.len - n1.ind) in
      let ln2 = (n2.left, n2.ind) in
      let rn2 = (n2.right, n2.len- n2.ind) in
      let l_opt = merge_branch store old.left old.right (ln1, rn1) (ln2, rn2) in
      let r_opt = merge_branch store old.right old.left (rn1, ln1) (rn2, ln2) in
      match (l_opt, r_opt) with
      | None, None -> merge_flush store path (Node old) (Node n1) (Node n2)
      | None, Some (br, lr) -> (
          Store_.read_exn store br.key >>= fun t_right ->
          Store_.read_exn store old.left.key >>= fun old ->
          Store_.read_exn store n1.left.key >>= fun t1 ->
          Store_.read_exn store n2.left.key >>= fun t2 ->
          merge_elt store path old t1 t2 >>= fun res ->
          match res with
          | `Conflict _ -> conflict "merge"
          | `Ok t_left ->
            crt_cns_node store t_left (t_right, br.key) >>= fun node ->
            ok (Node node)
        )
      | Some (bl, ll), None -> (
          Store_.read_exn store bl.key >>= fun t_left ->
          Store_.read_exn store old.right.key >>= fun old ->
          Store_.read_exn store n1.right.key >>= fun t1 ->
          Store_.read_exn store n2.right.key >>= fun t2 ->
          merge_elt store path old t1 t2 >>= fun res ->
          match res with
          | `Conflict _ -> conflict "merge"
          | `Ok t_right ->
            cns_crt_node store (t_left, bl.key) t_right >>= fun node ->
            ok (Node node)
        )
      | Some (bl, ll), Some (br, lr) ->
        make_node ll lr bl br >>= fun node ->
        ok (Node node)

    and merge_elt store path old t1 t2 =
      match (t1, t2) with
      | Index _, _
      | _, Index _ -> assert false
      | Leaf _, _
      | _, Leaf _ -> merge_flush store path old t1 t2
      | Node node1, Node node2 ->
        match old with
        | Index _ -> assert false
        | Leaf _ -> merge_flush store path old t1 t2
        | Node old -> merge_node store path old node1 node2
    in

    let merge_rope path ~old r1 r2 =
      old () >>= function (* FIXME *)
      | `Conflict _ | `Ok None -> conflict "merge"
      | `Ok (Some old) ->
      Store_.create () >>= fun store ->
      Store_.read_exn store old.root >>= fun old ->
      Store_.read_exn store r1.root >>= fun t1 ->
      Store_.read_exn store r2.root >>= fun t2 ->
      merge_elt store path old t1 t2 >>= fun res ->
      match res with
      | `Conflict _ -> conflict "merge"
      | `Ok t ->
        create_index store t >>= fun index ->
        ok index
    in

    fun path -> Irmin.Merge.option (module T) (merge_rope path)


  let create = create
  let make = make
  let flush = flush_free

  let is_empty = is_empty
  let length = length

  let set t ~pos = set t pos
  let get t ~pos = get t pos
  let insert t ~pos cont = insert t pos cont
  let delete t ~pos ~len = delete t pos len
  let append t1 t2 = append t1 t2
  let concat ~sep list = concat sep list
  let split t ~pos = split t pos

end

module type S = sig
  include Ezirmin_repo.S

  type t
  type atom
  type content

  val create : unit -> t Lwt.t
  val make : content -> t Lwt.t
  val flush : t -> content Lwt.t

  val is_empty : t -> bool Lwt.t
  val length : t -> int Lwt.t

  val set : t -> pos:int -> atom -> t Lwt.t
  val get : t -> pos:int -> atom Lwt.t
  val insert : t -> pos:int -> content -> t Lwt.t
  val delete : t -> pos:int -> len:int -> t Lwt.t
  val append : t -> t -> t Lwt.t
  val concat : sep:t -> t list -> t Lwt.t
  val split : t -> pos:int -> (t * t) Lwt.t

  val write : ?message:string -> branch -> path:string list -> t -> unit Lwt.t
  val read  : branch -> path:string list -> t option Lwt.t
end

module Make(AO: Irmin.AO_MAKER)(S : Irmin.S_MAKER)(V : Content) : S
  with type atom = V.atom and type content = V.t = struct

  module R = Rope(AO)(V)
  include R

  module Repo = Ezirmin_repo.Make(S)(R)
  include Repo

  let head_name = "head"

  let write ?message t ~path v =
    let msg = match message with
    | None -> "write"
    | Some m -> m
    in
    let head = path @ [head_name] in
    Store.update (t msg) head v

  let read t ~path =
    let head = path @ [head_name] in
    Store.read (t "read") head

end
