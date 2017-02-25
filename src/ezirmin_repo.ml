open Irmin_unix
open Lwt.Infix

module type S = sig

  module Store : Irmin.S

  type repo
  type branch = string -> Store.t

  val init : ?root:string -> ?bare:bool -> unit -> repo Lwt.t
  val master : repo -> branch Lwt.t
  val get_branch : repo -> branch_name:string -> branch Lwt.t
  val get_branch_name : branch -> string option Lwt.t
  val clone_force : branch -> string -> branch Lwt.t
  val merge : branch -> into:branch -> unit Lwt.t

  val predecessors  : repo -> branch -> branch list Lwt.t
  val update_branch : branch -> set:branch -> unit Lwt.t
  module Commit : sig
    type t
    val commit_of_branch : branch -> t option Lwt.t
    val branch_of_commit : repo -> t -> branch Lwt.t
    val predecessors : t -> t list
    val compare_and_update_branch : branch -> expect:t option -> update:t option -> bool Lwt.t
  end

  val install_listener : unit -> unit

  module Sync : sig
    type remote = Irmin.remote
    val remote_uri : string -> remote
    val pull : remote -> branch -> [`Merge | `Update] -> [`Conflict of string | `Ok | `Error | `No_head ] Lwt.t
    val push : remote -> branch -> [`Ok | `Error] Lwt.t
  end
end

module Make(Backend : Irmin.S_MAKER)(C : Irmin.Contents.S) : sig
  include S with module Store = Backend(C)(Irmin.Ref.String)(Irmin.Hash.SHA1)
end = struct

  module Store = Backend(C)(Irmin.Ref.String)(Irmin.Hash.SHA1)
  module Sync_ = Irmin.Sync(Store)

  type repo = Store.Repo.t
  type branch = string -> Store.t

  type path = string list

  let init ?root ?bare () =
    let config = Irmin_git.config ?root ?bare () in
    Store.Repo.create config

  let master = Store.master task
  let clone_force t name = Store.clone_force task (t "cloning") name
  let get_branch r ~branch_name = Store.of_branch_id task branch_name r
  let merge b ~into = Store.merge_exn "" b ~into
  let get_branch_name b = Store.name (b "name")

  module Commit = struct
    type t =
      { history : Store.History.t;
        id      : Store.commit_id }

    let commit_of_branch b =
      Store.history (b "history") >>= fun history ->
      Store.head (b "head") >>= function
      | None -> Lwt.return None
      | Some id -> Lwt.return (Some { history; id })

    let branch_of_commit r c = Store.of_commit_id task c.id r

    let predecessors c =
      List.map (fun cid -> {id = cid; history = c.history}) @@ Store.History.pred c.history c.id

    let compare_and_update_branch b ~expect ~update =
      let option_map = function
      | None -> None
      | Some v -> Some v.id
      in
      Store.compare_and_set_head (b "cas") ~test:(option_map expect) ~set:(option_map update)
  end

  let predecessors r b =
    Commit.commit_of_branch b >>= function
    | None -> Lwt.return []
    | Some c ->
        let pred = Commit.predecessors c in
        Lwt_list.map_p (fun c -> Commit.branch_of_commit r c) pred

  let update_branch b ~set =
    Commit.commit_of_branch set >>= function
    | None -> failwith "Ezirmin_repo.update_branch: unexpected target branch"
    | Some c -> Store.update_head (b "rebase") c.Commit.id

  let install_listener = set_listen_dir_hook

  module Sync = struct

    type remote = Irmin.remote

    let remote_uri = Irmin.remote_uri

    let pull uri b k =
      Sync_.pull (b "pull") uri k >|= function
      | `Conflict s -> `Conflict s
      | `Ok (`Error) -> `Error
      | `Ok (`No_head) -> `No_head
      | `Ok (`Ok) -> `Ok

    let push uri b =
      Sync_.push (b "push") uri
  end
end
