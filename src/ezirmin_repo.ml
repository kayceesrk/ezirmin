open Irmin_unix

module type S = sig
  type repo
  type branch

  val init : ?root:string -> ?bare:bool -> unit -> repo Lwt.t
  val master : repo -> branch Lwt.t
  val get_branch : repo -> branch_name:string -> branch Lwt.t
  val get_branch_name : branch -> string option Lwt.t
  val clone_force : branch -> string -> branch Lwt.t
  val merge : branch -> into:branch -> unit Lwt.t
  val install_listener : unit -> unit
end

module Make(Backend : Irmin.S_MAKER)(C : Irmin.Contents.S) : sig
  module Store : (module type of (Backend(C)(Irmin.Ref.String)(Irmin.Hash.SHA1)))
  include S with type branch = string -> Store.t
end = struct

  module Store = Backend(C)(Irmin.Ref.String)(Irmin.Hash.SHA1)

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

  let install_listener = set_listen_dir_hook
end
