open Irmin_unix

module Make(Backend : Irmin.S_MAKER)(C : Irmin.Contents.S) = struct

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

  let install_listener = set_listen_dir_hook
end
