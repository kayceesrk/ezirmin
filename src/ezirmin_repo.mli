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
end
