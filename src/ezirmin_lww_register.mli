module type S = sig
  type repo
  type branch
  type value

  val init        : ?root:string -> ?bare:bool -> unit -> repo Lwt.t
  val master      : repo -> branch Lwt.t
  val get_branch  : repo -> branch_name:string -> branch Lwt.t
  val clone_force : branch -> string -> branch Lwt.t
  val merge       : branch -> into:branch -> unit Lwt.t

  val install_listener : unit -> unit

  val read  : branch -> path:string list -> value option Lwt.t
  val write : branch -> path:string list -> value -> unit Lwt.t

  val watch : branch -> path:string list
              -> ([ `Added of value | `Removed of value | `Updated of value * value ] -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
end

module Make
  (Backend : Irmin.S_MAKER)
  (V : Tc.S0) : S with type value = V.t
