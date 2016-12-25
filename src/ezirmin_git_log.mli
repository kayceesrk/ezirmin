module type S = sig
  type repo
  type branch

  val init        : ?root:string -> ?bare:bool -> unit -> repo Lwt.t
  val master      : repo -> branch Lwt.t
  val get_branch  : repo -> branch_name:string -> branch Lwt.t
  val clone_force : branch -> string -> branch Lwt.t
  val merge       : branch -> into:branch -> unit Lwt.t

  type elt
  type cursor

  val append     : branch -> path:string list -> elt -> unit Lwt.t
  val get_cursor : branch -> path:string list -> cursor option Lwt.t
  val read       : cursor -> num_items:int -> (elt list * cursor option) Lwt.t
  val read_all   : branch -> path:string list -> elt list Lwt.t

  val install_listener : unit -> unit
  val watch : branch -> path:string list -> (elt -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
end

module Make
  (Backend : Irmin.S_MAKER)
  (V : Tc.S0) : S with type elt = V.t
