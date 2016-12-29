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


module Make(AO : Irmin.AO_MAKER)(S : Irmin.S_MAKER)(V : Tc.S0)
  : S with type elt = V.t
