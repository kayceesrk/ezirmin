module type S = sig
  include Ezirmin_repo.S
  type elt

  val append     : branch -> path:string list -> elt -> unit Lwt.t
  val read_all   : branch -> path:string list -> elt list Lwt.t
  val watch : branch -> path:string list -> (elt -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
end

module Make
  (Backend : Irmin.S_MAKER)
  (V : Tc.S0) : S with type elt = V.t
