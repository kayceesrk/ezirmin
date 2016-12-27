module type S = sig
  include Ezirmin_repo.S

  type value
  val read  : branch -> path:string list -> value option Lwt.t
  val write : branch -> path:string list -> value -> unit Lwt.t
  val watch : branch -> path:string list
              -> ([ `Added of value | `Removed of value | `Updated of value * value ] -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
end

module Make
  (Backend : Irmin.S_MAKER)
  (V : Tc.S0) : S with type value = V.t
