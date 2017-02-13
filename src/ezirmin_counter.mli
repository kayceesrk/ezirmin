module type S = sig
  include Ezirmin_repo.S

  val inc  : ?message:string -> ?by:int -> branch -> path:string list -> unit Lwt.t
  val dec  : ?message:string -> ?by:int -> branch -> path:string list -> unit Lwt.t
  val read : branch -> path:string list -> int Lwt.t
  val watch : branch -> path:string list -> (int -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
end

module Make (Backend : Irmin.S_MAKER) : S
