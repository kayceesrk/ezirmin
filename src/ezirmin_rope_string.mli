module type S = sig
  include Ezirmin_repo.S

  type t
  type value = char
  type cont = string

  val create : unit -> t Lwt.t
  val make : string -> t Lwt.t
  val flush : t -> string Lwt.t

  val is_empty : t -> bool Lwt.t
  val length : t -> int Lwt.t

  val set : t -> pos:int -> char -> t Lwt.t
  val get : t -> pos:int -> char Lwt.t
  val insert : t -> pos:int -> string -> t Lwt.t
  val delete : t -> pos:int -> len:int -> t Lwt.t
  val append : t -> t -> t Lwt.t
  val concat : sep:t -> t list -> t Lwt.t
  val split : t -> pos:int -> (t * t) Lwt.t

  val write : ?message:string -> branch -> path:string list -> t -> unit Lwt.t
  val read  : branch -> path:string list -> t option Lwt.t
end

module Make(AO: Irmin.AO_MAKER)(S : Irmin.S_MAKER) : S
