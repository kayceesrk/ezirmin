module type Container = sig
  type a
  include Irmin.Contents.S
    with module Path = Irmin.Path.String_list

  val empty : t
  val length : t -> int
  val set : t -> int -> a -> t
  val get : t -> int -> a
  val insert : t -> int -> t -> t
  val delete : t -> int -> int -> t
  val append : t -> t -> t
  val concat : t -> t list -> t
  val split : t -> int -> (t * t)
end

module type S = sig
  include Ezirmin_repo.S

  type t
  type value
  type cont

  val create : unit -> t Lwt.t
  val make : cont -> t Lwt.t
  val flush : t -> cont Lwt.t

  val is_empty : t -> bool Lwt.t
  val length : t -> int Lwt.t

  val set : t -> pos:int -> value -> t Lwt.t
  val get : t -> pos:int -> value Lwt.t
  val insert : t -> pos:int -> cont -> t Lwt.t
  val delete : t -> pos:int -> len:int -> t Lwt.t
  val append : t -> t -> t Lwt.t
  val concat : sep:t -> t list -> t Lwt.t
  val split : t -> pos:int -> (t * t) Lwt.t

  val write : ?message:string -> branch -> path:string list -> t -> unit Lwt.t
  val read  : branch -> path:string list -> t option Lwt.t
end

module Make(AO: Irmin.AO_MAKER)(S : Irmin.S_MAKER)(V : Container) : S
  with type value = V.a and type cont = V.t
