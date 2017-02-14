module type Content = sig
  type atom
  include Irmin.Contents.S
    with module Path = Irmin.Path.String_list

  val empty : t
  val length : t -> int
  val set : t -> int -> atom -> t
  val get : t -> int -> atom
  val insert : t -> int -> t -> t
  val delete : t -> int -> int -> t
  val append : t -> t -> t
  val concat : t -> t list -> t
  val split : t -> int -> (t * t)
end

module type S = sig
  include Ezirmin_repo.S

  type t
  type atom
  type content

  val create : unit -> t Lwt.t
  val make : content -> t Lwt.t
  val flush : t -> content Lwt.t

  val is_empty : t -> bool Lwt.t
  val length : t -> int Lwt.t

  val set : t -> pos:int -> atom -> t Lwt.t
  val get : t -> pos:int -> atom Lwt.t
  val insert : t -> pos:int -> content -> t Lwt.t
  val delete : t -> pos:int -> len:int -> t Lwt.t
  val append : t -> t -> t Lwt.t
  val concat : sep:t -> t list -> t Lwt.t
  val split : t -> pos:int -> (t * t) Lwt.t

  val write : ?message:string -> branch -> path:string list -> t -> unit Lwt.t
  val read  : branch -> path:string list -> t option Lwt.t
end

module Make(AO: Irmin.AO_MAKER)(S : Irmin.S_MAKER)(V : Content) : S
  with type atom = V.atom and type content = V.t
