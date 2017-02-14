module type S = Ezirmin_rope.S with type atom = char and type content =string

module Make(AO: Irmin.AO_MAKER)(S : Irmin.S_MAKER) : S
