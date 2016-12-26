(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module type Repo = sig
  type repo
  type branch

  val init        : ?root:string -> ?bare:bool -> unit -> repo Lwt.t
  val master      : repo -> branch Lwt.t
  val get_branch  : repo -> branch_name:string -> branch Lwt.t
  val clone_force : branch -> string -> branch Lwt.t
  val merge       : branch -> into:branch -> unit Lwt.t
  val install_listener : unit -> unit
end

module type Log = Ezirmin_log.S

module FS_log(V:Tc.S0) =
  Ezirmin_log.Make(Irmin_unix.Irmin_git.FS)(V)
module Memory_log(V:Tc.S0) =
  Ezirmin_log.Make(Irmin_unix.Irmin_git.Memory)(V)

module type Lww_register = Ezirmin_lww_register.S

module FS_lww_register (V : Tc.S0) =
  Ezirmin_lww_register.Make(Irmin_unix.Irmin_git.FS)(V)
module Memory_lww_register (V : Tc.S0) =
  Ezirmin_lww_register.Make(Irmin_unix.Irmin_git.Memory)(V)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
