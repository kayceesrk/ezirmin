(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module type Repo = Ezirmin_repo.S

module Make_git_AO_maker (G : Git.Store.S) (K : Irmin.Hash.S) (V : Tc.S0) = struct
  module M = Irmin_unix.Irmin_git.AO(G)(K)(V)
  include M

  let create config =
    let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
      "level" Irmin.Private.Conf.(some int) None
    in
    let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
    let level = Irmin.Private.Conf.get config level in
    G.create ?root ?level ()
end

module type Blob_log = Ezirmin_blob_log.S

module FS_blob_log(V:Tc.S0) =
  Ezirmin_blob_log.Make(Irmin_unix.Irmin_git.FS)(V)
module Memory_blob_log(V:Tc.S0) =
  Ezirmin_blob_log.Make(Irmin_unix.Irmin_git.Memory)(V)

module type Log = Ezirmin_log.S

module FS_log(V:Tc.S0) =
  Ezirmin_log.Make(Make_git_AO_maker(Git_unix.FS))(Irmin_unix.Irmin_git.FS)(V)
module Memory_log(V:Tc.S0) =
  Ezirmin_log.Make(Make_git_AO_maker(Git_unix.Memory))(Irmin_unix.Irmin_git.Memory)(V)

module type Lww_register = Ezirmin_lww_register.S

module FS_lww_register (V : Tc.S0) =
  Ezirmin_lww_register.Make(Irmin_unix.Irmin_git.FS)(V)
module Memory_lww_register (V : Tc.S0) =
  Ezirmin_lww_register.Make(Irmin_unix.Irmin_git.Memory)(V)

module type Queue = Ezirmin_queue.S

module Memory_queue (V : Tc.S0) =
  Ezirmin_queue.Make(Make_git_AO_maker(Git_unix.Memory))(Irmin_unix.Irmin_git.Memory)(V)
module FS_queue (V : Tc.S0) =
  Ezirmin_queue.Make(Make_git_AO_maker(Git_unix.FS))(Irmin_unix.Irmin_git.FS)(V)

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
