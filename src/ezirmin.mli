(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** An easy interface on top of the Irmin library.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Ezirmin} *)

module type Log = sig

  (** {1 Append-only logs} *)

  (** [Log] provides mergeable append-only logs with support for paginated
      reads. Appending messages to logs and merging two logs are constant time
      operations. Logs can be read in reverse chronological order. Logs also
      provide support for paginated reads. *)

  (** {2 Repo operations} *)

  type repo
  (** The type of repository handles. A repository contains a set of branches. *)

  type branch
  (** The type of persistent branches. *)

  val init : ?root:string -> ?bare:bool -> unit -> repo Lwt.t
  (** Initialize and connect to the repository in a backend specific manner.

      {ul
      {- [root] is the local git repository's root directory. If unset, the
      current working directory is chosen to be the root.}
      {- If [bare] is set (unset by default), the git repository is made to be
      bare. Otherwise, the contents are expanded into the local filesystem on
      each update. This can cause performance issues on write heavy workloads.}
      } *)

  val master : repo -> branch Lwt.t
  (** [master repo] fetches the master branch of the repository. *)

  val get_branch : repo -> branch_name:string -> branch Lwt.t
  (** [get_branch repo b] fetches the branch [b] by name. If branch [b] does not
      already exist, then the branch will be created and a handle to it
      returned. *)

  val clone_force : branch -> string -> branch Lwt.t
  (** [clone_force b n] clones the branch [b] naming the new branch [n]. If a
      branch named [n] exists, then that branch will be overwritten. *)

  val merge : branch -> into:branch -> unit Lwt.t
  (** [merge w m] merges branch [w] into [m]. Merge will always succeed. After
      the merge the branch [w] continues to exist. *)

  (** {2 Log operations} *)

  type elt
  (** The type of value stored in the log. *)

  type cursor
  (** The type of cursor. The cursor is an abstraction that represents a specifc
      position in the log. *)

  val append : branch -> path:string list -> elt -> unit Lwt.t
  (** [append b p m] appends the message [m] to the log at path [b] in the
      branch [b]. Append is an O(1) operation. *)

  val get_cursor : branch -> path:string list -> cursor option Lwt.t
  (** [get_cursor b p] fetches the cursor that points to the latest message in
      in the log at path [p] in the branch [b]. If the log at path [p] is empty,
      then the operation returns [None]. *)

  val read : cursor -> num_items:int -> (elt list * cursor option) Lwt.t
  (** [read c n] reads the next [n] messages in reverse chronological order at
      the cursor. The operation returns the list of messages read and an
      optional cursor value that represents the tail of the last message read.
      The cursor can then be used to read subsequent messages.

      If the number of requested message is greater than the number of available
      messages in the log, then the operation returns the available messages and
      [None] for the cursor. *)

  val read_all : branch -> path:string list -> elt list Lwt.t
  (** [read_all b p] reads all the messages in the log at path [p] in the branch
      [b] in reverse chronological order. *)

  val install_listener : float -> unit
  (** Create a thread that actively polls for change. The parameter is the thread sleep
      time. Prefer {{:https://opam.ocaml.org/packages/inotify/inotify.2.0/}inotify}
      if it works on your system. *)

  val watch : branch -> path:string list -> (elt -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  (** [watch b p cb] watches the log at the path [p] in the branch [b]. On each
      append of a message [m] to the log, the callback function [cb m] is
      invoked. Before installing watches, a listener thread must be started with
      {!install_listener} that watches the store for changes.
      @return a function to disable the watch. *)

  val uninstall_listener : unit -> unit
  (** Stop the thread started by {!install_listener}. *)

end


module Git_FS_log (V : Tc.S0) : Log with type elt = V.t
(** A log abstraction that uses the git filesystem backend. *)

module Git_Memory_log (V : Tc.S0) : Log with type elt = V.t
(** A log abstraction that uses the git in-memory backend. *)

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
