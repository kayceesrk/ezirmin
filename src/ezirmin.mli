(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** An easy interface on top of the Irmin library.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Ezirmin} *)

module type Repo = sig

  (** {1 Repo operations} *)

  (** [Repo] provides operations on repositories. *)

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

  val get_branch_name : branch -> string option Lwt.t
  (** Return the branch name. Return [None] if the branch is not persistent. *)

  val clone_force : branch -> string -> branch Lwt.t
  (** [clone_force b n] clones the branch [b] naming the new branch [n]. If a
      branch named [n] exists, then that branch will be overwritten. *)

  val merge : branch -> into:branch -> unit Lwt.t
  (** [merge w m] merges branch [w] into [m]. Merge will always succeed. After
      the merge the branch [w] continues to exist. *)

  (** {2 History} *)

  val predecessors : repo -> branch -> branch list Lwt.t
  (** Return the list of predecessors of the head of the branch. *)

  val update_branch : branch -> set:branch -> unit Lwt.t
  (** [update_branch b s] updates the head of the branch [b] to the head of the
      branch [s]. *)

  module Commit : sig
    type t
    (** The type of commit. *)

    val commit_of_branch : branch -> t option Lwt.t
    (** Get the commit corresponding to the head of the branch. If the branch is
        empty, then return [None]. *)

    val branch_of_commit : repo -> t -> branch Lwt.t
    (** Get the branch that corresponds to a commit. *)

    val predecessors : t -> t list
    (** Get the predecessors or the commit. *)

    val compare_and_update_branch : branch -> expect:t option -> update:t option -> bool Lwt.t
    (** [compare_and_update_branch b e u] compares the head of the branch [b]
        to the commit [e]. If they are the same, then the head of branch [b] is
        updated to [u]. Returns [true] if the update is successful. *)
  end

  (** {2 Watch} *)

  val install_listener : unit -> unit
  (** Create a thread that actively polls for change. Prefer
      {{:https://opam.ocaml.org/packages/inotify/inotify.2.0/}inotify} if it
      works on your system. *)

  (** {2 Sync} *)

  (** [Sync] provides functionality to sync with remote repositories. *)

  module Sync : sig
    type remote
    (** The type of remote. *)

    val remote_uri : string -> remote
    (** Return a remote for the URI string. *)

    val pull : remote -> branch -> [`Merge | `Update] -> [`Conflict of string | `Ok | `Error | `No_head ] Lwt.t
    (** Pull updates from remote. *)

    val push : remote -> branch -> [`Ok | `Error] Lwt.t
    (** Push updates to remote. *)
  end

  module Store : Irmin.S
  (** Underlying Irmin store. *)
end

(** {2 Mergeable datastructures} *)

module type Blob_log = sig
  (** {1 An append-only log} *)

  (** [Blob_log] provides mergeable append-only logs. Appending to log is a
      [O(n)] operation, where [n] is the size of the size of the log. For an
      efficient implementation see, {!Log}. *)

  include Repo

  type elt
  (** The type of value stored in the log. *)

  val append : ?message:string -> branch -> path:string list -> elt -> unit Lwt.t
  (** [append m b p e] appends a log message [e] to the log at path [p] in branch
      [b] with commit message [m]. *)

  val read_all : branch -> path:string list -> elt list Lwt.t
  (** [read_all b p] returns the list of messages in the log at path [b] in
      branch [b]. *)

  val watch : branch -> path:string list -> (elt -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  (** [watch b p cb] watches the log at the path [p] in the branch [b]. On each
      append of a message [m] to the log, the callback function [cb m] is
      invoked. Before installing watches, a listener thread must be started
      with {!Repo.install_listener} that watches the store for changes.
      @return a function to disable the watch. *)
end

module type Counter = sig

  (** {1 Mergeable counters} *)

  include Repo

  val inc : ?message:string -> ?by:int -> branch -> path:string list -> unit Lwt.t
  (** [inc m d b p] increments the counter at path [p] in branch [b] by [d].
      The commit message is [m]. *)

  val dec : ?message:string -> ?by:int -> branch -> path:string list -> unit Lwt.t
  (** [dec m d b p] decrements the counter at path [p] in branch [b] by [d].
      The commit message is [m]. *)

  val read : branch -> path:string list -> int Lwt.t
  (** [read b p] reads the value of the counter at path [p] in branch [b]. *)

  val watch : branch -> path:string list -> (int -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  (** [watch b p cb] watches the counter at the path [p] in the branch [b]. On each
      counter update, the callback function [cb v] is invoked, where [v] is the
      new value of the counter. Before installing watches, a listener thread
      must be started with {!Repo.install_listener} that watches the store for
      changes. @return a function to disable the watch. *)
end

module type Log = sig

  (** {1 An efficient write-optimized append-only logs} *)

  (** [Log] provides mergeable append-only logs with support for paginated
      reads. Appending messages to logs and merging two logs are constant time
      operations. Logs can be read in reverse chronological order. Logs also
      provide support for paginated reads. *)

  include Repo

  type elt
  (** The type of value stored in the log. *)

  type cursor
  (** The type of cursor. The cursor is an abstraction that represents a specifc
      position in the log. *)

  val append : ?message:string -> branch -> path:string list -> elt -> unit Lwt.t
  (** [append m b p c] appends the message [m] to the log at path [b] in the
      branch [b] with commit message [c]. Append is an O(1) operation. *)

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

  val at_time : cursor -> Ptime.t option
  (** [at_time c] returns the timestamp of the latest message in the cursor [c]. *)

  val is_earlier : cursor -> than:cursor -> bool option
  (** [is_earlier x y] returns [true] if the latest message in [x] is older
      than the latest message in [y]. *)

  val is_later : cursor -> than:cursor -> bool option
  (** [is_later x y] returns [true] if the latest message in [x] is newer
      than the latest message in [y]. *)

  (** {2 Watch} *)

  val watch : branch -> path:string list -> (elt -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  (** [watch b p cb] watches the log at the path [p] in the branch [b]. On each
      append of a message [m] to the log, the callback function [cb m] is
      invoked. Before installing watches, a listener thread must be started
      with {!Repo.install_listener} that watches the store for changes.
      @return a function to disable the watch. *)
end

module type Lww_register = sig

  (** {1 Last-write-wins register} *)

  (** [Lww_register] provides registers that can be read and written to. In
      case of conflicting writes, the latest write wins. *)

  include Repo

  type value
  (** The type of content stored in the register. *)

  val read : branch -> path:string list -> value option Lwt.t
  (** [read b p] fetches the value of the register at path [b] in the branch
      [b]. If the register had not been previously written to, then the
      operation returns [None]. *)

  val write : ?message:string -> branch -> path:string list -> value -> unit Lwt.t
  (** [write b p m v] updates the value of the register at path [b] in branch [b]
      to [v] with commit message [m]. *)

  val watch : branch -> path:string list
              -> ([ `Added of value | `Removed of value | `Updated of value * value ] -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
  (** [watch b p cb] watches the register at path [p] in the branch [b], where
      [cb] is the callback function.
      @return a function to disable the watch. *)
end

module type Queue = sig

  (** {1 Mergeable Queue} *)

  (** [Queue] provides double-ended queue with automatic merges. *)

  include Repo

  type elt
  (** The type of element in the queue. *)

  val create : ?message:string -> branch -> path:string list -> unit Lwt.t
  (** Create a new queue. If a queue already exists at this path, then this
      operation overwrites the old queue. *)

  val length : branch -> path:string list -> int Lwt.t
  (** Return the length of the queue. If the queue does not exist, then [0] is
      returned. *)

  val is_empty : branch -> path:string list -> bool Lwt.t
  (** Returns [true] if the queue is empty. If the queue does not exist, then
      return [true]. *)

  val push : ?message:string -> branch -> path:string list -> elt -> unit Lwt.t
  (** [push m b p e] pushes element [e] to the back of the queue at path [p] in
      branch [b] with commit message [m]. If the queue does not exist at this
      path, then an empty queue is created and [e] is pushed to the back of
      this queue. *)

  val pop_exn : ?message:string -> branch -> path:string list -> elt Lwt.t
  (** Pop an element from the front of the queue. If the queue is empty or does
      not exist at this path, then exception [Empty] is raised. *)

  val pop : ?message:string -> branch -> path:string list -> elt option Lwt.t
  (** Pop an element from the front of the queue. If the queue is empty of does
      not exist at this path, then [None] is returned. *)

  val to_list : branch -> path:string list -> elt list Lwt.t
  (** Return a list with the elements in the queue. *)

  (** {2 Iteration} *)

  type cursor
  (** The type of cursor. *)

  val get_cursor : branch -> path:string list -> cursor option Lwt.t
  (** Return the cursor that points to the head of the queue. If the queue does
      not exist, return [None]. *)

  val next_exn : cursor -> (elt * cursor) Lwt.t
  (** Get the next element in the queue. Return a pair of the head element and a
      cursor that points to the tail of the queue. Raise [Empty] if the cursor
      points to an empty queue. *)

  val next : cursor -> (elt * cursor) option Lwt.t
  (** Get the next element in the queue. Return a pair of the head element and a
      cursor that points to the tail of the queue. Return [None] if the cursor
      points to an empty queue. *)

  val iter : branch -> path:string list -> f:(elt -> unit) -> unit Lwt.t
  (** [iter b p f] applies f on each element of the queue from the front to the
      back of the queue. *)

  (** {2 Watch} *)

  val watch : branch -> path:string list -> (unit -> unit Lwt.t)
              -> (unit -> unit Lwt.t) Lwt.t
  (** [watch b p cb] watches the queue at path [p] in branch [b], and invokes
      the callback function if there are any updates.
      @return a function to disable the watch. *)
end

module type Rope_container = sig
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

module type Rope = sig
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

module Make_rope (AO : Irmin.AO_MAKER) (S: Irmin.S_MAKER) (C : Rope_container) : Rope
  with type value = C.a and type cont = C.t

module type Rope_string = Rope with type cont = string and type value = char

(** {2 File system backend} *)

(** Mergeable datatypes instantiated with git filesystem backend. *)

module FS_blob_log (V : Tc.S0) : Blob_log with type elt = V.t
(** A log abstraction that uses git filesystem backend. *)

module FS_counter : Counter
(** A counter that uses git filesystem backend. *)

module FS_log (V : Tc.S0) : Log with type elt = V.t
(** An efficient write-optimized log abstraction that uses the git filesystem
    backend. *)

module FS_lww_register (V : Tc.S0) : Lww_register with type value = V.t
(** An Lww_reigster that uses the git filesystem backend. *)

module FS_queue (V : Tc.S0) : Queue with type elt = V.t
(** A Queue that uses the git filesystem backend. *)

module FS_rope_string : Rope_string

(** {2 In-memory backend} *)

(** Mergeable datatypes instantiated with git in-memory backend. *)

module Memory_blob_log (V : Tc.S0) : Blob_log with type elt = V.t
(** A log abstraction that uses git in-memory backend. *)

module Memory_counter : Counter
(** A counter that uses git in-memory backend. *)

module Memory_log (V : Tc.S0) : Log with type elt = V.t
(** An efficient write-optimized log abstraction that uses the git in-memory
    backend. *)

module Memory_lww_register (V : Tc.S0) : Lww_register with type value = V.t
(** An Lww_register abstraction that uses the git in-memory backend. *)

module Memory_queue (V : Tc.S0) : Queue with type elt = V.t
(** A Queue that uses the git in-memory backend. *)

module Memory_rope_string : Rope_string

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
