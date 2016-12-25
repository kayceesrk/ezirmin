(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

module M = Ezirmin.Git_FS_log(Tc.String)
open M

let rec append_msgs m = function
  | [] -> Lwt.return ()
  | x::xs ->
      append m [] x >>= fun () ->
      append_msgs m xs

let read_all_incrementally m =
  let rec loop cursor =
    read cursor 2 >>= fun (l, cursor) ->
    List.iter (fun s -> Printf.printf "%s\n" s) l;
    match cursor with
    | None -> Printf.printf "<read_done>\n"; Lwt.return ()
    | Some cursor -> Printf.printf "<read_more>..\n"; loop cursor
  in
  get_cursor m [] >>= function
  | None -> failwith "test_append_read_cursor : impossible"
  | Some c -> loop c

let test_append_read_all () =
  Printf.printf "\n(** append and read all **)\n";
  init ~root:"/tmp/ezirmin" () >>= master >>= fun m ->
  append_msgs m ["master.1"; "master.2"] >>= fun () ->
  read_all m [] >|= fun l ->
  List.iter (fun s -> Printf.printf "%s\n" s) l

let test_append_read_incr () =
  Printf.printf "\n(** append and read incrementally **)\n";
  init ~root:"/tmp/ezirmin" () >>= master >>= fun m ->
  append_msgs m ["master.3"; "master.4"] >>= fun () ->
  read_all_incrementally m

let test_branch_append_read_incr () =
  Printf.printf "\n(** branch, append and read incrementally **)\n";
  init ~root:"/tmp/ezirmin" () >>= master >>= fun m ->
  clone_force m "working" >>= fun w ->
  append_msgs w ["working.1"; "working.2"] >>= fun () ->
  append_msgs m ["master.5"; "master.6"] >>= fun () ->
  merge w ~into:m >>= fun () ->
  append_msgs m ["master.7"; "master.8"] >>= fun () ->
  read_all_incrementally m

let test_get_branch () =
  Printf.printf "\n(** get branch **)\n";
  init ~root:"/tmp/ezirmin" () >>= fun r ->
  get_branch r "foobar" >>= fun fb ->
  append_msgs fb ["foobar.1"; "foobar.2"] >>= fun () ->
  read_all fb [] >|= fun l ->
  List.iter (fun s -> Printf.printf "%s\n" s) l

let test_watch () =
  Printf.printf "\n(** watch **)\n%!";
  init ~root:"/tmp/ezirmin" () >>= master >>= fun m ->
  install_listener ();
  watch m [] (Lwt_io.printf "%s\n") >>= fun unwatch ->
  append_msgs m ["master.9"; "master.10"; "master.11"] >>= fun () ->
  unwatch () >>= fun () -> (* stop watching *)
  append_msgs m ["master.12"; "master.13"; "master.14"]

let _ = Lwt_main.run (
  test_append_read_all () >>=
  test_append_read_incr >>=
  test_branch_append_read_incr >>=
  test_get_branch >>=
  test_watch
)

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
