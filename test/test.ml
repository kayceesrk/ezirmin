(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

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

open Lwt.Infix

let _ = Lwt_log.add_rule "*" Lwt_log.Debug

module M = Ezirmin.Git_log.Make(Tc.String)
open M

let test_append_read_all =
  Lwt_log.info_f "(** append and read all **)" >>= fun () ->
  init "/tmp/ezirmin" false >>= master >>= fun m ->
  append m [] "hello1" >>= fun () ->
  append m [] "world1" >>= fun () ->
  read_all m [] >|= fun l ->
  List.iter (fun s -> Printf.printf "%s\n%!" s) l;
  m

let test_append_read_cursor m =
  append m [] "hello2" >>= fun () ->
  append m [] "world2" >>= fun () ->
  let rec loop cursor =
    read cursor 2 >>= fun (l, cursor) ->
    List.iter (fun s -> Printf.printf "%s\n%!" s) l;
    match cursor with
    | None -> Lwt.return ()
    | Some cursor -> loop cursor
  in
  get_cursor m [] >>= function
  | None -> failwith "test_append_read_cursor : impossibl"
  | Some c -> loop c

let _ = Lwt_main.run (test_append_read_all >>= test_append_read_cursor)
