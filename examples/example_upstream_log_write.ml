(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

module M = Ezirmin.Memory_log(Tc.String)
open M

let uri = ref None
let logfile = ref None

let is_later newc ~than:oldc =
  match newc, oldc with
  | Some _, None -> true
  | None, None -> false
  | None, Some _ -> failwith "impossible"
  | Some n, Some o ->
      match is_later n ~than:o with
      | None -> failwith "impossible"
      | Some r -> r

let main uri bname =
  let remote = Sync.remote_uri uri in
  init ~bare:true ~root:"/tmp/ezirmin" () >>= fun repo ->
  get_branch repo bname >>= fun b ->
  let rec logger () =
    Lwt_io.read_line_opt Lwt_io.stdin >>= function
    | Some l -> append b ["logs"] l >>= logger
    | None -> Lwt.return ()
  in
  let pusher () =
    get_cursor b ["logs"] >>= fun cursor ->
    let rec loop cursor =
      Printf.printf "here1\n%!";
      Lwt_unix.sleep 1.0 >>= fun () ->
      Printf.printf "here2\n%!";
      get_cursor b ["logs"] >>= fun new_cursor ->
      Printf.printf "here3\n%!";
      if is_later new_cursor ~than:cursor then begin
        Printf.printf "here4\n%!";
        Sync.push remote b >>= function
        | `Ok -> loop new_cursor
        | `Error -> failwith "push error"
      end else
        (Printf.printf "here5\n%!";
        loop cursor)
    in
    loop cursor
  in
  Lwt.join [logger (); pusher ()]

let options = Arg.align
  [ "-uri", Arg.String (fun u -> uri := Some u), " github uri";
    "-logfile", Arg.String (fun l -> logfile := Some l), " name of the logfile" ]

let _ =
  Arg.parse options (fun _ -> ()) "example_upstream_log [options]";
  let uri,logfile = match !uri, !logfile with
  | Some u, Some l  -> u, l
  | _ ->
    Arg.usage options "Require logfile and github repo uri (https://<username>:<token>@github.com/<handle>/<repo>.git)";
    raise Exit
  in
  Lwt_main.run @@ main uri logfile
