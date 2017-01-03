(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

module M = Ezirmin.Memory_log(Tc.String)
open M

let uri = ref None
let logfiles = ref []

let debug = true

let dprintf s =
  if debug then Printf.printf s
  else Printf.ifprintf stdout s

let main uri =
  let remote = Sync.remote_uri uri in

  init ~bare:true ~root:"/tmp/ezirmin" () >>= fun repo ->
  master repo >>= fun m ->

  Sync.pull remote m `Update >>= fun _ ->

  Lwt_list.iter_s (fun bn ->
    get_branch repo bn >>= fun b ->
    Sync.pull remote b `Update >>= fun _ ->
    merge b ~into:m) !logfiles >>= fun () ->

  Sync.push remote m >>= function
  | `Ok ->
      read_all m ["logs"] >>= fun log ->
      Lwt_list.iter_s Lwt_io.printl log
  | `Error -> failwith "push error"

let options = Arg.align
  [ "-uri", Arg.String (fun u -> uri := Some u), " github uri";
    "-logfile", Arg.String (fun l -> logfiles := l::!logfiles), " name of the logfile" ]

let _ =
  Arg.parse options (fun _ -> ()) "example_upstream_log [options]";
  let uri = match !uri with
  | Some u -> u
  | _ ->
    Arg.usage options "Require github repo uri (https://<username>:<token>@github.com/<handle>/<repo>.git)";
    raise Exit
  in
  Lwt_main.run @@ main uri
