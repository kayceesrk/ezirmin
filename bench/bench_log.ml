(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

module M = Ezirmin.FS_log(Tc.String)
open M

let main =
  init ~bare:false ~root:"/tmp/ezirmin" () >>= master >>= fun m ->
  let rec loop () =
    Lwt_io.read_line_opt Lwt_io.stdin >>= function
    | Some l -> append m [] l >>= loop
    | None -> (* read_all m [] >>= fun l ->
        Printf.printf "total elements=%d\n" (List.length l); *)
        Lwt.return ()
  in
  loop ()

let _ = Lwt_main.run main
