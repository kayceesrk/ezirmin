let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_stamp h tags k ppf fmt =
      let stamp = None in
      let dt = 0. in
      Format.kfprintf k ppf ("%a[%0+04.0fus] @[" ^^ fmt ^^ "@]@.")
        Logs.pp_header (level, h) dt
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Logs.report = report }

let _ = Logs.set_reporter (reporter (Format.std_formatter))

let _ = Logs.set_level ~all:true (Some (Logs.Debug))

open Lwt.Infix

module Rope = Ezirmin.FS_rope_string
open Rope

let repo = Lwt_main.run (init ~root:"/tmp/ezirminr" ~bare:true ())
let mb = Lwt_main.run (master repo)
let ib = Lwt_main.run (get_branch repo "internal")

let push r b = Sync.push r ib >>= fun _ -> Sync.push r b
let pull r b k = Sync.pull r ib k >>= fun _ -> Sync.pull r b k

let remote = Sync.remote_uri "git+ssh://kc@dali-eu/tmp/ezirminr"
let _ = Lwt_main.run (pull remote mb `Merge)
