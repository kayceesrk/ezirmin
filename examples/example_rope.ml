open Lwt.Infix
open Ezirmin.Memory_rope_string

let m = Lwt_main.run (init () >>= master);;

let t = Lwt_main.run (
  make "they're good dogs Brent" >>= fun t ->
  write m [] t >>= fun _ ->
  to_string t >>= fun st ->
  print_endline st;
  Lwt.return t
)

let w = Lwt_main.run (clone_force m "w")

let _ = Lwt_main.run (
  delete t 4 1 >>= fun t' ->
  insert t' 4 " a" >>= fun t' ->
  write m [] t' >>= fun _ ->

  insert t 16 "go" >>= fun t' ->
  write w [] t' >>= fun _ ->

  merge w ~into:m >>= fun _ ->
  merge m ~into:w
);;

Lwt_main.run (
  read m [] >>= function
  | None -> failwith "impossible"
  | Some r -> flush r >|= fun s ->
  Printf.printf "m is \"%s\"\n" s
);;

Lwt_main.run (
  read w [] >>= function
  | None -> failwith "impossible"
  | Some r -> flush r >|= fun s ->
  Printf.printf "w is \"%s\"\n" s
)
