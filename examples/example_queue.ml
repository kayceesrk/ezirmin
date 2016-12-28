(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

module M = Ezirmin.Memory_queue(Tc.Int)
open M

let rec iter f c =
  next c >>= function
  | None -> Lwt.return ()
  | Some (e,c') ->
      assert (not (c = c'));
      f e; iter f c'


let push_pop () =
  Printf.printf "\n(** push & pop **)\n";
  init ~root:"/tmp/ezirmin_queue" () >>= master >>= fun m ->
  push m [] 0 >>= fun () ->
  push m [] 1 >>= fun () ->
  Printf.printf "[master] pushed : 0 1\n";
  pop_exn m [] >>= fun i ->
  pop_exn m [] >>= fun j ->
  Lwt.return @@ Printf.printf "[master] popped : %d %d\n" i j

let branch_test () =
  Printf.printf "\n(** push & pop **)\n";
  init ~root:"/tmp/ezirmin_queue" () >>= master >>= fun m ->
  push m [] 0 >>= fun () ->
  push m [] 1 >>= fun () ->
  Printf.printf "[master] pushed : 0 1\n";
  clone_force m "working" >>= fun w ->
  pop_exn m [] >>= fun i ->
  Printf.printf "[master] popped : %d\n" i;
  pop_exn w [] >>= fun i ->
  Printf.printf "[working] popped : %d\n" i;
  push m [] 2 >>= fun () ->
  Printf.printf "[master] pushed : 2\n";
  push w [] 3 >>= fun () ->
  Printf.printf "[working] pushed : 3\n";
  merge w ~into:m >>= fun () ->
  get_cursor m [] >>= function
  | None -> failwith "impossible"
  | Some c ->
      Printf.printf "[master] ";
      iter (Printf.printf "%d ") c >>= fun () ->
      print_endline "";
      Lwt.return ()

let _ = Lwt_main.run (
  push_pop () >>=
  branch_test
)
