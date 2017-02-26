(* #require "ezirmin" *)

let s =
 "To be, or not to be: that is the question:
  Whether ’tis nobler in the mind to suffer
  The slings and arrows of outrageous fortune,
  Or to take arms against a sea of troubles,
  And by opposing end them? To die: to sleep;
  No more; and by a sleep to say we end
  The heart-ache and the thousand natural shocks
  That flesh is heir to, ’tis a consummation
  Devoutly to be wish’d. To die, to sleep;
  To sleep: perchance to dream: ay, there’s the rub;
  For in that sleep of death what dreams may come
  When we have shuffled off this mortal coil,
  Must give us pause: there’s the respect
  That makes calamity of so long life;
  For who would bear the whips and scorns of time,
  The oppressor’s wrong, the proud man’s contumely,
  The pangs of despised love, the law’s delay,
  The insolence of office and the spurns
  That patient merit of the unworthy takes,
  When he himself might his quietus make
  With a bare bodkin? who would fardels bear,
  To grunt and sweat under a weary life,
  But that the dread of something after death,
  The undiscover’d country from whose bourn
  No traveller returns, puzzles the will
  And makes us rather bear those ills we have
  Than fly to others that we know not of?
  Thus conscience does make cowards of us all;
  And thus the native hue of resolution
  Is sicklied o’er with the pale cast of thought,
  And enterprises of great pith and moment
  With this regard their currents turn awry,
  And lose the name of action.–Soft you now!
  The fair Ophelia! Nymph, in thy orisons
  Be all my sins remember’d."

open Lwt.Infix

module Rope = Ezirmin.FS_rope_string
open Rope

let repo = Lwt_main.run (init ~root:"/tmp/ezirminr" ~bare:true ())
let mb = Lwt_main.run (master repo)
let ib = Lwt_main.run (get_branch repo "internal")

let ignore_lwt t = t >|= fun _ -> ()

let push r b = Lwt.join [ignore_lwt @@ Sync.push r ib;
                         ignore_lwt @@ Sync.push r b]
let pull r b k = Lwt.join [ignore_lwt @@ Sync.pull r ib k;
                           ignore_lwt @@ Sync.pull r b k]

let _ = Random.self_init ()

let gen_string length =
  let gen () = match Random.int (26 + 26 +10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
  let gen_char () = char_of_int @@ gen () in
  let b = Bytes.create length in
  for i = 0 to length-1 do
    Bytes.set b i @@ gen_char ()
  done;
  Bytes.to_string b

let perform_op r =
  let edit_length = 1 in
  let toss = Random.int 100 in
  length r >>= fun l ->
  let pos = Random.int l in
  if toss < 85 then
    let ns = gen_string edit_length in
    insert r pos ns
  else
    let pos = if l - pos < edit_length then pos - edit_length else pos in
    delete r pos edit_length

(* Commandline arguments *)
let _ =
  if Array.length Sys.argv < 4 then
    (Printf.printf "Usage: %s <num_ops : int> <sync_every : int> <first : bool> <remotes : comma-separated string list>\n" Sys.argv.(0);
     exit(1))

let num_ops = int_of_string @@ Sys.argv.(1)
let sync_every = int_of_string @@ Sys.argv.(2)
let is_first = bool_of_string @@ Sys.argv.(3)
let remotes_str =
  if Array.length Sys.argv > 4 then begin
    Stringext.full_split Sys.argv.(4) ','
  end else []

let remotes = List.map (fun r -> Sync.remote_uri ("git+ssh://kc@" ^ r ^ "/tmp/ezirminr")) remotes_str
let _ = Printf.printf "Num remotes=%d\n" (List.length remotes)

let sync_all () =
  let rec loop = function
    | [] -> Lwt.return ()
    | x::xs ->
        Printf.printf "Starting sync..\n%!";
        pull x mb `Merge >>= fun _ ->
        Printf.printf "Sync\n%!";
        loop xs
  in
  loop remotes

let rec edit r = function
  | 0 -> Lwt.return r
  | n ->
      (if n mod 10 = 0 then begin
        let p = int_of_float (100.0 *. float_of_int (num_ops - n) /. float_of_int num_ops) in
        Printf.printf "Completed=%d%%\n%!" p;
        write mb [] r
      end else
	Lwt.return ()) >>= fun () ->
      (if n mod sync_every = 0 then sync_all () else Lwt.return ()) >>= fun () ->
      length r >>= fun l ->
      if l > 10000 then begin
        delete r 0 1000 >>= fun r ->
        edit r n
      end else
        perform_op r >>= fun r ->
        edit r (n-1)

let rec daemon () =
  Lwt_unix.sleep 1.0 >>= fun () ->
  sync_all () >>=
  daemon

let main () =
  (if is_first then
    make s >>= fun r ->
    write mb [] r >>= fun _ ->
    Printf.printf "Committed init write\n";
    Lwt.return r
  else begin
    pull (List.hd remotes) mb `Merge >>= fun _ ->
    read mb [] >>= function
    | None -> failwith "Bench_rope: first read failed"
    | Some r ->
        Printf.printf "Read init write\n";
        Lwt.return r
  end) >>= fun r ->
  ignore (read_line ());
  let t = Unix.gettimeofday () in
  Lwt.async daemon;
  edit r num_ops >>= fun r ->
  Printf.printf "runtime: %f sec\n" (Unix.gettimeofday () -. t);
  length r

let l = Lwt_main.run (main ())
let _ = Printf.printf "Final length=%d" l
