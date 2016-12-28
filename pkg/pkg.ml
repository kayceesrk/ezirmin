#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ezirmin" @@ fun c ->
  Ok [
    Pkg.mllib "src/ezirmin.mllib";
    Pkg.test "examples/example_log";
    Pkg.test "examples/example_queue";
    Pkg.bin "bench/bench_blob_log";
    Pkg.bin "bench/bench_log";
  ]
