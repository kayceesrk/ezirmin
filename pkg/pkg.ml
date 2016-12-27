#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ezirmin" @@ fun c ->
  Ok [
    Pkg.mllib "src/ezirmin.mllib";
    Pkg.test "test/test_log";
    Pkg.test "test/test_queue";
  ]
