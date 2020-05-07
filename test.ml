open OUnit2
open Yojson.Basic.Util
open Game
open Main
open State
open Board

let tests = []

let suite =
  "test suite for A2"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite