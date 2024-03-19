open! Core
open! Async
open! Import

let%expect_test "[require]" =
  require [%here] true;
  [%expect {| |}];
  require [%here] false ~cr:CR_someday;
  [%expect {|
    |}];
  require
    [%here]
    false
    ~cr:Comment
    ~if_false_then_print_s:(lazy [%sexp "additional-info"]);
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/async/test/test_require.ml:LINE:COL. *)
    additional-info
    |}];
  return ()
;;
