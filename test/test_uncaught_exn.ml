open! Core
open! Async
open! Import

let () = Dynamic.set_root Backtrace.elide true

let%expect_test "without an [%expect]" =
  Printexc.record_backtrace false;
  failwith "foo"
[@@expect.uncaught_exn
  {|
  (monitor.ml.Error (Failure foo)
    ("<backtrace elided in test>" "Caught by monitor block_on_async"))
  |}]
;;

let%expect_test "with an [%expect]" =
  Printexc.record_backtrace false;
  ignore (failwith "foo" : _);
  [%expect.unreachable];
  return ()
[@@expect.uncaught_exn
  {|
  (monitor.ml.Error (Failure foo)
    ("<backtrace elided in test>" "Caught by monitor block_on_async"))
  |}]
;;
