open! Core
open! Async
open! Import

let%expect_test "nothing captured" =
  print_endline "one";
  let%bind () =
    with_empty_expect_test_output_async (fun () ->
      print_endline "two";
      return ())
  in
  print_endline "three";
  [%expect
    {|
    one
    two
    three
    |}];
  return ()
;;

let%expect_test "output captured" =
  print_endline "one";
  let%bind () =
    with_empty_expect_test_output_async (fun () ->
      print_endline "two";
      [%expect {| two |}];
      return ())
  in
  print_endline "three";
  [%expect
    {|
    one
    three
    |}];
  return ()
;;

let%expect_test "partial output captured" =
  print_endline "one";
  let%bind () =
    with_empty_expect_test_output_async (fun () ->
      print_endline "two";
      [%expect {| two |}];
      print_endline "three";
      return ())
  in
  print_endline "four";
  [%expect
    {|
    one
    three
    four
    |}];
  return ()
;;

let%expect_test "nothing captured in nested calls" =
  print_endline "one";
  let%bind () =
    with_empty_expect_test_output_async (fun () ->
      print_endline "two";
      let%bind () =
        with_empty_expect_test_output_async (fun () ->
          print_endline "three";
          return ())
      in
      print_endline "four";
      return ())
  in
  print_endline "five";
  [%expect
    {|
    one
    two
    three
    four
    five
    |}];
  return ()
;;

let%expect_test "output captured in nested calls" =
  print_endline "one";
  let%bind () =
    with_empty_expect_test_output_async (fun () ->
      print_endline "two";
      let%bind () =
        with_empty_expect_test_output_async (fun () ->
          print_endline "three";
          [%expect {| three |}];
          return ())
      in
      print_endline "four";
      [%expect
        {|
        two
        four
        |}];
      return ())
  in
  print_endline "five";
  [%expect
    {|
    one
    five
    |}];
  return ()
;;

let%expect_test "output returned from nested calls" =
  print_endline "one";
  let%bind a, b =
    with_empty_expect_test_output_async (fun () ->
      print_endline "two";
      let%bind a =
        with_empty_expect_test_output_async (fun () ->
          print_endline "three";
          return (String.split_lines (expect_test_output ())))
      in
      print_endline "four";
      let b = String.split_lines (expect_test_output ()) in
      return (a, b))
  in
  print_endline "five";
  let c = String.split_lines (expect_test_output ()) in
  print_s [%message "" (a : string list) (b : string list) (c : string list)];
  [%expect
    {|
    ((a (three))
     (b (two four))
     (c (one five)))
    |}];
  return ()
;;

let%expect_test "multiple successive calls" =
  let%bind () =
    with_empty_expect_test_output_async (fun () ->
      let%bind () =
        with_empty_expect_test_output_async (fun () ->
          print_endline "one";
          return ())
      in
      with_empty_expect_test_output_async (fun () ->
        print_endline "two";
        return ()))
  in
  let%bind () =
    with_empty_expect_test_output_async (fun () ->
      let%bind () =
        with_empty_expect_test_output_async (fun () ->
          print_endline "three";
          return ())
      in
      with_empty_expect_test_output_async (fun () ->
        print_endline "four";
        return ()))
  in
  [%expect
    {|
    one
    two
    three
    four
    |}];
  return ()
;;

let%expect_test "asynchronous entry/exit" =
  let%bind () =
    require_does_raise_async (fun () ->
      with_empty_expect_test_output_async (fun () ->
        with_empty_expect_test_output_async (fun () -> Deferred.never ()) |> return))
  in
  [%expect {| "[with_empty_expect_test_output_async]: nesting mismatch" |}];
  return ()
;;

let%expect_test "asynchronous entry/exit inside blocking version" =
  require_does_raise (fun () ->
    with_empty_expect_test_output (fun () ->
      with_empty_expect_test_output_async (fun () -> Deferred.never ()))
    |> don't_wait_for);
  [%expect
    {|
    ("[with_empty_expect_test_output]: nesting mismatch"
     "check uses of [with_empty_expect_test_output_async]")
    |}];
  return ()
;;
