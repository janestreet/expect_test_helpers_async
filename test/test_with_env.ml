open! Core
open! Async
open! Import

let print_env keys =
  let snapshot = List.map keys ~f:(fun key -> key, Unix.getenv key) in
  print_s [%sexp (snapshot : (string * string option) list)]
;;

let%expect_test "[with_env]" =
  let foo = "EXPECT_TEST_FOO" in
  let bar = "EXPECT_TEST_BAR" in
  let baz = "EXPECT_TEST_BAZ" in
  let print_env () = print_env [ foo; bar; baz ] in
  Monitor.protect
    ~finally:(fun () ->
      (Unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) foo;
      (Unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) bar;
      (Unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) baz;
      return ())
    (fun () ->
      (Unix.putenv [@ocaml.alert "-unsafe_multidomain"]) ~key:bar ~data:"old value of bar";
      (Unix.putenv [@ocaml.alert "-unsafe_multidomain"]) ~key:baz ~data:"old value of baz";
      print_env ();
      [%expect
        {|
        ((EXPECT_TEST_FOO ())
         (EXPECT_TEST_BAR ("old value of bar"))
         (EXPECT_TEST_BAZ ("old value of baz")))
        |}];
      let%bind () =
        with_env_async
          [ foo, "scoped value of foo"; bar, "scoped value of bar" ]
          ~f:(fun () ->
            print_env ();
            [%expect
              {|
              ((EXPECT_TEST_FOO ("scoped value of foo"))
               (EXPECT_TEST_BAR ("scoped value of bar"))
               (EXPECT_TEST_BAZ ("old value of baz")))
              |}];
            (Unix.putenv [@ocaml.alert "-unsafe_multidomain"])
              ~key:foo
              ~data:"temporary value of foo";
            (Unix.putenv [@ocaml.alert "-unsafe_multidomain"])
              ~key:baz
              ~data:"new value of baz";
            print_env ();
            [%expect
              {|
              ((EXPECT_TEST_FOO ("temporary value of foo"))
               (EXPECT_TEST_BAR ("scoped value of bar"))
               (EXPECT_TEST_BAZ ("new value of baz")))
              |}];
            return ())
      in
      print_env ();
      [%expect
        {|
        ((EXPECT_TEST_FOO ())
         (EXPECT_TEST_BAR ("old value of bar"))
         (EXPECT_TEST_BAZ ("new value of baz")))
        |}];
      return ())
;;

let%expect_test "[with_env] with raising f" =
  let foo = "EXPECT_TEST_FOO" in
  let print_env () = print_env [ foo ] in
  Monitor.protect
    ~finally:(fun () ->
      (Unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) foo;
      return ())
    (fun () ->
      print_env ();
      [%expect {| ((EXPECT_TEST_FOO ())) |}];
      let%bind () =
        require_does_raise_async (fun () ->
          with_env_async
            [ foo, "scoped value of foo" ]
            ~f:(fun () ->
              print_env ();
              [%expect {| ((EXPECT_TEST_FOO ("scoped value of foo"))) |}];
              failwith "error"))
      in
      [%expect {| (Failure error) |}];
      print_env ();
      [%expect {| ((EXPECT_TEST_FOO ())) |}];
      return ())
;;
