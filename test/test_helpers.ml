open! Core
open! Async
open! Import

let raises_exe = "bin/raises.exe"

let%expect_test "[~hide_positions:true] with a [Time.t]" =
  print_s ~hide_positions:true [%message (Time.epoch : Time.t)];
  [%expect {| (Time.epoch (1969-12-31 19:00:00.000000-05:00)) |}];
  return ()
;;

let%expect_test "run" =
  let%bind () = run "echo" [ "foo" ] in
  [%expect {| foo |}];
  return ()
;;

let%expect_test "run, with print_cmdline:true" =
  let%bind () = run ~print_cmdline:true "echo" [ "foo" ] in
  [%expect
    {|
    (run (cmdline (echo foo)))
    foo
    |}];
  return ()
;;

let with_cd_into_temp_dir f =
  with_temp_dir (fun dir ->
    let%bind cwd = Unix.getcwd () in
    let%bind () = Unix.chdir dir in
    Monitor.protect ~run:`Schedule ~rest:`Log f ~finally:(fun () -> Unix.chdir cwd))
;;

let%expect_test "run, with working dir" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "mkdir" [ "foo" ] in
    let%bind () = run "touch" [ "foo/bar" ] in
    let%bind () = run "ls" [] in
    [%expect {| foo |}];
    let%bind () = run "ls" [] ~working_dir:"./foo" in
    [%expect {| bar |}];
    return ())
;;

let%expect_test "run, with no expansion" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "echo" [ "~" ] in
    [%expect {| ~ |}];
    return ())
;;

let%expect_test "run, with stdin" =
  let%bind () = run "cat" [ "-" ] ~stdin:"foo $PATH" in
  [%expect {| foo $PATH |}];
  return ()
;;

let%expect_test "run, with stdin and print_cmdline:true" =
  let%bind () = run ~print_cmdline:true "cat" [ "-" ] ~stdin:"foo" in
  [%expect
    {|
    (run (cmdline (cat -)) (stdin foo))
    foo
    |}];
  return ()
;;

let%expect_test "run, with stderr and non-zero exit" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "cat" [ "./i-hope-this-does-not-exist" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      cat: ./i-hope-this-does-not-exist: No such file or directory
      |}];
    return ())
;;

let%expect_test "run, with print_stdout/print_stderr overrides" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "echo" [ "success" ] ~print_stdout:If_unclean_exit in
    [%expect {| |}];
    let%bind () =
      run "cat" [ "./i-hope-this-does-not-exist" ] ~print_stderr:If_unclean_exit
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      cat: ./i-hope-this-does-not-exist: No such file or directory
      |}];
    let%bind () = run "cat" [ "./i-hope-this-does-not-exist" ] ~print_stderr:Never in
    [%expect {| ("Unclean exit" (Exit_non_zero 1)) |}];
    return ())
;;

let%expect_test "run, with bad exec" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "./i-hope-this-does-not-exist" [] in
    [%expect
      {|
      ("Process creation failed"
        (prog ./i-hope-this-does-not-exist)
        (args ())
        (error (
          Unix.Unix_error
          "No such file or directory"
          Core_unix.create_process
          "((prog ./i-hope-this-does-not-exist) (args ()) (env (Extend ((OCAMLRUNPARAM b=0)))))")))
      |}];
    return ())
;;

let%expect_test "[run ~hide_positions:true]" =
  let%bind () =
    run ~hide_positions:true "echo" [ [%message [%here]] |> Sexp.to_string ]
  in
  [%expect {| lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL |}];
  return ()
;;

let%expect_test "run, with postprocess" =
  let%bind () =
    run
      ~hide_positions:true
      ~postprocess:(fun string ->
        String.substr_replace_all string ~pattern:"test" ~with_:"foo")
      "echo"
      [ [%message [%here]] |> Sexp.to_string ]
  in
  [%expect {| lib/expect_foo_helpers/async/foo/foo_helpers.ml:LINE:COL |}];
  return ()
;;

let%expect_test "system" =
  let%bind () = system {| echo $((1 + 1)) | tee /dev/stderr |} in
  [%expect
    {|
    2
    --- STDERR ---
    2
    |}];
  return ()
;;

let%expect_test "system, with cmdline" =
  let%bind () = system ~print_cmdline:true {| echo $((1 + 1)) | tee /dev/stderr |} in
  [%expect
    {|
    (run (cmdline (/bin/bash -c " echo $((1 + 1)) | tee /dev/stderr ")))
    2
    --- STDERR ---
    2
    |}];
  return ()
;;

let%expect_test "system, with non-zero exit" =
  let%bind () = system {| kill $$ |} in
  [%expect {| ("Unclean exit" (Signal sigterm)) |}];
  return ()
;;

let%expect_test "system, with multi-line command" =
  let%bind () =
    system
      {|
      for i in $(seq 1 10); do
        echo $i
      done; |}
  in
  [%expect
    {|
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10
    |}];
  return ()
;;

let%expect_test "system, with stdin" =
  let%bind () = system {| cat - |} ~stdin:"foo $PATH" in
  [%expect {| foo $PATH |}];
  return ()
;;

let%expect_test "system, with non-existent command" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = system "./i-hope-this-does-not-exist" in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 127))
      --- STDERR ---
      /bin/bash: ./i-hope-this-does-not-exist: No such file or directory
      |}];
    return ())
;;

let%expect_test "system, with a bash-ism not in POSIX or POSIX-mode bash" =
  let%bind () = system "cat <(echo foo)" in
  [%expect {| foo |}];
  return ()
;;

let%expect_test "[system ~hide_positions:true]" =
  let%bind () =
    system
      ~hide_positions:true
      (concat [ "echo >&2 '"; [%message [%here]] |> Sexp.to_string; "'" ])
  in
  [%expect
    {|
    --- STDERR ---
    lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL
    |}];
  return ()
;;

let%expect_test "system, without backtraces" =
  let%bind () = system ~enable_ocaml_backtraces:false raises_exe in
  [%expect
    {|
    ("Unclean exit" (Exit_non_zero 2))
    --- STDERR ---
    Uncaught exception:

      "An exception appeared!"
    |}];
  return ()
;;

let%expect_test "run, without backtraces" =
  let%bind () = run ~enable_ocaml_backtraces:false raises_exe [] in
  [%expect
    {|
    ("Unclean exit" (Exit_non_zero 2))
    --- STDERR ---
    Uncaught exception:

      "An exception appeared!"
    |}];
  return ()
;;

(* Can't test [run ~enable_ocaml_backtraces:true] because the backtraces are fragile. *)

let%expect_test "[show_raise_async], no exception, ignores return value" =
  let%bind () = show_raise_async ~hide_positions:true (fun () -> Deferred.return 1) in
  [%expect {| "did not raise" |}];
  return ()
;;

let%expect_test "[show_raise_async], raises hiding positions" =
  let%bind () =
    show_raise_async ~hide_positions:true (fun () -> raise_s [%message [%here]])
  in
  [%expect {| (raised lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL) |}];
  return ()
;;

let%expect_test "[show_raise_async] with a deep stack" =
  let rec loop n =
    if n = 0
    then failwith "raising"
    else (
      let%map r = loop (n - 1) in
      r + 1)
  in
  let%bind () = show_raise_async (fun () -> loop 5) in
  [%expect {| (raised (Failure raising)) |}];
  return ()
;;

let%expect_test "[show_raise_async], raise after return" =
  let returned = Ivar.create () in
  let%bind () =
    show_raise_async ~hide_positions:true (fun () ->
      upon (Ivar.read returned) (fun () ->
        Exn.raise_without_backtrace (Failure "raise after return"));
      Deferred.unit)
  in
  [%expect {| "did not raise" |}];
  Ivar.fill_exn returned ();
  let%bind () = Scheduler.yield () in
  [%expect {| ("Raised after return" (Failure "raise after return")) |}];
  return ()
;;

let%expect_test "[require_does_not_raise_async], no raise" =
  let%bind () = require_does_not_raise_async (fun () -> return ()) in
  [%expect {| |}];
  return ()
;;

let%expect_test "[require_does_not_raise_async], raises" =
  let%bind () =
    require_does_not_raise_async ~cr:Comment (fun () -> raise_s [%message "KABOOM"])
  in
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL. *)
    ("unexpectedly raised" KABOOM)
    |}];
  return ()
;;

let%expect_test "[require_does_not_raise_async], raise after return" =
  let returned = Ivar.create () in
  let%bind () =
    require_does_not_raise_async ~cr:Comment (fun () ->
      upon (Ivar.read returned) (fun () -> raise_s [%message "KABOOM"]);
      return ())
  in
  [%expect {| |}];
  Ivar.fill_exn returned ();
  let%bind () = Scheduler.yield () in
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL. *)
    ("Raised after return" KABOOM)
    |}];
  return ()
;;

let%expect_test "[require_does_raise_async], no raise" =
  let%bind () =
    require_does_raise_async ~cr:Comment (fun () -> return `ignore_return_value)
  in
  [%expect
    {|
    (* require-failed: lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL. *)
    "did not raise"
    |}];
  return ()
;;

let%expect_test "[require_does_raise_async], raises" =
  let%bind () = require_does_raise_async (fun () -> raise_s [%message "KABOOM"]) in
  [%expect {| KABOOM |}];
  return ()
;;

let%expect_test "[require_does_raise_async], raise after return" =
  let returned = Ivar.create () in
  let%bind () =
    require_does_raise_async ~cr:Comment (fun () ->
      upon (Ivar.read returned) (fun () -> raise_s [%message "also KAPOW"]);
      raise_s [%message "KABOOM"])
  in
  [%expect {| KABOOM |}];
  Ivar.fill_exn returned ();
  let%bind () = Scheduler.yield () in
  [%expect
    {|
    ("Raised after return"
     lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL
     "also KAPOW")
    |}];
  return ()
;;

let%expect_test "[require_does_raise_async], backtrace" =
  let test show_backtrace =
    let%map () =
      require_does_raise_async ~show_backtrace (fun () ->
        Monitor.try_with ~extract_exn:false (fun () ->
          (* raise an exception *)
          try raise_s [%message "oops"] with
          | exn ->
            (* annotate and re-raise with backtrace *)
            let backtrace = Backtrace.Exn.most_recent () in
            raise_s [%message "raising" (exn : exn) (backtrace : Backtrace.t)])
        >>| function
        | Ok (_ : Nothing.t) -> .
        | Error exn ->
          (* catch and re-raise with monitor backtrace, round-tripped through sexp so that
             [extract_exn] doesn't remove the monitor backtrace *)
          raise_s (sexp_of_exn exn))
    in
    (* placeholders instead of actual backtraces *)
    expect_test_output () |> Sexp.of_string |> remove_backtraces |> print_s
  in
  let%bind () = test false in
  [%expect {| (monitor.ml.Error (raising (exn oops) (backtrace ()))) |}];
  let%bind () = test true in
  [%expect
    {|
    ((monitor.ml.Error
       (raising (exn oops) (backtrace ("ELIDED BACKTRACE")))
       ("ELIDED BACKTRACE"))
     (backtrace ("ELIDED BACKTRACE")))
    |}];
  return ()
;;

let%expect_test "remove_connection_details" =
  {|(connection_description
     ("Client connected via TCP" (some-random-hostname 38133))) |}
  |> Sexp.of_string
  |> remove_connection_details
  |> print_s;
  [%expect {| (connection_description ("Client connected via TCP" "HOST PORT")) |}];
  return ()
;;

let%expect_test "with_robust_global_log_output" =
  with_robust_global_log_output (fun () ->
    {| ("Task error after 320.4ms."
      (errors
       (((rpc_error
          (Uncaught_exn
           ((location "server-side rpc computation")
            (exn
             (monitor.ml.Error ("Fake Failure" (message "Deliberate failure after 23.2ms"))
              ("Raised at Base__Error.raise in file \"error.ml\" (inlined), line 9, characters 14-30"
               "Called from Base__Error.raise_s in file \"error.ml\", line 10, characters 19-40"
               "Called from Base__Result.try_with in file \"result.ml\", line 227, characters 9-15"))))))
         (connection_description
         ("Client connected via TCP" (some-random-hostname 38133)))
         (rpc_tag rpc_parallel_plain_1) (rpc_version 0))))) |}
    |> Sexp.of_string
    |> Log.Global.error_s;
    let%bind () = Log.Global.flushed () in
    [%expect
      {|
      ("Task error after SPAN." (
        errors ((
          (rpc_error (
            Uncaught_exn (
              (location "server-side rpc computation")
              (exn (
                monitor.ml.Error
                ("Fake Failure" (message "Deliberate failure after SPAN"))
                ("ELIDED BACKTRACE"))))))
          (connection_description ("Client connected via TCP" "HOST PORT"))
          (rpc_tag     rpc_parallel_plain_1)
          (rpc_version 0)))))
      |}];
    return ())
;;

let%expect_test "with_robust_global_log_output with ~map_output" =
  let replace_o = String.tr ~target:'o' ~replacement:'0' in
  with_robust_global_log_output ~map_output:replace_o (fun () ->
    "(errors (Hello World!))" |> Sexp.of_string |> Log.Global.error_s;
    let%bind () = Log.Global.flushed () in
    [%expect {| (err0rs (Hell0 W0rld!)) |}];
    return ())
;;

let%expect_test "[remove_backtraces] with no interesting backtrace lines except the \
                 monitor"
  =
  let wrap_exn unwrapped_exn =
    let monitor = Monitor.create ~name:"my-monitor" () in
    let wrapped_exn_deferred = Monitor.detach_and_get_next_error monitor in
    let backtrace = `This (Backtrace.get ~at_most_num_frames:0 ()) in
    Monitor.send_exn monitor ~backtrace unwrapped_exn;
    wrapped_exn_deferred
  in
  let%bind exn = wrap_exn (Error.to_exn (Error.of_string "my-error")) in
  let sexp = [%sexp (exn : exn)] in
  print_s sexp;
  [%expect
    {|
    (monitor.ml.Error my-error (
      "Caught by monitor my-monitor at file \"lib/expect_test_helpers/async/test/test_helpers.ml\", line LINE, characters C1-C2"))
    |}];
  print_s (remove_backtraces sexp);
  [%expect {| (monitor.ml.Error my-error ("ELIDED BACKTRACE")) |}];
  return ()
;;
