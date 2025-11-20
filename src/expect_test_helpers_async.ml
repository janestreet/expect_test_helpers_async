open! Core
open! Async
open Expect_test_helpers_core

module Print_rule = struct
  type t =
    | Always
    | If_unclean_exit
    | Never
  [@@deriving sexp_of]
end

let run
  ?(enable_ocaml_backtraces = false)
  ?(extend_env = [])
  ?(hide_positions = false)
  ?(postprocess = Fn.id)
  ?(print_cmdline = false)
  ?(print_stdout = Print_rule.Always)
  ?(print_stderr = Print_rule.Always)
  ?stdin
  ?working_dir
  prog
  args
  =
  let env =
    `Extend
      (if enable_ocaml_backtraces
       then extend_env
       else ("OCAMLRUNPARAM", "b=0") :: extend_env)
  in
  if print_cmdline
  then (
    let cmdline = prog :: args in
    match stdin with
    | None -> print_s [%message "run" (cmdline : string list)]
    | Some stdin -> print_s [%message "run" (cmdline : string list) (stdin : string)]);
  match%bind Process.create ?working_dir ~env ~prog ~args () with
  | Error error ->
    print_s
      [%message
        "Process creation failed"
          (prog : string)
          (args : string list)
          (working_dir : (string option[@sexp.option]))
          (error : Error.t)];
    return ()
  | Ok process ->
    (match stdin with
     | None -> ()
     | Some stdin -> Writer.write (Process.stdin process) stdin);
    let%bind { stdout; stderr; exit_status } = Process.collect_output_and_wait process in
    let maybe_hide_positions string =
      if not hide_positions then string else hide_positions_in_string string
    in
    let stdout = maybe_hide_positions stdout |> postprocess in
    let stderr = maybe_hide_positions stderr |> postprocess in
    let should_print : Print_rule.t -> bool = function
      | Always -> true
      | If_unclean_exit -> Result.is_error exit_status
      | Never -> false
    in
    if should_print print_stdout then print_string stdout;
    (match exit_status with
     | Ok () -> ()
     | Error err -> print_s [%message "Unclean exit" ~_:(err : Unix.Exit_or_signal.error)]);
    if should_print print_stderr && not (String.is_empty stderr)
    then (
      print_endline "--- STDERR ---";
      print_string stderr);
    return ()
;;

let system ?enable_ocaml_backtraces ?hide_positions ?print_cmdline ?stdin cmd =
  run
    ?enable_ocaml_backtraces
    ?hide_positions
    ?print_cmdline
    ?stdin
    "/bin/bash"
    [ "-c"; cmd ]
;;

let cleanup_dir__best_effort dir =
  (* there's various reasons why this might fail. Most commonly, it's because the test
     [chmod]s one of the files so we can't remove it. We've also observed apparent race
     conditions that give an error like

     rm: cannot remove '/tmp/._expect_.tmp.7M0HDW_test.tmp/some/path: Directory not empty

     [-f] already ignores many errors, and we also ignore the output and the exit code.
     Worst case, because we're in /tmp anyway, it'll be cleaned up later by the OS.
  *)
  match%map Process.run ~prog:"rm" ~args:[ "-rf"; dir ] () with
  | Ok _ | Error _ -> ()
;;

let with_temp_dir ?in_dir f =
  let in_dir =
    match in_dir with
    | None -> Sys.getenv "TMPDIR"
    | Some in_dir -> Some in_dir
  in
  let keep_tmp_dir = Option.is_some (Sys.getenv "KEEP_EXPECT_TEST_DIR") in
  let dir = Filename_unix.temp_dir ?in_dir "._expect-" "-test.tmp" in
  (* Note that this blocks *)
  assert (Filename.is_absolute dir);
  Monitor.protect
    ~run:`Schedule
    ~rest:`Log
    (fun () -> f dir)
    ~finally:(fun () ->
      if keep_tmp_dir
      then (
        eprintf "OUTPUT LEFT IN %s\n" dir;
        return ())
      else cleanup_dir__best_effort dir)
;;

let with_env_async env ~f =
  let env = String.Map.of_alist_exn env |> Map.map ~f:(fun data -> Some data) in
  let env_snapshot = Map.mapi env ~f:(fun ~key ~data:_ -> Sys.getenv key) in
  let set_env =
    Map.iteri ~f:(fun ~key ~data ->
      match data with
      | None -> (Unix.unsetenv [@ocaml.alert "-unsafe_multidomain"]) key
      | Some data -> (Unix.putenv [@ocaml.alert "-unsafe_multidomain"]) ~key ~data)
  in
  set_env env;
  Monitor.protect f ~finally:(fun () ->
    set_env env_snapshot;
    return ())
;;

let hardlink_or_copy ~orig ~dst =
  match%bind
    Monitor.try_with ~run:`Schedule ~rest:`Log ~extract_exn:true (fun () ->
      Unix.link ~target:orig ~link_name:dst ())
  with
  | Ok () -> return ()
  | Error (Unix.Unix_error (EXDEV, _, _)) -> run "cp" [ "-T"; "--"; orig; dst ]
  | Error e -> raise e
;;

let within_temp_dir ?in_dir ?(links = []) f =
  let%bind cwd = Unix.getcwd () in
  with_temp_dir ?in_dir (fun temp_dir ->
    let path_var = "PATH" in
    let old_path = Unix.getenv_exn path_var in
    let add_bin_to_path =
      Lazy_deferred.create (fun () ->
        let bin = temp_dir ^/ "bin" in
        (Unix.putenv [@ocaml.alert "-unsafe_multidomain"])
          ~key:path_var
          ~data:(String.concat ~sep:":" [ bin; old_path ]);
        run "mkdir" [ bin ])
    in
    let%bind () =
      Deferred.List.iter ~how:`Sequential links ~f:(fun (file, action, link_as) ->
        let%bind link_as =
          match action with
          | `In_path_as ->
            let%bind () = Lazy_deferred.force_exn add_bin_to_path in
            return ("bin" ^/ link_as)
          | `In_temp_as -> return link_as
        in
        hardlink_or_copy ~orig:file ~dst:(temp_dir ^/ link_as))
    in
    let%bind () = Unix.chdir temp_dir in
    Monitor.protect ~run:`Schedule ~rest:`Log f ~finally:(fun () ->
      (Unix.putenv [@ocaml.alert "-unsafe_multidomain"]) ~key:path_var ~data:old_path;
      Unix.chdir cwd))
;;

let sets_temporarily_async and_values ~f =
  let restore_to = List.map and_values ~f:Ref.And_value.snapshot in
  Ref.And_value.sets and_values;
  Monitor.protect ~run:`Schedule ~rest:`Log f ~finally:(fun () ->
    Ref.And_value.sets restore_to;
    return ())
;;

let set_temporarily_async r x ~f = sets_temporarily_async [ T (r, x) ] ~f

let with_temporarily_async d x ~f =
  let restore_to = Dynamic.get d in
  Dynamic.set_root d x;
  Monitor.protect f ~finally:(fun () ->
    Dynamic.set_root d restore_to;
    return ())
;;

let with_empty_expect_test_output_async ~(here : [%call_pos]) f =
  let frame =
    (Ppx_expect_runtime.For_external.push_output_exn [@alert "-ppx_expect_runtime"]) ~here
  in
  Monitor.protect f ~finally:(fun () ->
    match
      (Ppx_expect_runtime.For_external.pop_output_exn [@alert "-ppx_expect_runtime"])
        ~here
        frame
    with
    | Match -> return ()
    | Mismatch ->
      raise_s [%message "[with_empty_expect_test_output_async]: nesting mismatch"])
;;

let try_with f ~rest =
  let monitor = Monitor.create () in
  Monitor.detach_and_iter_errors monitor ~f:(fun exn -> rest (Monitor.extract_exn exn));
  Scheduler.within' ~monitor (fun () ->
    Monitor.try_with ~run:`Schedule ~extract_exn:true ~rest:`Raise f)
;;

let show_raise_async
  (type a)
  ?hide_positions
  ?(sanitize = Fn.id)
  ?show_backtrace
  (f : unit -> a Deferred.t)
  =
  let%map result =
    try_with f ~rest:(fun exn ->
      let exn : Sexp.t = sanitize [%sexp (exn : exn)] in
      print_s ?hide_positions [%message "Raised after return" ~_:(exn : Sexp.t)])
  in
  show_raise ?hide_positions ~sanitize ?show_backtrace (fun () -> Result.ok_exn result)
;;

let require_does_not_raise_async
  ?cr
  ?hide_positions
  ?(sanitize = Fn.id)
  ?show_backtrace
  ~(here : [%call_pos])
  f
  =
  let%map result =
    try_with f ~rest:(fun exn ->
      let exn : Sexp.t = sanitize [%sexp (exn : exn)] in
      print_cr
        ~here
        ?cr
        ?hide_positions
        [%message "Raised after return" ~_:(exn : Sexp.t)])
  in
  require_does_not_raise ?cr ?hide_positions ~sanitize ?show_backtrace ~here (fun () ->
    Result.ok_exn result)
;;

let require_does_raise_async
  ?(cr = CR.CR)
  ?(hide_positions = CR.hide_unstable_output cr)
  ?(sanitize = Fn.id)
  ?(show_backtrace = false)
  ~(here : [%call_pos])
  f
  =
  let%map result =
    try_with
      (fun () ->
        let backtrace_recording = Backtrace.Exn.am_recording () in
        Backtrace.Exn.set_recording show_backtrace;
        Monitor.protect f ~finally:(fun () ->
          Backtrace.Exn.set_recording backtrace_recording;
          return ()))
      ~rest:(fun exn ->
        (* It's not clear what do if we get exceptions after the deferred is returned...
           Just printing out "Raised after return" for now. *)
        print_s
          ~hide_positions
          [%message
            "Raised after return" ~_:(here : Source_code_position.t) ~_:(exn : exn)])
  in
  require_does_raise ~cr ~hide_positions ~sanitize ~show_backtrace ~here (fun () ->
    Result.ok_exn result)
;;

let tty_log =
  lazy
    (Log.create
       ~level:`Info
       ~output:
         (match Out_channel.create "/dev/tty" with
          | oc -> [ Log.Output.writer `Text (Writer.of_out_channel oc Char) ]
          | exception _ -> [])
         (* We can explicitly use the wall clock because this output is designed to bypass
            the expect test output capture mechanism. *)
       ~time_source:(Synchronous_time_source.wall_clock ())
         (* [`Raise] causes background errors to be sent the monitor in effect when
            [create] is called. Since this value is lazy, it is not predictable which
            monitor is active when [create] actually gets called, so we send the exn to
            the main monitor instead.

            This code is copied from the implementation of {!Async.Log.Global.Make.log}. *)
       ~on_error:
         (`Call
           (fun e ->
             let e = Error.to_exn e in
             Monitor.send_exn Monitor.main ~backtrace:`Get e))
       ())
;;

let remove_connection_details =
  smash_sexp ~f:(function
    | Sexp.(List [ (Atom "Client connected via TCP" as a); _ ]) ->
      Sexp.(List [ a; Atom "HOST PORT" ])
    | s -> s)
;;

let with_robust_global_log_output ?(map_output = Fn.id) fn =
  let sexp_wrap f s =
    match Sexp.of_string s with
    | sexp -> sexp |> f |> sexp_to_string
    | exception _ -> s
  in
  let output_mappers =
    [ sexp_wrap (Fn.compose remove_backtraces remove_connection_details)
    ; remove_time_spans
    ; map_output
    ]
  in
  let map_output init = List.fold ~init ~f:(fun s f -> f s) output_mappers in
  let old_outputs = Log.Global.get_output () in
  Monitor.protect
    ~finally:(fun () -> Log.Global.set_output old_outputs |> return)
    (fun () ->
      Log.Global.set_output [ Log.For_testing.create_output ~map_output ];
      fn ())
;;

let with_sexp_round_floats f ~significant_digits =
  let restore_to = Dynamic.get Sexplib0.Sexp_conv.default_string_of_float in
  Dynamic.set_root
    Sexplib0.Sexp_conv.default_string_of_float
    (Portability_hacks.magic_portable__needs_base_and_core (fun v ->
       Float.to_string (Float.round_significant ~significant_digits v)));
  Monitor.protect f ~finally:(fun () ->
    Dynamic.set_root Sexplib0.Sexp_conv.default_string_of_float restore_to;
    Deferred.unit)
;;
