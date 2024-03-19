open! Core
open Poly
open! Async
open! Import

let%expect_test "[within_temp_dir `In_temp_as]" =
  let jbuild = "jbuild" in
  let foobar = "foobar" in
  let%bind jbuild_contents = Reader.file_contents jbuild in
  let%bind () =
    within_temp_dir
      ~links:[ jbuild, `In_temp_as, foobar ]
      (fun () ->
        let%bind foobar_contents = Reader.file_contents "foobar" in
        require
          [%here]
          (jbuild_contents = foobar_contents)
          ~if_false_then_print_s:
            (lazy [%message "" (jbuild_contents : string) (foobar_contents : string)]);
        return ())
  in
  [%expect {| |}];
  return ()
;;

let%expect_test "[within_temp_dir `In_path_as]" =
  let%bind () =
    within_temp_dir
      ~links:[ "bin/raises.exe", `In_path_as, "foobar" ]
      (fun () ->
        let%bind () = system "which foobar >/dev/null && echo ok" in
        return ())
  in
  [%expect {| ok |}];
  return ()
;;

let%expect_test "[within_temp_dir] setup" =
  let test links =
    within_temp_dir ~links (fun () ->
      let%bind tmp = Sys.getcwd () in
      let path = Sys.getenv_exn "PATH" in
      (* Print any PATH entries under [tmp]. *)
      List.iter (String.split path ~on:':') ~f:(fun dir ->
        Option.iter (String.chop_prefix dir ~prefix:tmp) ~f:(fun suffix ->
          printf "In PATH: ${TMP}%s\n" suffix));
      (* Print all files and directories under [tmp]. *)
      run "tree" [ "--noreport" ])
  in
  (* no links *)
  let%bind () = test [] in
  [%expect {| . |}];
  (* temp links only *)
  let%bind () = test [ "/usr/share/dict/words", `In_temp_as, "dictionary" ] in
  [%expect {|
    .
    `-- dictionary
    |}];
  (* path links only *)
  let%bind () =
    test [ "/bin/true", `In_path_as, "true"; "/bin/false", `In_path_as, "false" ]
  in
  [%expect
    {|
    In PATH: ${TMP}/bin
    .
    `-- bin
        |-- false
        `-- true
    |}];
  (* temp and path links *)
  let%bind () =
    test
      [ "/usr/share/dict/words", `In_temp_as, "dictionary"
      ; "/bin/true", `In_path_as, "true"
      ; "/bin/false", `In_path_as, "false"
      ]
  in
  [%expect
    {|
    In PATH: ${TMP}/bin
    .
    |-- bin
    |   |-- false
    |   `-- true
    `-- dictionary
    |}];
  return ()
;;

let%expect_test "[within_temp_dir ~in_dir]" =
  let%bind cwd = Sys.getcwd () in
  (* without [in_dir] *)
  let%bind () =
    within_temp_dir (fun () ->
      let%bind cwd_within_temp = Sys.getcwd () in
      require
        [%here]
        (Filename.dirname cwd_within_temp <> cwd)
        ~if_false_then_print_s:
          (lazy [%message "" (cwd : string) (cwd_within_temp : string)]);
      return ())
  in
  [%expect {| |}];
  (* with [in_dir] *)
  let%bind () =
    within_temp_dir ~in_dir:cwd (fun () ->
      let%bind cwd_within_temp = Sys.getcwd () in
      require
        [%here]
        (Filename.dirname cwd_within_temp = cwd)
        ~if_false_then_print_s:
          (lazy [%message "" (cwd : string) (cwd_within_temp : string)]);
      return ())
  in
  [%expect {| |}];
  return ()
;;
