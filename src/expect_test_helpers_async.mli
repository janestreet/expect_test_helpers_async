(** Helpers for producing output inside [let%expect_test]. Designed for code using
    [Async]. See also [Expect_test_helpers_base] and [Expect_test_helpers_core]. *)

open! Core
open! Async
open! Import
open Expect_test_helpers_core

(** [with_temp_dir f] creates a temporary directory which is fed to [f]. The directory is
    removed after [f] exits. *)
val with_temp_dir : ?in_dir:string -> (string -> 'a Deferred.t) -> 'a Deferred.t

(** [within_temp_dir ?links f] creates a temporary directory, $T, and:

    1. For each [file, `In_path_as, name] in [links], links [file] as $T/bin/[name].
    2. For each [file, `In_temp_as, name] in [links], links [file] as $T/[name].
    3. Adds $T/bin to the PATH environment variable if step 1 added any links.

    It then [cd]s to $T and calls [f]. After [f] exits, it [cd]s back, removes the
    temporary directory, and restores the original PATH.

    [within_temp_dir] creates hard links to ensure that files remain available and
    unchanged even if jenga starts to rebuild while the test is running. If [file] and $T
    are not on the same file system, [within_temp_dir] copies the files instead of
    creating hard links. *)
val within_temp_dir
  :  ?in_dir:string
  -> ?links:(string * [ `In_path_as | `In_temp_as ] * string) list
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t

(** [with_env] sets specified environment variables before calling [f]. After [f] exits,
    these variables are reset to their previous values. *)
val with_env_async : (string * string) list -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t

(** Like [Ref.set_temporarily], but waits for a deferred function to finish. *)
val set_temporarily_async : 'a ref -> 'a -> f:(unit -> 'b Deferred.t) -> 'b Deferred.t

(** Like [Dynamic.with_temporarily], but waits for a deferred function to finish.

    Uses [Dynamic.set_root] and [Dynamic.get], so if this is for some reason called from
    within [Dynamic.with_temporarily], it will have confusing behavior. *)
val with_temporarily_async
  : ('a : value mod contended) 'b.
  'a Dynamic.t -> 'a @ portable -> f:(unit -> 'b Deferred.t) -> 'b Deferred.t

(** Like [Ref.sets_temporarily], but waits for a deferred function to finish. *)
val sets_temporarily_async
  :  Ref.And_value.t list
  -> f:(unit -> 'a Deferred.t)
  -> 'a Deferred.t

(** Like [Expect_test_helpers_base.with_empty_expect_test_output], for async callbacks. *)
val with_empty_expect_test_output_async
  :  here:[%call_pos]
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t

module Print_rule : sig
  type t =
    | Always
    | If_unclean_exit
    | Never
  [@@deriving sexp_of]
end

(** [run prog args] creates a child process that runs [prog], with arguments [args], its
    environment extended with [extend_env] and its stdin coming from [stdin]. No expansion
    or escaping is done to [args] or [stdin]. The child process's stdout and stderr are
    captured separately for comparison. *)
val run
  :  ?enable_ocaml_backtraces:bool (** default is [false] *)
  -> ?extend_env:(string * string) list (** default is [] *)
  -> ?hide_positions:bool (** default is [false] *)
  -> ?postprocess:(string -> string) (** default is [Fn.id] *)
  -> ?print_cmdline:bool (** default is [false] *)
  -> ?print_stdout:Print_rule.t (** default is [Always] *)
  -> ?print_stderr:Print_rule.t (** default is [Always] *)
  -> ?stdin:string
  -> ?working_dir:string
  -> string
  -> string list
  -> unit Deferred.t

(** [system ?stdin cmd] creates a child process that runs [/bin/sh], with arguments
    ["-c"; cmd] and its stdin coming from [stdin]. The child process's stdout and stderr
    are captured separately for comparison. Unlike with [Unix.system], the child shell's
    stdin is never a tty, even if the stdin of this process is a tty, and the child
    shell's stderr is never copied to this process's stderr. *)
val system
  :  ?enable_ocaml_backtraces:bool (** default is [true] *)
  -> ?hide_positions:bool (** default is [false] *)
  -> ?print_cmdline:bool (** default is [false] *)
  -> ?stdin:string
  -> string
  -> unit Deferred.t

(** [show_raise_async ?hide_positions ?rest f] calls [f ()] and prints either the
    exception raised by [f] or "did not raise". [show_raise_async] ignores the result of
    [f] so that one doesn't have to put an [ignore] inside [f]. [~hide_positions] operates
    as in [print_s], to make output less fragile. Once a result is returned, the rest of
    the errors are printed to stdout. [~sanitize] is applied before hiding the positions,
    and can be used to keep some unstable fields out of the error message. *)
val show_raise_async
  :  ?hide_positions:bool (** default is [false] *)
  -> ?sanitize:(Sexp.t -> Sexp.t) (** default is Fn.id *)
  -> ?show_backtrace:bool (** default is [false] *)
  -> (unit -> _ Deferred.t)
  -> unit Deferred.t

(** [require_does_not_raise_async] is like [require_does_not_raise], but for functions
    that produce a deferred result. *)
val require_does_not_raise_async
  :  ?cr:CR.t (** default is [CR] *)
  -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
  -> ?sanitize:(Sexp.t -> Sexp.t) (** default is Fn.id *)
  -> ?show_backtrace:bool (** default is [false] *)
  -> here:[%call_pos]
  -> (unit -> unit Deferred.t)
  -> unit Deferred.t

(** [require_does_raise_async] is like [require_does_raise], but for functions that
    produce a deferred result. *)
val require_does_raise_async
  :  ?cr:CR.t (** default is [CR] *)
  -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
  -> ?sanitize:(Sexp.t -> Sexp.t) (** default is Fn.id *)
  -> ?show_backtrace:bool (** default is [false] *)
  -> here:[%call_pos]
  -> (unit -> _ Deferred.t)
  -> unit Deferred.t

(** A log that goes to the test runner's controlling terminal, if it exists.

    This log's output is not captured by the expect test runner, and instead appears
    immediately on the attached terminal[1]. This is especially useful when debugging
    expect tests that time out.

    [1] : When tests are run by a build system, there may be no controlling terminal, in
    which case the output of this log is silently discarded. You would need to run
    [inline_test_runner] interactively to see output from this log. *)
val tty_log : Log.t Lazy.t

(** Removes connection details (hostnames and ports) from sexps containing the connection
    string "Client connected via TCP" followed by a host and port pair. *)
val remove_connection_details : Sexp.t -> Sexp.t

(** Sets [Async.Log.Global] output to one that elides log timestamps, time spans,
    backtraces, and connection strings with hostnames and ports. These settings help make
    output deterministic. Additionally runs [map_output], if provided, on each log line
    before it is emitted. *)
val with_robust_global_log_output
  :  ?map_output:(string -> string)
  -> (unit -> unit Deferred.t)
  -> unit Deferred.t

(** [with_sexp_round_floats] rounds floats when making sexp strings. The effect lasts
    until the deferred returned by the passed function completes, after which the previous
    behavior (full precision, by default) is restored. *)
val with_sexp_round_floats
  :  (unit -> 'a Deferred.t)
  -> significant_digits:int
  -> 'a Deferred.t
