open Cmdliner
open Result

let named wrapper = Term.(app (const wrapper))

let syntax =
  let parse s =
    match Mdx.Syntax.of_string s with
    | Some syntax -> `Ok syntax
    | None -> `Error (Format.sprintf "unrecognized syntax %S" s)
  in
  let syntax = (parse, Mdx.Syntax.pp) in
  let doc =
    "Which syntax to use. Either 'markdown' (also 'normal'), 'cram', or 'mli'."
  in
  named
    (fun x -> `Syntax x)
    Arg.(value & opt (some syntax) None & info [ "syntax" ] ~doc ~docv:"SYNTAX")

let file_docv = "FILE"

let file =
  let doc = "The file to use." in
  named
    (fun x -> `File x)
    Arg.(
      required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:file_docv)

let force_output =
  let doc = "Force generation of corrected file (even if there was no diff)" in
  named
    (fun x -> `Force_output x)
    Arg.(value & flag & info [ "force-output" ] ~doc)

type output = File of string | Stdout

let output_conv =
  let sparse, sprint = Arg.string in
  let parse s =
    match sparse s with
    | `Ok "-" -> Ok Stdout
    | `Ok s -> Ok (File s)
    | `Error msg -> Error (`Msg msg)
  in
  let print fmt = function
    | Stdout -> sprint fmt "-"
    | File s -> sprint fmt s
  in
  Arg.conv ~docv:"OUTPUT" (parse, print)

let output =
  let docv = "OUTPUT" in
  let doc =
    Printf.sprintf
      "Specify where to write the command output. $(docv) should be $(b,-) for \
       stdout or a filename. Defaults to $(i,%s).corrected. Note that setting \
       this option implies $(b,--force-output)."
      file_docv
  in
  named
    (fun x -> `Output x)
    Arg.(
      value & opt (some output_conv) None & info ~doc ~docv [ "o"; "output" ])

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup =
  named
    (fun x -> `Setup x)
    Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
