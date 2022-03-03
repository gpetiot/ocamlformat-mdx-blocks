open Cmdliner.Term

val named : ('a -> 'b) -> 'a t -> 'b t
val syntax : [> `Syntax of Mdx.syntax option ] t
val file : [> `File of string ] t
val force_output : [> `Force_output of bool ] t

type output = File of string | Stdout

val output : [> `Output of output option ] t
(** A --output option to overwrite the command output.
    One can pass it ["-"] to set it to stdout which should imply [force_output].
    [default_doc] is used to describe the default value in the command's
    manpage *)

val setup : [> `Setup of unit ] t
