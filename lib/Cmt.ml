(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

open Migrate_ast

module T = struct
  type t = {txt: string; loc: Location.t}

  let loc t = t.loc

  let txt t = t.txt

  let create txt loc = {txt; loc}

  let compare =
    Comparable.lexicographic
      [ Comparable.lift String.compare ~f:txt
      ; Comparable.lift Location.compare ~f:loc ]

  let sexp_of_t {txt; loc} =
    Sexp.Atom (Format.asprintf "%s %a" txt Migrate_ast.Location.fmt loc)
end

include T
include Comparator.Make (T)

type error =
  { kind: [`Added of t | `Modified of t * t | `Dropped of t]
  ; cmt_kind: [`Comment | `Doc_comment] }

let pp_error fs {kind; cmt_kind} =
  let pp_cmt fs x =
    match cmt_kind with
    | `Doc_comment -> Format.fprintf fs "(** %s *)" (txt x)
    | `Comment -> Format.fprintf fs "(* %s *)" (txt x)
  in
  let s_kind =
    match cmt_kind with
    | `Doc_comment -> "doc-comment"
    | `Comment -> "comment"
  in
  match kind with
  | `Added x ->
      Format.fprintf fs "%!@{<loc>%a@}:@,@{<error>Error@}: %s %a added.\n%!"
        Location.print_loc (loc x) s_kind pp_cmt x
  | `Dropped x ->
      Format.fprintf fs
        "%!@{<loc>%a@}:@,@{<error>Error@}: %s %a dropped.\n%!"
        Location.print_loc (loc x) s_kind pp_cmt x
  | `Modified (x, y) -> (
      Format.fprintf fs
        "%!@{<loc>%a@}:@,\
         @{<error>Error@}: formatting of %s is unstable.\n\
        \  before: %a\n\
        \   after: %a\n\
         %!"
        Location.print_loc (loc x) s_kind pp_cmt x pp_cmt y ;
      match cmt_kind with
      | `Comment -> ()
      | `Doc_comment ->
          Format.fprintf fs
            "Please tighten up this comment in the source or disable the \
             formatting using the option --no-parse-docstrings.\n\
             %!" )

type pos = Before | Within | After

type decoded_kind =
  | Verbatim of string
  | Doc of string
  | Normal of string
  | Code of string
  | Asterisk_prefixed of string list

type decoded = {prefix: string; suffix: string; kind: decoded_kind}

(** [~content_offset] indicates at which column the body of the comment
    starts (1-indexed). [~max_idnent] indicates the maximum amount of
    indentation to trim. *)
let unindent_lines ?(max_indent = Stdlib.max_int) ~content_offset first_line
    tl_lines =
  let indent_of_line s =
    (* index of first non-whitespace is indentation, None means white line *)
    String.lfindi s ~f:(fun _ c -> not (Char.is_whitespace c))
  in
  (* The indentation of the first line must account for the location of the
     comment opening. Don't account for the first line if it's empty. *)
  let fl_spaces, fl_indent =
    match indent_of_line first_line with
    | Some i -> (i, i + content_offset - 1)
    | None -> (String.length first_line, Stdlib.max_int)
  in
  let fl_indent = min max_indent fl_indent in
  let min_indent =
    List.fold_left ~init:fl_indent
      ~f:(fun acc s ->
        match indent_of_line s with Some i -> min acc i | None -> acc )
      tl_lines
  in
  (* Completely trim the first line *)
  String.drop_prefix first_line fl_spaces
  :: List.map ~f:(fun s -> String.drop_prefix s min_indent) tl_lines

let unindent_lines ?max_indent ~content_offset txt =
  match String.split ~on:'\n' txt with
  | [] -> []
  | hd :: tl -> unindent_lines ?max_indent ~content_offset hd tl

let is_all_whitespace s = String.for_all s ~f:Char.is_whitespace

let split_asterisk_prefixed =
  let prefix = " *" in
  let drop_prefix s = String.drop_prefix s (String.length prefix) in
  let rec lines_are_asterisk_prefixed = function
    | [] -> true
    (* Allow the last line to be empty *)
    | [last] when is_all_whitespace last -> true
    | hd :: tl ->
        String.is_prefix hd ~prefix && lines_are_asterisk_prefixed tl
  in
  function
  (* Check whether the second line is not empty to avoid matching a comment
     with no asterisks. *)
  | fst_line :: (snd_line :: _ as tl)
    when lines_are_asterisk_prefixed tl && not (is_all_whitespace snd_line)
    ->
      Some (fst_line :: List.map tl ~f:drop_prefix)
  | _ -> None

let mk ?(prefix = "") ?(suffix = "") kind = {prefix; suffix; kind}

let decode ~parse_comments_as_doc {txt; loc} =
  let txt =
    (* Windows compatibility *)
    let f = function '\r' -> false | _ -> true in
    String.filter txt ~f
  in
  let opn_offset =
    let {Lexing.pos_cnum; pos_bol; _} = loc.Location.loc_start in
    pos_cnum - pos_bol + 1
  in
  if String.length txt >= 2 then
    match txt.[0] with
    | '$' when not (Char.is_whitespace txt.[1]) -> mk (Verbatim txt)
    | '$' ->
        let content_offset = opn_offset + 3 (* for opening + [$] *) in
        let dollar_suf = Char.equal txt.[String.length txt - 1] '$' in
        let suffix = if dollar_suf then "$" else "" in
        let source =
          let len = String.length txt - if dollar_suf then 2 else 1 in
          String.sub ~pos:1 ~len txt
        in
        let lines = unindent_lines ~content_offset source in
        let lines = List.map ~f:String.rstrip lines in
        let lines = List.drop_while ~f:is_all_whitespace lines in
        let code = String.concat ~sep:"\n" lines in
        mk ~prefix:"$" ~suffix (Code code)
    | '=' -> mk (Verbatim txt)
    | '*' -> mk ~prefix:"*" (Doc (String.drop_prefix txt 1))
    | _ when is_all_whitespace txt ->
        mk (Verbatim " ") (* Make sure not to format to [(**)]. *)
    | _ when parse_comments_as_doc -> mk (Doc txt)
    | _ -> (
        (* Indentation baseline is at the level of the opening to avoid
           indenting commented code. *)
        let lines =
          let content_offset = opn_offset + 2
          and max_indent = opn_offset - 1 in
          unindent_lines ~max_indent ~content_offset txt
        in
        (* Don't add a space to the prefix if the first line was only
           spaces. *)
        let prefix =
          if
            String.starts_with_whitespace txt
            && not (is_all_whitespace (List.hd_exn lines))
          then " "
          else ""
        in
        match split_asterisk_prefixed lines with
        | Some deprefixed_lines ->
            mk ~prefix (Asterisk_prefixed deprefixed_lines)
        | None -> mk ~prefix (Normal (String.concat ~sep:"\n" lines)) )
  else
    match txt with
    (* "(**)" is not parsed as a docstring but as a regular comment
       containing '*' and would be rewritten as "(***)" *)
    | "*" when Location.width loc = 4 -> mk (Verbatim "")
    | ("*" | "$") as txt -> mk (Verbatim txt)
    | "\n" | " " -> mk (Verbatim " ")
    | _ -> mk (Normal txt)
