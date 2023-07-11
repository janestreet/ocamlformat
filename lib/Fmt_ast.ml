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

(** Format OCaml Ast *)

open Migrate_ast
open Extended_ast
open Asttypes
open Ast
open Fmt

type c =
  { conf: Conf.t
  ; debug: bool
  ; source: Source.t
  ; cmts: Cmts.t
  ; fmt_code: Fmt_odoc.fmt_code }

module Cmts = struct
  include Cmts

  let fmt_before c = fmt_before c.cmts c.conf ~fmt_code:c.fmt_code

  let fmt_within c = fmt_within c.cmts c.conf ~fmt_code:c.fmt_code

  let fmt_after c = fmt_after c.cmts c.conf ~fmt_code:c.fmt_code

  let fmt c ?pro ?epi ?eol ?adj loc =
    (* remove the before comments from the map first *)
    let before = fmt_before c ?pro ?epi ?eol ?adj loc in
    (* remove the within comments from the map by accepting the
       continuation *)
    fun inner ->
      (* delay the after comments until the within comments have been
         removed *)
      let after = fmt_after c ?pro ?epi loc in
      let open Fmt in
      before $ inner $ after

  module Toplevel = struct
    let fmt_before c = Toplevel.fmt_before c.cmts c.conf ~fmt_code:c.fmt_code

    let fmt_after c = Toplevel.fmt_after c.cmts c.conf ~fmt_code:c.fmt_code
  end
end

let cmt_checker {cmts; _} =
  { cmts_before= Cmts.has_before cmts
  ; cmts_within= Cmts.has_within cmts
  ; cmts_after= Cmts.has_after cmts }

let break_between c = Ast.break_between c.source (cmt_checker c)

type block =
  { opn: Fmt.t option
  ; pro: Fmt.t option
  ; psp: Fmt.t
  ; bdy: Fmt.t
  ; cls: Fmt.t
  ; esp: Fmt.t
  ; epi: Fmt.t option }

let empty =
  { opn= None
  ; pro= None
  ; psp= noop
  ; bdy= noop
  ; cls= noop
  ; esp= noop
  ; epi= None }

let blk_box ?(box = true) blk k =
  match blk.opn with Some opn -> wrap_if_k box opn blk.cls k | None -> k

let compose_module' ?box ?pro ?epi ({psp; bdy; esp; _} as blk) =
  ( blk_box ?box blk (fmt_opt pro $ (fmt_opt blk.pro $ psp $ bdy)) $ esp
  , fmt_opt blk.epi $ fmt_opt epi )

let compose_module ?box ?pro ?epi blk ~f =
  let bdy, epi' = compose_module' ?box ?pro blk in
  f (bdy $ epi') $ fmt_opt epi

(* Debug: catch and report failures at nearest enclosing Ast.t *)

let protect =
  let first = ref true in
  fun c ast pp ->
    Fmt.protect pp ~on_error:(fun exc ->
        if !first && c.debug then (
          let bt = Stdlib.Printexc.get_backtrace () in
          Stdlib.Format.eprintf "@\nFAIL@\n%a@\n%s@.%!" Ast.dump ast bt ;
          first := false ) ;
        raise exc )

let update_config ?quiet c l =
  {c with conf= List.fold ~init:c.conf l ~f:(Conf.update ?quiet)}

(* Preserve the position of comments located after the last element of a
   list/array (after `;`), otherwise comments are picked up by
   `fmt_expression` and printed before `;`. *)
let collection_last_cmt ?pro c (loc : Location.t) locs =
  let filter = function Parser.SEMI -> true | _ -> false in
  opt (List.last locs) (fun (last : Location.t) ->
      match
        Source.tokens_between c.source last.loc_end loc.loc_end ~filter
      with
      | [] -> noop
      | (_, semicolon_loc) :: _ ->
          Cmts.fmt_after ?pro c last ~filter:(fun cmt ->
              Location.compare (Cmt.loc cmt) semicolon_loc >= 0 ) )

let fmt_elements_collection ?pro ?(first_sep = true) ?(last_sep = true) c
    (p : Params.elements_collection) f loc fmt_x xs =
  let fmt_one ~first ~last x =
    fmt_if_k (not (first && first_sep)) p.sep_before
    $ fmt_x x
    $ fmt_or_k (last && last_sep) p.sep_after_final p.sep_after_non_final
  in
  list_fl xs fmt_one $ collection_last_cmt ?pro c loc (List.map ~f xs)

let fmt_expressions c width sub_exp exprs fmt_expr p loc =
  match c.conf.fmt_opts.break_collection_expressions.v with
  | `Fit_or_vertical ->
      fmt_elements_collection c p Exp.location loc fmt_expr exprs
  | `Wrap ->
      let is_simple x = is_simple c.conf width (sub_exp x) in
      let break x1 x2 = not (is_simple x1 && is_simple x2) in
      let grps = List.group exprs ~break in
      let fmt_grp ~first:first_grp ~last:last_grp exprs =
        fmt_elements_collection c ~first_sep:first_grp ~last_sep:last_grp p
          Exp.location loc fmt_expr exprs
      in
      list_fl grps fmt_grp

(** Handle the `break-fun-decl` option *)
let wrap_fun_decl_args c k =
  match c.conf.fmt_opts.break_fun_decl.v with
  | `Wrap | `Fit_or_vertical -> k
  | `Smart -> hvbox 0 k

let box_fun_decl_args c =
  match c.conf.fmt_opts.break_fun_decl.v with
  | `Fit_or_vertical -> hvbox
  | `Wrap | `Smart -> hovbox

(** Handle the `break-fun-sig` option *)
let box_fun_sig_args c =
  match c.conf.fmt_opts.break_fun_sig.v with
  | _ when c.conf.fmt_opts.ocp_indent_compat.v -> hvbox
  | `Fit_or_vertical -> hvbox
  | `Wrap | `Smart -> hovbox

let closing_paren ?force ?(offset = 0) c =
  match c.conf.fmt_opts.indicate_multiline_delimiters.v with
  | `No -> str ")"
  | `Space -> fits_breaks ")" " )" ?force
  | `Closing_on_separate_line -> fits_breaks ")" ")" ~hint:(1000, offset)

let maybe_disabled_k c (loc : Location.t) (l : attributes) f k =
  if not c.conf.opr_opts.disable.v then f c
  else
    let loc = Source.extend_loc_to_include_attributes loc l in
    Cmts.drop_inside c.cmts loc ;
    let s = Source.string_at c.source loc in
    k (Cmts.fmt c loc (str s))

let maybe_disabled c loc l f = maybe_disabled_k c loc l f Fn.id

let update_config_maybe_disabled c loc l f =
  let c = update_config c l in
  maybe_disabled c loc l f

let update_config_maybe_disabled_block c loc l f =
  let fmt bdy = {empty with opn= Some (open_vbox 2); bdy; cls= close_box} in
  let c = update_config c l in
  maybe_disabled_k c loc l f fmt

let update_items_config c items update_config =
  let with_config c i =
    let c = update_config c i in
    (c, (i, c))
  in
  let _, items = List.fold_map items ~init:c ~f:with_config in
  items

let check_include_functor_attr attrs =
  match
    List.partition_tf attrs ~f:(fun attr ->
        String.equal attr.attr_name.txt "extension.include_functor" )
  with
  | [], _ -> (attrs, false)
  | _ :: _, rest -> (rest, true)

let box_semisemi c ~parent_ctx b k =
  let space = Poly.(c.conf.fmt_opts.sequence_style.v = `Separator) in
  match parent_ctx with
  | _ when not b -> k
  | Rep -> k $ fmt_if space " " $ str ";;"
  | _ -> hvbox 0 (k $ fmt_or space "@;" "@," $ str ";;")

let fmt_hole () = str "_"

let fmt_item_list c ctx update_config ast fmt_item items =
  let items = update_items_config c items update_config in
  let break_struct = c.conf.fmt_opts.break_struct.v || is_top ctx in
  hvbox 0 @@ list_pn items
  @@ fun ~prev (itm, c) ~next ->
  let loc = Ast.location (ast itm) in
  maybe_disabled c loc [] (fun c -> fmt_item c ctx ~prev ~next itm)
  $ opt next (fun (i_n, c_n) ->
        fmt_or_k
          (break_between c (ast itm, c.conf) (ast i_n, c_n.conf))
          (fmt "\n@;<1000 0>")
          (fmt_or break_struct "@;<1000 0>" "@ ") )

let fmt_recmodule c ctx items fmt_item ast sub =
  let update_config c i = update_config c (Ast.attributes (ast i)) in
  let fmt_item c ctx ~prev ~next:_ i =
    fmt_item c ~rec_flag:true ~first:(Option.is_none prev) (sub ~ctx i)
  in
  fmt_item_list c ctx update_config ast fmt_item items

(* In several places, naked newlines (i.e. not "@\n") are used to avoid
   trailing space in open lines. *)
(* In several places, a break such as "@;<1000 0>" is used to force the
   enclosing box to break across multiple lines. *)

let rec fmt_longident (li : Longident.t) =
  let fmt_id id =
    wrap_if (Std_longident.String_id.is_symbol id) "( " " )" (str id)
  in
  match li with
  | Lident id -> fmt_id id
  | Ldot (li, id) -> hvbox 0 (fmt_longident li $ fmt "@,." $ fmt_id id)
  | Lapply (li1, li2) ->
      hvbox 2 (fmt_longident li1 $ wrap "@,(" ")" (fmt_longident li2))

let fmt_longident_loc c ?pre {txt; loc} =
  Cmts.fmt c loc (opt pre str $ fmt_longident txt)

let str_longident x =
  Format_.asprintf "%a" (fun fs x -> eval fs (fmt_longident x)) x

let fmt_str_loc c ?pre {txt; loc} = Cmts.fmt c loc (opt pre str $ str txt)

let fmt_str_loc_opt c ?pre ?(default = "_") {txt; loc} =
  Cmts.fmt c loc (opt pre str $ str (Option.value ~default txt))

let variant_var c ({txt= x; loc} : variant_var) =
  Cmts.fmt c loc @@ (str "`" $ fmt_str_loc c x)

let fmt_constant c ?epi {pconst_desc; pconst_loc= loc} =
  Cmts.fmt c loc
  @@
  match pconst_desc with
  | Pconst_integer (lit, suf) | Pconst_float (lit, suf) ->
      str lit $ opt suf char
  | Pconst_char (_, s) -> wrap "'" "'" @@ str s
  | Pconst_string (s, loc', Some delim) ->
      Cmts.fmt c loc'
      @@ wrap_k (str ("{" ^ delim ^ "|")) (str ("|" ^ delim ^ "}")) (str s)
  | Pconst_string (_, loc', None) -> (
      let delim = ["@,"; "@;"] in
      let contains_pp_commands s =
        let is_substring substring = String.is_substring s ~substring in
        List.exists delim ~f:is_substring
      in
      let fmt_string_auto ~break_on_newlines s =
        let fmt_words ~epi s =
          let words = String.split s ~on:' ' in
          let fmt_word ~prev:_ curr ~next =
            match next with
            | Some "" -> str curr $ str " "
            | Some _ ->
                str curr $ cbreak ~fits:("", 1, "") ~breaks:(" \\", 0, "")
            | None -> str curr
          in
          hovbox_if (List.length words > 1) 0 (list_pn words fmt_word $ epi)
        in
        let fmt_line ~epi ~prev:_ curr ~next =
          let not_suffix suffix = not (String.is_suffix curr ~suffix) in
          let print_ln =
            List.for_all delim ~f:not_suffix || not break_on_newlines
          in
          let fmt_next next =
            if String.is_empty next then fmt_if_k print_ln (str "\\n")
            else if Char.equal next.[0] ' ' then
              fmt_if_k print_ln (str "\\n")
              $ cbreak ~fits:("", 0, "") ~breaks:("\\", -1, "\\")
            else
              fmt_if_k print_ln (str "\\n")
              $ cbreak ~fits:("", 0, "") ~breaks:("\\", 0, "")
          in
          let epi = match next with Some _ -> noop | None -> epi in
          fmt_words ~epi curr $ opt next fmt_next
        in
        let lines = String.split ~on:'\n' s in
        let lines =
          if break_on_newlines then lines
          else
            let n_lines = List.length lines in
            (* linebreaks are merged with the preceding line when possible
               instead of having a blank line in the list *)
            List.foldi lines ~init:[] ~f:(fun i acc -> function
              | "" when i < n_lines - 1 -> (
                match acc with [] -> [""] | h :: t -> (h ^ "\\n") :: t )
              | line -> line :: acc )
            |> List.rev
        in
        let epi = str "\"" $ fmt_opt epi in
        hvbox 1 (str "\"" $ list_pn lines (fmt_line ~epi))
      in
      let preserve_or_normalize =
        match c.conf.fmt_opts.break_string_literals.v with
        | `Never -> `Preserve
        | `Auto -> `Normalize
      in
      let s = Source.string_literal c.source preserve_or_normalize loc in
      Cmts.fmt c loc'
      @@
      match c.conf.fmt_opts.break_string_literals.v with
      | `Auto when contains_pp_commands s ->
          let break_on_pp_commands in_ pattern =
            String.substr_replace_all in_ ~pattern ~with_:(pattern ^ "\n")
          in
          List.fold_left delim ~init:s ~f:break_on_pp_commands
          |> fmt_string_auto ~break_on_newlines:true
      | `Auto -> fmt_string_auto ~break_on_newlines:false s
      | `Never -> wrap "\"" "\"" (str s) )

let fmt_variance_injectivity c vc = hvbox 0 (list vc "" (fmt_str_loc c))

let fmt_label lbl sep =
  (* No comment can be attached here. *)
  match lbl with
  | Nolabel -> noop
  | Labelled l -> str "~" $ str l.txt $ fmt sep
  | Optional l -> str "?" $ str l.txt $ fmt sep

let fmt_direction_flag = function
  | Upto -> fmt "@ to "
  | Downto -> fmt "@ downto "

let fmt_private ?(pro = fmt "@ ") c loc =
  pro $ hvbox 0 @@ Cmts.fmt c loc @@ str "private"

let fmt_virtual ?(pro = fmt "@ ") c loc =
  pro $ hvbox 0 @@ Cmts.fmt c loc @@ str "virtual"

let fmt_mutable ?(pro = fmt "@ ") ?(epi = noop) c loc =
  pro $ hvbox 0 (Cmts.fmt c loc (str "mutable")) $ epi

let fmt_private_flag c = function
  | Private loc -> fmt_private c loc
  | Public -> noop

let fmt_virtual_flag c = function
  | Virtual loc -> fmt_virtual c loc
  | Concrete -> noop

let fmt_mutable_flag ?pro ?epi c = function
  | Mutable loc -> fmt_mutable ?pro ?epi c loc
  | Immutable -> noop

let fmt_mutable_virtual_flag c = function
  | {mv_mut= Some m; mv_virt= Some v} when Location.compare_start v m < 1 ->
      fmt_virtual c v $ fmt_mutable c m
  | {mv_mut; mv_virt} ->
      opt mv_mut (fmt_mutable c) $ opt mv_virt (fmt_virtual c)

let fmt_private_virtual_flag c = function
  | {pv_priv= Some p; pv_virt= Some v} when Location.compare_start v p < 1 ->
      fmt_virtual c v $ fmt_private c p
  | {pv_priv; pv_virt} ->
      opt pv_priv (fmt_private c) $ opt pv_virt (fmt_virtual c)

let virtual_or_override = function
  | Cfk_virtual _ -> noop
  | Cfk_concrete (Override, _) -> str "!"
  | Cfk_concrete (Fresh, _) -> noop

let fmt_parsed_docstring c ~loc ?pro ~epi input parsed =
  assert (not (String.is_empty input)) ;
  let offset =
    let pos = loc.Location.loc_start in
    pos.pos_cnum - pos.pos_bol + 3
  and fmt_code = c.fmt_code in
  let doc = Fmt_odoc.fmt_parsed c.conf ~fmt_code ~offset ~input parsed in
  Cmts.fmt c loc
  @@ vbox_if (Option.is_none pro) 0 (fmt_opt pro $ wrap "(**" "*)" doc $ epi)

let docstring_epi ~standalone ~next ~epi ~floating =
  let epi = if Option.is_some next then fmt "@\n" else fmt_opt epi in
  match next with
  | (None | Some (_, false)) when floating && not standalone ->
      str "\n" $ epi
  | _ -> epi

let fmt_docstring c ?(standalone = false) ?pro ?epi doc =
  list_pn (Option.value ~default:[] doc)
    (fun ~prev:_ ({txt; loc}, floating) ~next ->
      let epi = docstring_epi ~standalone ~next ~epi ~floating in
      fmt_parsed_docstring c ~loc ?pro ~epi txt (Docstring.parse ~loc txt) )

let fmt_docstring_around_item' ?(is_val = false) ?(force_before = false)
    ?(fit = false) c doc1 doc2 =
  match (doc1, doc2) with
  | Some _, Some _ ->
      ( fmt_docstring c ~epi:(fmt "@\n") doc1
      , fmt_docstring c ~pro:(fmt "@\n") doc2 )
  | None, None -> (noop, noop)
  | None, Some doc | Some doc, None -> (
      let is_tag_only =
        List.for_all ~f:(function
          | Ok es, _ -> Docstring.is_tag_only es
          | _ -> false )
      in
      let fmt_doc ?epi ?pro doc =
        list_pn doc (fun ~prev:_ (parsed, ({txt; loc}, floating)) ~next ->
            let next = Option.map next ~f:snd in
            let epi = docstring_epi ~standalone:false ~next ~epi ~floating in
            fmt_parsed_docstring c ~loc ~epi ?pro txt parsed )
      in
      let floating_doc, doc =
        doc
        |> List.map ~f:(fun (({txt; loc}, _) as doc) ->
               (Docstring.parse ~loc txt, doc) )
        |> List.partition_tf ~f:(fun (_, (_, floating)) -> floating)
      in
      let placement =
        if force_before then `Before
        else if
          Poly.( = ) c.conf.fmt_opts.doc_comments_tag_only.v `Fit
          && fit && is_tag_only doc
        then `Fit
        else
          let ((`Before | `After) as conf) =
            match c.conf.fmt_opts.doc_comments.v with
            | `After_when_possible -> `After
            | `Before_except_val when is_val -> `After
            | `Before_except_val -> `Before
            | `Before -> `Before
          in
          conf
      in
      let floating_doc = fmt_doc ~epi:(fmt "@\n") floating_doc in
      match placement with
      | `Before -> (floating_doc $ fmt_doc ~epi:(fmt "@\n") doc, noop)
      | `After -> (floating_doc, fmt_doc ~pro:(fmt "@\n") doc)
      | `Fit ->
          ( floating_doc
          , fmt_doc ~pro:(break c.conf.fmt_opts.doc_comments_padding.v 0) doc
          ) )

(** Formats docstrings and decides where to place them Handles the
    [doc-comments] and [doc-comment-tag-only] options Returns the tuple
    [doc_before, doc_after, attrs] *)
let fmt_docstring_around_item ?is_val ?force_before ?fit c attrs =
  let doc1, attrs = doc_atrs attrs in
  let doc2, attrs = doc_atrs attrs in
  let doc_before, doc_after =
    fmt_docstring_around_item' ?is_val ?force_before ?fit c doc1 doc2
  in
  (doc_before, doc_after, attrs)

let fmt_extension_suffix c ext =
  opt ext (fun name -> str "%" $ fmt_str_loc c name)

let is_arrow_or_poly = function
  | {ptyp_desc= Ptyp_arrow _ | Ptyp_poly _; _} -> true
  | _ -> false

let fmt_assign_arrow c =
  match c.conf.fmt_opts.assignment_operator.v with
  | `Begin_line ->
      break 1 (Params.Indent.assignment_operator_bol c.conf) $ fmt "<- "
  | `End_line -> fmt " <-@;<1 2>"

let arrow_sep c ~parens : Fmt.s =
  match c.conf.fmt_opts.break_separators.v with
  | `Before -> if parens then "@;<1 1>-> " else "@ -> "
  | `After -> " ->@;<1 0>"

let fmt_docstring_padded c doc =
  fmt_docstring c ~pro:(break c.conf.fmt_opts.doc_comments_padding.v 0) doc

let sequence_blank_line c (l1 : Location.t) (l2 : Location.t) =
  match c.conf.fmt_opts.sequence_blank_line.v with
  | `Preserve_one ->
      let rec loop prev_pos = function
        | cmt :: tl ->
            let loc = Cmt.loc cmt in
            (* Check empty line before each comment *)
            Source.empty_line_between c.source prev_pos loc.loc_start
            || loop loc.loc_end tl
        | [] ->
            (* Check empty line after all comments *)
            Source.empty_line_between c.source prev_pos l2.loc_start
      in
      loop l1.loc_end (Cmts.remaining_before c.cmts l2)
  | `Compact -> false

let fmt_quoted_string key ext s = function
  | None ->
      wrap_k (str (Format_.sprintf "{%s%s|" key ext)) (str "|}") (str s)
  | Some delim ->
      let ext_and_delim =
        if String.is_empty delim then ext
        else Format_.sprintf "%s %s" ext delim
      in
      wrap_k
        (str (Format_.sprintf "{%s%s|" key ext_and_delim))
        (str (Format_.sprintf "|%s}" delim))
        (str s)

let fmt_type_var s =
  str "'"
  (* [' a'] is a valid type variable, the space is required to not lex as a
     char. https://github.com/ocaml/ocaml/pull/2034 *)
  $ fmt_if (String.length s > 1 && Char.equal s.[1] '\'') " "
  $ str s

let split_global_flags_from_attrs atrs =
  match
    List.partition_map atrs ~f:(fun a ->
        match a.attr_name.txt with
        | "extension.global" -> First `Global
        | _ -> Second a )
  with
  | [`Global], atrs -> (true, atrs)
  | _ -> (false, atrs)

let is_layout attr =
  match attr.attr_name.txt with
  | "any" -> true
  | "value" -> true
  | "void" -> true
  | "immediate" -> true
  | "immediate64" -> true
  | _ -> false

let rec fmt_extension_aux c ctx ~key (ext, pld) =
  match (ext.txt, pld, ctx) with
  (* Quoted extensions (since ocaml 4.11). *)
  | ( ext
    , PStr
        [ { pstr_desc=
              Pstr_eval
                ( { pexp_desc=
                      Pexp_constant
                        {pconst_desc= Pconst_string (str, loc, delim); _}
                  ; pexp_loc
                  ; pexp_loc_stack= _
                  ; pexp_attributes= [] }
                , [] )
          ; pstr_loc } ]
    , _ )
    when Source.is_quoted_string c.source pstr_loc ->
      (* Comments and attributes are not allowed by the parser *)
      assert (not (Cmts.has_before c.cmts loc)) ;
      assert (not (Cmts.has_after c.cmts loc)) ;
      assert (not (Cmts.has_before c.cmts pexp_loc)) ;
      assert (not (Cmts.has_after c.cmts pexp_loc)) ;
      assert (not (Cmts.has_before c.cmts pstr_loc)) ;
      assert (not (Cmts.has_after c.cmts pstr_loc)) ;
      hvbox 0 (fmt_quoted_string (Ext.Key.to_string key) ext str delim)
  | _, PStr [({pstr_loc; _} as si)], (Pld _ | Str _ | Top)
    when Source.extension_using_sugar ~name:ext ~payload:pstr_loc ->
      fmt_structure_item c ~last:true ~ext ~semisemi:false (sub_str ~ctx si)
  | _, PSig [({psig_loc; _} as si)], (Pld _ | Sig _ | Top)
    when Source.extension_using_sugar ~name:ext ~payload:psig_loc ->
      fmt_signature_item c ~ext (sub_sig ~ctx si)
  | _, PPat (({ppat_loc; _} as pat), _), (Pld _ | Top)
    when Source.extension_using_sugar ~name:ext ~payload:ppat_loc ->
      fmt_pattern c ~ext (sub_pat ~ctx pat)
  | _ ->
      let indent =
        match pld with
        | PStr [{pstr_desc= Pstr_eval _; _}] | PTyp _ | PPat _ ->
            c.conf.fmt_opts.extension_indent.v
        | PSig _ | PStr _ -> c.conf.fmt_opts.stritem_extension_indent.v
      in
      hvbox indent
        (wrap "[" "]"
           ( str (Ext.Key.to_string key)
           $ fmt_str_loc c ext
           $ fmt_payload c (Pld pld) pld
           $ fmt_if (Exposed.Right.payload pld) " " ) )

and fmt_extension = fmt_extension_aux ~key:Ext.Key.Regular

and fmt_item_extension = fmt_extension_aux ~key:Ext.Key.Item

and fmt_attribute c ~key {attr_name; attr_payload; attr_loc} =
  hvbox 0 @@ Cmts.fmt c attr_loc
  @@
  match (attr_name, attr_payload) with
  | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc= {loc_ghost= true; _}}
    , PStr
        [ { pstr_desc=
              Pstr_eval
                ( { pexp_desc=
                      Pexp_constant
                        {pconst_desc= Pconst_string (doc, _, None); _}
                  ; pexp_attributes= []
                  ; _ }
                , [] )
          ; _ } ] ) ->
      fmt_or (String.equal txt "ocaml.text") "@ " " "
      $ wrap "(**" "*)" (str doc)
  | name, pld ->
      let indent =
        match (pld, key) with
        | (PStr _ | PSig _), Attr.Key.Floating ->
            c.conf.fmt_opts.stritem_extension_indent.v
        | _ -> c.conf.fmt_opts.extension_indent.v
      in
      hvbox indent
        (wrap "[" "]"
           ( str (Attr.Key.to_string key)
           $ fmt_str_loc c name
           $ fmt_payload c (Pld pld) pld
           $ fmt_if (Exposed.Right.payload pld) " " ) )

and fmt_attributes_aux c ?pre ?suf ~key attrs =
  let num = List.length attrs in
  fmt_if_k (num > 0)
    ( opt pre sp
    $ hvbox_if (num > 1) 0
        (hvbox 0 (list attrs "@ " (fmt_attribute c ~key)) $ opt suf str) )

and fmt_attributes = fmt_attributes_aux ~key:Attr.Key.Regular

and fmt_item_attributes = fmt_attributes_aux ~key:Attr.Key.Item

and fmt_attributes_and_docstrings_aux c ~key attrs =
  let standalone, pro, space =
    match key with
    | Attr.Key.Regular | Attr.Key.Item ->
        (false, break c.conf.fmt_opts.doc_comments_padding.v 0, break 1 0)
    | Attr.Key.Floating -> (true, noop, noop)
  in
  let aux = function
    | { attr_name=
          {txt= "ocaml.doc" | "ocaml.text"; loc= {loc_ghost= true; _}}
      ; attr_payload=
          PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc=
                          Pexp_constant
                            {pconst_desc= Pconst_string (txt, _, None); _}
                      ; pexp_loc= loc
                      ; pexp_attributes= []
                      ; _ }
                    , [] )
              ; _ } ]
      ; _ } ->
        fmt_docstring c ~standalone ~pro (Some [({txt; loc}, standalone)])
    | attr -> space $ fmt_attribute c ~key attr
  in
  list attrs "" aux

and fmt_attributes_and_docstrings =
  fmt_attributes_and_docstrings_aux ~key:Attr.Key.Regular

and fmt_floating_attributes_and_docstrings =
  fmt_attributes_and_docstrings_aux ~key:Attr.Key.Floating

and fmt_payload c ctx pld =
  protect c (Pld pld)
  @@
  match pld with
  | PStr mex ->
      fmt_if (not (List.is_empty mex)) "@ " $ fmt_structure c ctx mex
  | PSig mty ->
      fmt ":@ "
      (* A trailing space is necessary because [:]] is the immutable array
         closing delimiter*)
      $ fmt_signature c ctx mty
  | PTyp typ -> fmt ":@ " $ fmt_core_type c (sub_typ ~ctx typ)
  | PPat (pat, exp) ->
      let fmt_when exp =
        str " when " $ fmt_expression c (sub_exp ~ctx exp)
      in
      fmt "?@ " $ fmt_pattern c (sub_pat ~ctx pat) $ opt exp fmt_when

and fmt_record_field c ?typ1 ?typ2 ?rhs lid1 =
  let field_space =
    match c.conf.fmt_opts.field_space.v with
    | `Loose | `Tight_decl -> str " "
    | `Tight -> noop
  in
  let t1 = Option.map typ1 ~f:(fun x -> fmt ": " $ fmt_core_type c x) in
  let t2 = Option.map typ2 ~f:(fun x -> fmt ":> " $ fmt_core_type c x) in
  let r = Option.map rhs ~f:(fun x -> fmt "=@;<1 2>" $ cbox 0 x) in
  let fmt_type_rhs =
    match List.filter_opt [t1; t2; r] with
    | [] -> noop
    | l -> field_space $ list l "@ " Fn.id
  in
  Cmts.fmt_before c lid1.loc
  $ cbox 0
      (fmt_longident_loc c lid1 $ Cmts.fmt_after c lid1.loc $ fmt_type_rhs)

and fmt_type_cstr c ?constraint_ctx xtyp =
  let colon_before = Poly.(c.conf.fmt_opts.break_colon.v = `Before) in
  fmt_or_k colon_before (fits_breaks " " ~hint:(1000, 0) "") (fmt "@;<0 -1>")
  $ cbox_if colon_before 0
      (fmt_core_type c ~pro:":" ?constraint_ctx ~pro_space:(not colon_before)
         ~box:(not colon_before) xtyp )

and type_constr_and_body c xbody =
  let body = xbody.ast in
  match xbody.ast.pexp_desc with
  | Pexp_constraint (exp, typ) ->
      Cmts.relocate c.cmts ~src:body.pexp_loc ~before:exp.pexp_loc
        ~after:exp.pexp_loc ;
      let typ_ctx = Exp body in
      let exp_ctx =
        Exp Ast_helper.(Exp.fun_ Nolabel None (Pat.any ()) exp)
      in
      ( Some (fmt_type_cstr c ~constraint_ctx:`Fun (sub_typ ~ctx:typ_ctx typ))
      , sub_exp ~ctx:exp_ctx exp )
  | _ -> (None, xbody)

and fmt_arrow_param c ctx
    ({pap_label= lI; pap_loc= locI; pap_type= tI}, localI) =
  let arg_label lbl =
    match lbl with
    | Nolabel -> if localI then Some (str "local_ ") else None
    | Labelled l -> Some (str l.txt $ fmt ":@," $ fmt_if localI "local_ ")
    | Optional l ->
        Some (str "?" $ str l.txt $ fmt ":@," $ fmt_if localI "local_ ")
  in
  let xtI = sub_typ ~ctx tI in
  let arg =
    match arg_label lI with
    | None -> fmt_core_type c xtI
    | Some f -> hovbox 2 (f $ fmt_core_type c xtI)
  in
  hvbox 0 (Cmts.fmt_before c locI $ arg)

(** Format [Ptyp_arrow]. [indent] can be used to override the indentation
    added for the break-separators option. [parent_has_parens] is used to
    align arrows to parentheses. *)
and fmt_arrow_type c ~ctx ?indent ~parens ~parent_has_parens args fmt_ret_typ
    =
  let indent =
    match indent with
    | Some k -> k
    | None ->
        fmt_if_k
          Poly.(c.conf.fmt_opts.break_separators.v = `Before)
          (fmt_or_k c.conf.fmt_opts.ocp_indent_compat.v (fits_breaks "" "")
             (fits_breaks "" "   ") )
  and ret_typ =
    match fmt_ret_typ with
    | Some k -> fmt (arrow_sep c ~parens:parent_has_parens) $ k
    | None -> noop
  in
  indent
  $ wrap_if parens "(" ")"
      ( list args
          (arrow_sep c ~parens:parent_has_parens)
          (fmt_arrow_param c ctx)
      $ ret_typ )

(* The context of [xtyp] refers to the RHS of the expression (namely
   Pexp_constraint) and does not give a relevant information as to whether
   [xtyp] should be parenthesized. [constraint_ctx] gives the higher context
   of the expression, i.e. if the expression is part of a `fun`
   expression. *)
(* CR layouts: Instead of having a [tydecl_param] argument here, the right
   thing would be for [xtyp] to provide enough information to determine
   whether we are printing a type parameter in a typedecl. But it doesn't,
   and that change would be a much bigger diff and make rebasing on upstream
   harder in the future. When layouts are upstreamed and upstream ocamlformat
   gets support for them, we should remove tydecl_param and go with whatever
   their solution is. *)
and fmt_core_type c ?(box = true) ?pro ?(pro_space = true) ?constraint_ctx
    ?(tydecl_param = false) ({ast= typ; ctx} as xtyp) =
  protect c (Typ typ)
  @@
  let {ptyp_desc; ptyp_attributes; ptyp_loc; _} = typ in
  let ptyp_attributes =
    List.filter ptyp_attributes ~f:(fun a ->
        not (String.equal a.attr_name.txt "extension.curry") )
  in
  let ptyp_attributes =
    List.filter ptyp_attributes ~f:(fun a ->
        not (String.equal a.attr_name.txt "extension.global") )
  in
  update_config_maybe_disabled c ptyp_loc ptyp_attributes
  @@ fun c ->
  ( match pro with
  | Some pro -> (
    match c.conf.fmt_opts.break_colon.v with
    | `Before -> fmt_if pro_space "@;" $ str pro $ str " "
    | `After -> fmt_if pro_space " " $ str pro $ fmt "@ " )
  | None -> noop )
  $
  let doc, atrs = doc_atrs ptyp_attributes in
  Cmts.fmt c ptyp_loc
  @@ (fun k -> k $ fmt_docstring c ~pro:(fmt "@ ") doc)
  @@ ( match atrs with
     | [] -> Fn.id
     | [attr] when is_layout attr ->
         Fn.id
         (* CR layouts v1.5: layout annotations on type params are printed by
            the type parameter printer. Revisit when we have support for
            pretty layout annotations in more places. *)
     | _ ->
         fun k ->
           hvbox 0
             (Params.parens_if (not tydecl_param) c.conf
                (k $ fmt_attributes c ~pre:Cut atrs) ) )
  @@
  let parens = (not tydecl_param) && parenze_typ xtyp in
  hvbox_if box 0
  @@ Params.parens_if
       (match typ.ptyp_desc with Ptyp_tuple _ -> false | _ -> parens)
       c.conf
  @@
  let in_type_declaration =
    match ctx with
    | Td {ptype_manifest= Some t; _} -> phys_equal t typ
    | _ -> false
  in
  let ctx = Typ typ in
  let parenze_constraint_ctx =
    match constraint_ctx with
    | Some `Fun when not parens -> true
    | _ -> false
  in
  match ptyp_desc with
  | Ptyp_alias (typ, str) ->
      hvbox 0
        (wrap_if parenze_constraint_ctx "(" ")"
           ( fmt_core_type c (sub_typ ~ctx typ)
           $ fmt "@ as@ "
           $ Cmts.fmt c str.loc @@ fmt_type_var str.txt ) )
  | Ptyp_any -> str "_"
  | Ptyp_arrow (args, ret_typ) ->
      Cmts.relocate c.cmts ~src:ptyp_loc
        ~before:(List.hd_exn args).pap_type.ptyp_loc ~after:ret_typ.ptyp_loc ;
      let args, ret_typ, ctx = Sugar.decompose_arrow ctx args ret_typ in
      let indent =
        match pro with
        | Some pro when c.conf.fmt_opts.ocp_indent_compat.v ->
            let indent =
              if Poly.(c.conf.fmt_opts.break_separators.v = `Before) then 2
              else 0
            in
            Some
              (fits_breaks ""
                 (String.make (Int.max 1 (indent - String.length pro)) ' ') )
        | _ -> None
      in
      let fmt_ret_typ = fmt_arrow_param c ctx ret_typ in
      fmt_arrow_type c ~ctx ?indent ~parens:parenze_constraint_ctx
        ~parent_has_parens:parens args (Some fmt_ret_typ)
  | Ptyp_constr (lid, []) -> fmt_longident_loc c lid
  | Ptyp_constr (lid, [t1]) ->
      hvbox
        (Params.Indent.type_constr c.conf)
        ( fmt_core_type c (sub_typ ~ctx t1)
        $ fmt "@ " $ fmt_longident_loc c lid )
  | Ptyp_constr (lid, t1N) ->
      hvbox
        (Params.Indent.type_constr c.conf)
        ( wrap_fits_breaks c.conf "(" ")"
            (list t1N (Params.comma_sep c.conf)
               (sub_typ ~ctx >> fmt_core_type c) )
        $ fmt "@ " $ fmt_longident_loc c lid )
  | Ptyp_extension ext ->
      hvbox c.conf.fmt_opts.extension_indent.v (fmt_extension c ctx ext)
  | Ptyp_package (id, cnstrs) ->
      hvbox 2
        ( hovbox 0 (fmt "module@ " $ fmt_longident_loc c id)
        $ fmt_package_type c ctx cnstrs )
  | Ptyp_poly ([], _) ->
      impossible "produced by the parser, handled elsewhere"
  | Ptyp_poly (a1N, t) ->
      hovbox_if box 0
        ( list a1N "@ " (fun {txt; _} -> fmt_type_var txt)
        $ fmt ".@ "
        $ fmt_core_type c ~box:true (sub_typ ~ctx t) )
  | Ptyp_tuple typs ->
      hvbox 0
        (wrap_if parenze_constraint_ctx "(" ")"
           (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
              (list typs "@ * " (sub_typ ~ctx >> fmt_core_type c)) ) )
  | Ptyp_var s -> fmt_type_var s
  | Ptyp_variant (rfs, flag, lbls) ->
      let row_fields rfs =
        match rfs with
        | [] -> Cmts.fmt_within c ~pro:noop ptyp_loc
        | _ ->
            list rfs
              ( if
                  in_type_declaration
                  && Poly.(c.conf.fmt_opts.type_decl.v = `Sparse)
                then "@;<1000 0>| "
                else "@ | " )
              (fmt_row_field c ctx)
      in
      let protect_token = Exposed.Right.(list ~elt:row_field) rfs in
      let space_around = c.conf.fmt_opts.space_around_variants.v in
      let closing =
        let empty = List.is_empty rfs in
        let force =
          match c.conf.fmt_opts.type_decl.v with
          | `Sparse -> Option.some_if space_around Break
          | `Compact -> None
        in
        let nspaces = if empty then 0 else 1 in
        let space = (protect_token || space_around) && not empty in
        fits_breaks
          (if space && not empty then " ]" else "]")
          ~hint:(nspaces, 0) "]" ?force
      in
      hvbox 0
        ( match (flag, lbls, rfs) with
        | Closed, None, [{prf_desc= Rinherit _; _}] ->
            str "[ | " $ row_fields rfs $ closing
        | Closed, None, _ ->
            let opening = if space_around then "[ " else "[" in
            fits_breaks opening "[ " $ row_fields rfs $ closing
        | Open, None, _ -> str "[> " $ row_fields rfs $ closing
        | Closed, Some [], _ -> str "[< " $ row_fields rfs $ closing
        | Closed, Some ls, _ ->
            str "[< " $ row_fields rfs $ str " > "
            $ list ls "@ " (variant_var c)
            $ closing
        | Open, Some _, _ -> impossible "not produced by parser" )
  | Ptyp_object ([], closed_flag) ->
      wrap "<@ " ">"
        ( match closed_flag with
        | OClosed -> Cmts.fmt_within c ~pro:noop ~epi:(str " ") ptyp_loc
        | OOpen loc -> Cmts.fmt c loc (str "..") $ fmt "@ " )
  | Ptyp_object (fields, closed_flag) ->
      let fmt_field {pof_desc; pof_attributes; pof_loc} =
        let fmt_field =
          match pof_desc with
          | Otag (lab_loc, typ) ->
              (* label loc * attributes * core_type -> object_field *)
              let field_loose =
                match c.conf.fmt_opts.field_space.v with
                | `Loose | `Tight_decl -> true
                | `Tight -> false
              in
              fmt_str_loc c lab_loc $ fmt_if field_loose " " $ fmt ":@ "
              $ fmt_core_type c (sub_typ ~ctx typ)
          | Oinherit typ -> fmt_core_type c (sub_typ ~ctx typ)
        in
        Cmts.fmt c pof_loc
        @@ hvbox 4
             ( hvbox 2 fmt_field
             $ fmt_attributes_and_docstrings c pof_attributes )
      in
      hvbox 0
        (wrap "< " " >"
           ( list fields "@ ; " fmt_field
           $
           match closed_flag with
           | OClosed -> noop
           | OOpen loc -> fmt "@ ; " $ Cmts.fmt c loc @@ str ".." ) )
  | Ptyp_class (lid, []) -> fmt_longident_loc c ~pre:"#" lid
  | Ptyp_class (lid, [t1]) ->
      fmt_core_type c (sub_typ ~ctx t1)
      $ fmt "@ "
      $ fmt_longident_loc c ~pre:"#" lid
  | Ptyp_class (lid, t1N) ->
      wrap_fits_breaks c.conf "(" ")"
        (list t1N (Params.comma_sep c.conf)
           (sub_typ ~ctx >> fmt_core_type c) )
      $ fmt "@ "
      $ fmt_longident_loc c ~pre:"#" lid

and fmt_package_type c ctx cnstrs =
  let fmt_cstr ~first ~last:_ (lid, typ) =
    fmt_or first "@;<1 0>" "@;<1 1>"
    $ hvbox 2
        ( fmt_or first "with type " "and type "
        $ fmt_longident_loc c lid $ fmt " =@ "
        $ fmt_core_type c (sub_typ ~ctx typ) )
  in
  list_fl cnstrs fmt_cstr

and fmt_row_field c ctx {prf_desc; prf_attributes; prf_loc} =
  let c = update_config c prf_attributes in
  let row =
    match prf_desc with
    | Rtag (name, const, typs) ->
        variant_var c name
        $ fmt_if (not (const && List.is_empty typs)) " of@ "
        $ fmt_if (const && not (List.is_empty typs)) " & "
        $ list typs "@ & " (sub_typ ~ctx >> fmt_core_type c)
    | Rinherit typ -> fmt_core_type c (sub_typ ~ctx typ)
  in
  hvbox 0
    ( hvbox (Params.Indent.variant_type_arg c.conf) (Cmts.fmt c prf_loc row)
    $ fmt_attributes_and_docstrings c prf_attributes )

and fmt_pattern_attributes c xpat k =
  match xpat.ast.ppat_attributes with
  | [] -> k
  | attrs ->
      let parens_attr =
        match xpat.ast.ppat_desc with
        | Ppat_or _ -> (
          match xpat.ctx with
          | Pat {ppat_desc= Ppat_construct _; _}
           |Pat {ppat_desc= Ppat_variant _; _} ->
              true
          | _ -> false )
        | _ -> (
          match xpat.ctx with
          | Exp {pexp_desc= Pexp_object _; _}
           |Cl {pcl_desc= Pcl_structure _; _} ->
              false
          | _ -> true )
      in
      Params.parens_if parens_attr c.conf
        (k $ fmt_attributes c ~pre:Space attrs)

and maybe_fmt_pattern_extension ~ext c ~pro ~parens ~box ~ctx0 ~ctx ~ppat_loc
    pat fmt_normal_pattern =
  match Extensions.Pattern.of_ast pat with
  | Some epat ->
      fmt_pattern_extension ~ext c ~pro ~parens ~box ~ctx0 ~ctx ~ppat_loc
        epat
  | None -> fmt_normal_pattern ()

and fmt_pattern ?ext c ?pro ?parens ?(box = false)
    ({ctx= ctx0; ast= pat} as xpat) =
  protect c (Pat pat)
  @@
  let ctx = Pat pat in
  let {ppat_desc; ppat_attributes; ppat_loc; _} = pat in
  update_config_maybe_disabled c ppat_loc ppat_attributes
  @@ fun c ->
  let parens = match parens with Some b -> b | None -> parenze_pat xpat in
  (match ctx0 with Pat {ppat_desc= Ppat_tuple _; _} -> hvbox 0 | _ -> Fn.id)
  @@ ( match ppat_desc with
     | Ppat_or _ -> fun k -> Cmts.fmt c ppat_loc @@ k
     | _ -> fun k -> Cmts.fmt c ppat_loc @@ (fmt_opt pro $ k) )
  @@ hovbox_if box 0
  @@ fmt_pattern_attributes c xpat
  @@ maybe_fmt_pattern_extension ~ext c ~pro ~parens ~box ~ctx0 ~ctx
       ~ppat_loc pat
  @@ fun () ->
  match ppat_desc with
  | Ppat_any -> str "_"
  | Ppat_var {txt; loc} ->
      Cmts.fmt c loc
      @@ wrap_if (Std_longident.String_id.is_symbol txt) "( " " )" (str txt)
  | Ppat_alias (pat, {txt; loc}) ->
      let paren_pat =
        match pat.ppat_desc with
        | Ppat_or _ | Ppat_tuple _ -> Some true
        | _ -> None
      in
      hovbox 0
        (wrap_fits_breaks_if ~space:false c.conf parens "(" ")"
           (hovbox 0
              ( fmt_pattern c ?parens:paren_pat (sub_pat ~ctx pat)
              $ fmt "@ as@ "
              $ Cmts.fmt c loc
                  (wrap_if
                     (Std_longident.String_id.is_symbol txt)
                     "( " " )" (str txt) ) ) ) )
  | Ppat_constant const -> fmt_constant c const
  | Ppat_interval (l, u) -> fmt_constant c l $ str " .. " $ fmt_constant c u
  | Ppat_tuple pats ->
      let parens =
        parens || Poly.(c.conf.fmt_opts.parens_tuple_patterns.v = `Always)
      in
      hvbox 0
        (Params.wrap_tuple ~parens ~no_parens_if_break:false c.conf
           (list_k pats (Params.tuple_sep c.conf)
              (sub_pat ~ctx >> fmt_pattern c) ) )
  | Ppat_construct ({txt= Lident (("()" | "[]") as txt); loc}, None) ->
      let opn = txt.[0] and cls = txt.[1] in
      Cmts.fmt c loc
        (hvbox 0
           (wrap_k (char opn) (char cls)
              (Cmts.fmt_within c ~pro:(str " ") ~epi:(str " ") ppat_loc) ) )
  | Ppat_construct (lid, None) -> fmt_longident_loc c lid
  | Ppat_cons lp ->
      Cmts.fmt c ppat_loc
        (hvbox 0 (fmt_pat_cons c ~parens (List.map lp ~f:(sub_pat ~ctx))))
  | Ppat_construct (lid, Some (exists, pat)) ->
      cbox
        (Params.Indent.variant c.conf ~parens)
        (Params.parens_if parens c.conf
           ( fmt_longident_loc c lid $ fmt "@ "
           $ ( match exists with
             | [] -> noop
             | names ->
                 hvbox 0
                   (Params.parens c.conf
                      (str "type " $ list names "@ " (fmt_str_loc c)) )
                 $ fmt "@ " )
           $ fmt_pattern c (sub_pat ~ctx pat) ) )
  | Ppat_variant (lbl, None) -> variant_var c lbl
  | Ppat_variant (lbl, Some pat) ->
      cbox
        (Params.Indent.variant c.conf ~parens)
        (Params.parens_if parens c.conf
           (variant_var c lbl $ fmt "@ " $ fmt_pattern c (sub_pat ~ctx pat)) )
  | Ppat_record (flds, closed_flag) ->
      let fmt_field (lid, typ1, pat) =
        let typ1 = Option.map typ1 ~f:(sub_typ ~ctx) in
        let rhs =
          Option.map pat ~f:(fun p -> fmt_pattern c (sub_pat ~ctx p))
        in
        hvbox 0 @@ Cmts.fmt c ppat_loc @@ fmt_record_field c ?typ1 ?rhs lid
      in
      let p1, p2 = Params.get_record_pat c.conf ~ctx:ctx0 in
      let last_sep, fmt_underscore =
        match closed_flag with
        | OClosed -> (true, noop)
        | OOpen loc -> (false, Cmts.fmt ~pro:(break 1 2) c loc p2.wildcard)
      in
      let last_loc (lid, t, p) =
        match (t, p) with
        | _, Some p -> p.ppat_loc
        | Some t, _ -> t.ptyp_loc
        | _ -> lid.loc
      in
      let fmt_fields =
        fmt_elements_collection c ~last_sep p1 last_loc ppat_loc fmt_field
          flds
      in
      hvbox_if parens 0
        (Params.parens_if parens c.conf
           (p1.box (fmt_fields $ fmt_underscore)) )
  | Ppat_array [] ->
      hvbox 0
        (wrap_fits_breaks c.conf "[|" "|]" (Cmts.fmt_within c ppat_loc))
  | Ppat_array pats ->
      let p = Params.get_array_pat c.conf ~ctx:ctx0 in
      p.box
        (fmt_elements_collection c p Pat.location ppat_loc
           (sub_pat ~ctx >> fmt_pattern c >> hvbox 0)
           pats )
  | Ppat_list pats ->
      let p = Params.get_list_pat c.conf ~ctx:ctx0 in
      p.box
        (fmt_elements_collection c p Pat.location ppat_loc
           (sub_pat ~ctx >> fmt_pattern c >> hvbox 0)
           pats )
  | Ppat_or pats ->
      Cmts.relocate c.cmts ~src:ppat_loc ~before:(List.hd_exn pats).ppat_loc
        ~after:(List.last_exn pats).ppat_loc ;
      let nested =
        match ctx0 with
        | Pat {ppat_desc= Ppat_or _; _}
         |Exp {pexp_desc= Pexp_match _ | Pexp_try _ | Pexp_function _; _} ->
            List.is_empty xpat.ast.ppat_attributes
        | _ -> false
      in
      let xpats = List.map ~f:(sub_pat ~ctx) pats in
      let space p =
        match p.ppat_desc with
        | Ppat_constant
            {pconst_desc= Pconst_integer (i, _) | Pconst_float (i, _); _}
          -> (
          match i.[0] with '-' | '+' -> true | _ -> false )
        | _ -> false
      in
      let break {ast= p1; _} {ast= p2; _} =
        Poly.(c.conf.fmt_opts.break_cases.v = `Nested)
        || (not (Pat.is_simple p1))
        || (not (Pat.is_simple p2))
        || Cmts.has_after c.cmts p1.ppat_loc
      in
      let open_box =
        match c.conf.fmt_opts.break_cases.v with
        | `Fit_or_vertical | `Vertical -> open_hvbox
        | `Fit | `Nested | `Toplevel | `All -> open_hovbox
      in
      hvbox 0
        ( list_fl (List.group xpats ~break)
            (fun ~first:first_grp ~last:_ xpat_grp ->
              list_fl xpat_grp (fun ~first ~last:_ xpat ->
                  (* side effects of Cmts.fmt_before before [fmt_pattern] is
                     important *)
                  let loc = xpat.ast.ppat_loc in
                  let cmts_before = Cmts.has_before c.cmts loc in
                  let leading_cmt =
                    let pro, adj =
                      if first_grp && first then (noop, fmt "@ ")
                      else (fmt "@ ", noop)
                    in
                    Cmts.fmt_before ~pro c loc ~adj ~eol:noop
                  in
                  let pro =
                    if first_grp && first then
                      fmt_opt pro
                      $ fits_breaks
                          (if parens then "(" else "")
                          (if nested then "" else "( ")
                      $ open_box (-2)
                    else if first then
                      Params.get_or_pattern_sep c.conf ~ctx:ctx0 ~cmts_before
                      $ open_box (-2)
                    else
                      Params.get_or_pattern_sep c.conf ~ctx:ctx0 ~cmts_before
                        ~space:(space xpat.ast)
                  in
                  leading_cmt $ fmt_pattern c ~box:true ~pro xpat )
              $ close_box )
        $ fmt_or_k nested
            (fits_breaks (if parens then ")" else "") "")
            (fits_breaks (if parens then ")" else "") ~hint:(1, 2) ")") )
  | Ppat_constraint (pat, typ) ->
      hvbox 2
        (Params.parens_if parens c.conf
           ( fmt_pattern c (sub_pat ~ctx pat)
           $ ( match ctx0 with
             | Exp {pexp_desc= Pexp_let _; _} -> fmt "@ : "
             | _ -> fmt " :@ " )
           $ fmt_core_type c (sub_typ ~ctx typ) ) )
  | Ppat_type lid -> fmt_longident_loc c ~pre:"#" lid
  | Ppat_lazy pat ->
      cbox 2
        (Params.parens_if parens c.conf
           ( str "lazy"
           $ fmt_extension_suffix c ext
           $ fmt "@ "
           $ fmt_pattern c (sub_pat ~ctx pat) ) )
  | Ppat_unpack (name, pt) ->
      let fmt_constraint_opt pt k =
        match pt with
        | Some (id, cnstrs) ->
            hovbox 0
              (Params.parens_if parens c.conf
                 (hvbox 1
                    ( hovbox 0 (k $ fmt "@ : " $ fmt_longident_loc c id)
                    $ fmt_package_type c ctx cnstrs ) ) )
        | None -> wrap_fits_breaks_if ~space:false c.conf parens "(" ")" k
      in
      fmt_constraint_opt pt
        ( str "module"
        $ fmt_extension_suffix c ext
        $ char ' ' $ fmt_str_loc_opt c name )
  | Ppat_exception pat ->
      cbox 2
        (Params.parens_if parens c.conf
           ( fmt "exception"
           $ fmt_extension_suffix c ext
           $ fmt "@ "
           $ fmt_pattern c (sub_pat ~ctx pat) ) )
  | Ppat_extension
      ( ext
      , PPat
          ( ( { ppat_desc= Ppat_lazy _ | Ppat_unpack _ | Ppat_exception _
              ; ppat_loc
              ; ppat_attributes= []
              ; _ } as pat )
          , _ ) )
    when Source.extension_using_sugar ~name:ext ~payload:ppat_loc ->
      hvbox 0 (fmt_pattern ~ext c ~box (sub_pat ~ctx pat))
  | Ppat_extension ext ->
      hvbox c.conf.fmt_opts.extension_indent.v (fmt_extension c ctx ext)
  | Ppat_open (lid, pat) ->
      let can_skip_parens_extension : Extensions.Pattern.t -> _ = function
        | Epat_immutable_array (Iapat_immutable_array _) -> true
      in
      let can_skip_parens =
        match Extensions.Pattern.of_ast pat with
        | Some epat -> can_skip_parens_extension epat
        | None -> (
          match pat.ppat_desc with
          | Ppat_array _ | Ppat_list _ | Ppat_record _ -> true
          | Ppat_tuple _ ->
              Poly.(c.conf.fmt_opts.parens_tuple_patterns.v = `Always)
          | Ppat_construct ({txt= Lident "[]"; _}, None) -> true
          | _ -> false )
      in
      let opn, cls = if can_skip_parens then (".", "") else (".(", ")") in
      cbox 0
        ( fmt_longident_loc c lid
        $ wrap_k (str opn) (str cls)
            (fmt "@;<0 2>" $ fmt_pattern c (sub_pat ~ctx pat)) )

and fmt_pattern_extension ~ext:_ c ~pro:_ ~parens:_ ~box:_ ~ctx0 ~ctx
    ~ppat_loc : Extensions.Pattern.t -> _ = function
  | Epat_immutable_array (Iapat_immutable_array []) ->
      hvbox 0
        (wrap_fits_breaks c.conf "[:" ":]" (Cmts.fmt_within c ppat_loc))
  | Epat_immutable_array (Iapat_immutable_array pats) ->
      let p = Params.get_iarray_pat c.conf ~ctx:ctx0 in
      p.box
        (fmt_elements_collection c p Pat.location ppat_loc
           (sub_pat ~ctx >> fmt_pattern c >> hvbox 0)
           pats )

and fmt_fun_args c args =
  let fmt_fun_arg (a : Sugar.arg_kind) =
    match a with
    | Val
        ( islocal
        , ((Labelled l | Optional l) as lbl)
        , ( { ast=
                { ppat_desc=
                    ( Ppat_var {txt; loc= _}
                    | Ppat_constraint
                        ( { ppat_desc= Ppat_var {txt; loc= _}
                          ; ppat_attributes= []
                          ; _ }
                        , _ ) )
                ; ppat_attributes= []
                ; _ }
            ; _ } as xpat )
        , None )
      when String.equal l.txt txt ->
        let symbol = match lbl with Labelled _ -> "~" | _ -> "?" in
        cbox 0
          ( str symbol
          $ hovbox 0
              (Params.parens_if
                 (islocal || parenze_pat xpat)
                 c.conf
                 (fmt_if islocal "local_ " $ fmt_pattern ~parens:false c xpat) )
          )
    | Val (islocal, (Optional _ as lbl), xpat, None) ->
        let has_attr = not (List.is_empty xpat.ast.ppat_attributes) in
        let outer_parens, inner_parens =
          match Extensions.Pattern.of_ast xpat.ast with
          | Some epat -> (
            (* Inlined because there's nowhere better *)
            match epat with
            (* Same as the fallthrough case below *)
            | Epat_immutable_array (Iapat_immutable_array _) ->
                (not has_attr, false) )
          | None -> (
            match xpat.ast.ppat_desc with
            | Ppat_any | Ppat_var _ -> (false, false)
            | Ppat_unpack _ -> (not has_attr, true)
            | Ppat_tuple _ -> (false, true)
            | Ppat_or _ -> (has_attr, true)
            | _ -> (not has_attr, false) )
        in
        let outer_parens = outer_parens || islocal in
        cbox 2
          ( fmt_label lbl ":@,"
          $ hovbox 0
            @@ Params.parens_if outer_parens c.conf
                 ( fmt_if islocal "local_ "
                 $ fmt_pattern ~parens:inner_parens c xpat ) )
    | Val (islocal, ((Labelled _ | Nolabel) as lbl), xpat, None) ->
        cbox 2
          ( fmt_label lbl ":@,"
          $ Params.parens_if islocal c.conf
              (fmt_if islocal "local_ " $ fmt_pattern c xpat) )
    | Val
        ( islocal
        , Optional l
        , ( { ast= {ppat_desc= Ppat_var {txt; loc= _}; ppat_attributes= []; _}
            ; _ } as xpat )
        , Some xexp )
      when String.equal l.txt txt ->
        cbox 0
          (wrap "?(" ")"
             ( fmt_if islocal "local_ "
             $ fmt_pattern c ~box:true xpat
             $ fmt " =@;<1 2>"
             $ hovbox 2 (fmt_expression c xexp) ) )
    | Val
        ( islocal
        , Optional l
        , ( { ast=
                { ppat_desc=
                    Ppat_constraint
                      ({ppat_desc= Ppat_var {txt; loc= _}; _}, _)
                ; ppat_attributes= []
                ; _ }
            ; _ } as xpat )
        , Some xexp )
      when String.equal l.txt txt ->
        cbox 0
          (wrap "?(" ")"
             ( fmt_if islocal "local_ "
             $ fmt_pattern c ~parens:false ~box:true xpat
             $ fmt " =@;<1 2>" $ fmt_expression c xexp ) )
    | Val (islocal, Optional l, xpat, Some xexp) ->
        let parens =
          match xpat.ast.ppat_desc with
          | Ppat_unpack _ -> None
          | _ -> Some false
        in
        cbox 2
          ( str "?" $ str l.txt
          $ wrap_k (fmt ":@,(") (str ")")
              ( fmt_if islocal "local_ "
              $ fmt_pattern c ?parens ~box:true xpat
              $ fmt " =@;<1 2>" $ fmt_expression c xexp ) )
    | Val (_, (Labelled _ | Nolabel), _, Some _) ->
        impossible "not accepted by parser"
    | Newtypes [] -> impossible "not accepted by parser"
    | Newtypes names ->
        cbox 0
          (Params.parens c.conf
             (str "type " $ list names "@ " (fmt_str_loc c)) )
  in
  list args "@;" fmt_fun_arg

(** The second returned value of [fmt_body] belongs to a box of level N-1 if
    the first returned value belongs to a box of level N. *)
and fmt_body c ?ext ({ast= body; _} as xbody) =
  let ctx = Exp body in
  let parens = parenze_exp xbody in
  match body with
  | {pexp_desc= Pexp_function cs; pexp_attributes; pexp_loc; _} ->
      ( ( update_config_maybe_disabled c pexp_loc pexp_attributes
        @@ fun c ->
        fmt "@ "
        $ Cmts.fmt_before c pexp_loc
        $ fmt_if parens "(" $ str "function"
        $ fmt_extension_suffix c ext
        $ fmt_attributes c pexp_attributes )
      , update_config_maybe_disabled c pexp_loc pexp_attributes
        @@ fun c ->
        fmt_cases c ctx cs $ fmt_if parens ")" $ Cmts.fmt_after c pexp_loc )
  | { pexp_desc=
        Pexp_apply
          ( { pexp_desc= Pexp_extension ({txt= "extension.local"; _}, PStr [])
            ; _ }
          , [(Nolabel, sbody)] )
    ; _ } ->
      ( fmt " local_"
      , fmt_expression c ~eol:(fmt "@;<1000 0>") (sub_exp ~ctx sbody) )
  | { pexp_desc=
        Pexp_apply
          ( { pexp_desc=
                Pexp_extension ({txt= "extension.exclave"; _}, PStr [])
            ; _ }
          , [(Nolabel, sbody)] )
    ; _ } ->
      ( fmt " exclave_"
      , fmt_expression c ~eol:(fmt "@;<1000 0>") (sub_exp ~ctx sbody) )
  | _ -> (noop, fmt_expression c ~eol:(fmt "@;<1000 0>") xbody)

and fmt_indexop_access c ctx ~fmt_atrs ~has_attr ~parens x =
  let {pia_lhs; pia_kind; pia_paren; pia_rhs} = x in
  let wrap_paren =
    match pia_paren with
    | Paren -> wrap "(" ")"
    | Bracket -> wrap "[" "]"
    | Brace -> wrap "{" "}"
  in
  let inner_wrap = has_attr && Option.is_some pia_rhs in
  Params.parens_if parens c.conf
    (hovbox 0
       ( Params.parens_if inner_wrap c.conf
           ( fmt_expression c (sub_exp ~ctx pia_lhs)
           $ str "."
           $ ( match pia_kind with
             | Builtin idx ->
                 wrap_paren (fmt_expression c (sub_exp ~ctx idx))
             | Dotop (path, op, [idx]) ->
                 opt path (fun x -> fmt_longident_loc c x $ str ".")
                 $ str op
                 $ wrap_paren (fmt_expression c (sub_exp ~ctx idx))
             | Dotop (path, op, idx) ->
                 opt path (fun x -> fmt_longident_loc c x $ str ".")
                 $ str op
                 $ wrap_paren
                     (list idx ";@ " (sub_exp ~ctx >> fmt_expression c)) )
           $ opt pia_rhs (fun e ->
                 fmt_assign_arrow c $ fmt_expression c (sub_exp ~ctx e) ) )
       $ fmt_atrs ) )

(** Format [Pexp_fun] or [Pexp_newtype]. [wrap_intro] wraps up to after the
    [->] and is responsible for breaking. *)
and fmt_fun ?force_closing_paren
    ?(wrap_intro = fun x -> hvbox 2 x $ fmt "@ ") ?(box = true) ~label
    ?(parens = false) c ({ast; _} as xast) =
  let has_label = match label with Nolabel -> false | _ -> true in
  (* Make sure the comment is placed after the eventual label but not into
     the inner box if no label is present. Side effects of Cmts.fmt c.cmts
     before Sugar.fun_ is important. *)
  let has_cmts_outer, cmts_outer, cmts_inner =
    let eol = if has_label then Some (fmt "@,") else None in
    let has_cmts = Cmts.has_before c.cmts ast.pexp_loc in
    let cmts = Cmts.fmt_before ?eol c ast.pexp_loc in
    if has_label then (false, noop, cmts) else (has_cmts, cmts, noop)
  in
  let xargs, xbody = Sugar.fun_ c.cmts xast in
  let fmt_cstr, xbody = type_constr_and_body c xbody in
  let body =
    let box =
      match xbody.ast.pexp_desc with
      | Pexp_fun _ | Pexp_newtype _ -> Some false
      | Pexp_function _ when not c.conf.fmt_opts.ocp_indent_compat.v ->
          Some false
      | _ -> None
    in
    fmt_expression c ?box xbody
  and closing =
    if parens then closing_paren c ?force:force_closing_paren ~offset:(-2)
    else noop
  in
  let (label_sep : s), break_fun =
    (* Break between the label and the fun to avoid ocp-indent's alignment.
       If a label is present, arguments should be indented more than the
       arrow and the eventually breaking [fun] keyword. *)
    if c.conf.fmt_opts.ocp_indent_compat.v then (":@,", fmt "@;<1 2>")
    else (":", if has_label then fmt "@;<1 2>" else fmt "@ ")
  in
  hovbox_if box 2
    ( wrap_intro
        (hvbox_if has_cmts_outer 0
           ( cmts_outer
           $ hvbox 2
               ( fmt_label label label_sep $ cmts_inner $ fmt_if parens "("
               $ fmt "fun" $ break_fun
               $ hvbox 0
                   ( fmt_attributes c ast.pexp_attributes ~suf:" "
                   $ fmt_fun_args c xargs $ fmt_opt fmt_cstr
                   $ fmt "@;<1 -2>->" ) ) ) )
    $ body $ closing
    $ Cmts.fmt_after c ast.pexp_loc )

and fmt_label_arg ?(box = true) ?epi ?eol c (lbl, ({ast= arg; _} as xarg)) =
  match (lbl, arg.pexp_desc) with
  | (Labelled l | Optional l), Pexp_ident {txt= Lident i; loc}
    when String.equal l.txt i && List.is_empty arg.pexp_attributes ->
      Cmts.fmt c loc @@ Cmts.fmt c ?eol arg.pexp_loc @@ fmt_label lbl ""
  | ( (Labelled l | Optional l)
    , Pexp_constraint ({pexp_desc= Pexp_ident {txt= Lident i; _}; _}, _) )
    when String.equal l.txt i
         && List.is_empty arg.pexp_attributes
         && Ocaml_version.(
              compare c.conf.opr_opts.ocaml_version.v Releases.v4_14_0 >= 0 )
    ->
      let lbl =
        match lbl with
        | Labelled _ -> str "~"
        | Optional _ -> str "?"
        | Nolabel -> noop
      in
      lbl $ fmt_expression c ~box ?epi xarg
  | (Labelled _ | Optional _), _ when Cmts.has_after c.cmts xarg.ast.pexp_loc
    ->
      let cmts_after = Cmts.fmt_after c xarg.ast.pexp_loc in
      hvbox_if box 2
        ( hvbox_if box 0
            (fmt_expression c
               ~pro:(fmt_label lbl ":@;<0 2>")
               ~box ?epi xarg )
        $ cmts_after )
  | (Labelled _ | Optional _), (Pexp_fun _ | Pexp_newtype _) ->
      fmt_fun ~box ~label:lbl ~parens:true c xarg
  | _ ->
      let label_sep : s =
        if box || c.conf.fmt_opts.wrap_fun_args.v then ":@," else ":"
      in
      fmt_label lbl label_sep $ fmt_expression c ~box ?epi xarg

and expression_width c xe =
  String.length
    (Cmts.preserve ~cache_key:(Expression xe.ast)
       (fun () -> fmt_expression c xe)
       c.cmts )

and fmt_args_grouped ?epi:(global_epi = noop) c ctx args =
  let fmt_arg c ~first:_ ~last (lbl, arg) =
    let ({ast; _} as xarg) = sub_exp ~ctx arg in
    let box =
      match ast.pexp_desc with
      | Pexp_fun _ | Pexp_function _ -> Some false
      | _ -> None
    in
    let epi =
      match (lbl, last) with
      | _, true -> None
      | Nolabel, _ -> Some (fits_breaks "" ~hint:(1000, -1) "")
      | _ -> Some (fits_breaks "" ~hint:(1000, -3) "")
    in
    hovbox
      (Params.Indent.fun_args_group c.conf ~lbl ast)
      (fmt_label_arg c ?box ?epi (lbl, xarg))
    $ fmt_if_k (not last) (break_unless_newline 1 0)
  in
  let fmt_args ~first ~last args =
    hovbox
      (if first then 2 else 0)
      (list_fl args (fmt_arg c) $ fmt_if_k last global_epi)
    $ fmt_if_k (not last) (break 1 0)
  in
  let is_simple (lbl, x) =
    let xexp = sub_exp ~ctx x in
    let output =
      Cmts.preserve
        ~cache_key:(Arg (lbl, x))
        (fun () ->
          let cmts = Cmts.drop_before c.cmts x.pexp_loc in
          fmt_arg ~first:false ~last:false {c with cmts} (lbl, x) )
        c.cmts
    in
    let breaks = String.(rstrip output |> is_substring ~substring:"\n   ") in
    is_simple c.conf (expression_width c) xexp && not breaks
  in
  let break x y =
    Cmts.has_after c.cmts (snd x).pexp_loc || not (is_simple x && is_simple y)
  in
  let groups =
    if c.conf.fmt_opts.wrap_fun_args.v then List.group args ~break
    else List.map args ~f:(fun x -> [x])
  in
  list_fl groups fmt_args

and fmt_sequence c ?ext ~has_attr parens width xexp fmt_atrs =
  let fmt_sep c ?(force_break = false) xe1 ext xe2 =
    let break =
      let l1 = xe1.ast.pexp_loc and l2 = xe2.ast.pexp_loc in
      if sequence_blank_line c l1 l2 then fmt "\n@;<1000 0>"
      else if c.conf.fmt_opts.break_sequences.v || force_break then
        fmt "@;<1000 0>"
      else if parens && Poly.(c.conf.fmt_opts.sequence_style.v = `Before)
      then fmt "@;<1 -2>"
      else fmt "@;<1 0>"
    in
    match c.conf.fmt_opts.sequence_style.v with
    | `Before ->
        break $ str ";"
        $ fmt_extension_suffix c ext
        $ fmt_or_k (Option.is_some ext)
            (fmt_or parens "@ " "@;<1 2>")
            (str " ")
    | `Separator -> str " ;" $ fmt_extension_suffix c ext $ break
    | `Terminator -> str ";" $ fmt_extension_suffix c ext $ break
  in
  let is_simple x = is_simple c.conf width x in
  let break (_, xexp1) (_, xexp2) =
    not (is_simple xexp1 && is_simple xexp2)
  in
  let elts = Sugar.sequence c.cmts xexp in
  ( match elts with
  | (None, _) :: (first_ext, _) :: _ ->
      let compare {txt= x; _} {txt= y; _} = String.compare x y in
      assert (Option.compare compare first_ext ext = 0)
  | _ -> impossible "at least two elements" ) ;
  let grps = List.group elts ~break in
  let fmt_seq ~prev (ext, curr) ~next:_ =
    let f (_, prev) = fmt_sep c prev ext curr in
    opt prev f $ fmt_expression c curr
  in
  let fmt_seq_list ~prev x ~next:_ =
    let f prev =
      let prev = snd (List.last_exn prev) in
      let ext, curr = List.hd_exn x in
      fmt_sep c ~force_break:true prev ext curr
    in
    opt prev f $ list_pn x fmt_seq
  in
  hvbox 0
    (Params.Exp.wrap c.conf ~parens
       ( Params.parens_if has_attr c.conf
           (hvbox_if (parens || has_attr) 0 @@ list_pn grps fmt_seq_list)
       $ fmt_atrs ) )

and fmt_infix_op_args c ~parens xexp op_args =
  let op_prec = prec_ast (Exp xexp.ast) in
  let groups =
    let width xe = expression_width c xe in
    let not_simple arg = not (is_simple c.conf width arg) in
    let break (cmts_before1, _, (_, arg1)) (_, _, (_, arg2)) =
      Option.is_some cmts_before1 || not_simple arg1 || not_simple arg2
    in
    let break_infix =
      match c.conf.fmt_opts.break_infix.v with
      | `Wrap -> `Wrap
      | `Fit_or_vertical -> `Fit_or_vertical
      | `Wrap_or_vertical -> (
        match op_prec with
        | Some p when Prec.compare p InfixOp1 < 0 -> `Fit_or_vertical
        | Some _ ->
            if
              List.exists op_args ~f:(fun (_, _, (_, {ast= arg; _})) ->
                  match Ast.prec_ast (Exp arg) with
                  | Some p when Prec.compare p Apply <= 0 -> true
                  | Some _ -> false
                  | None -> false )
            then `Fit_or_vertical
            else `Wrap
        | None -> impossible "Pexp_apply expressions always have a prec" )
    in
    match break_infix with
    | `Wrap -> List.group op_args ~break
    | `Fit_or_vertical -> List.map ~f:(fun x -> [x]) op_args
  in
  let is_not_indented {ast= exp; _} =
    match exp.pexp_desc with
    | Pexp_letop _ | Pexp_letexception _ | Pexp_letmodule _ ->
        (* In 0.25.1 and before, these used to not break and were aligned at
           the end of the operator. Preserve the break to avoid introducing
           large diffs while allowing consistent formatting. *)
        Source.begins_line c.source exp.pexp_loc
    | Pexp_ifthenelse _ | Pexp_let _ | Pexp_match _ | Pexp_newtype _
     |Pexp_sequence _ | Pexp_try _ | Pexp_letopen _ ->
        true
    | _ -> false
  in
  let fmt_arg ~epi ~very_last xarg =
    let parens =
      ((not very_last) && exposed_right_exp Ast.Non_apply xarg.ast)
      || parenze_exp xarg
    in
    if Params.Exp.Infix_op_arg.dock c.conf xarg then
      (* Indentation of docked fun or function start before the operator.
         Warning: [fmt_expression] doesn't use the [epi] in every case. *)
      hovbox 2 (fmt_expression c ~parens ~box:false ~epi xarg)
    else
      let expr_box =
        match xarg.ast.pexp_desc with
        | Pexp_fun _ | Pexp_function _ -> Some false
        | _ -> None
      in
      hvbox 0
        ( epi
        $ hovbox_if (not very_last) 2
            (fmt_expression c ?box:expr_box ~parens xarg) )
  in
  let fmt_op_arg_group ~first:first_grp ~last:last_grp args =
    let indent = if first_grp && parens then -2 else 0 in
    hovbox indent
      (list_fl args
         (fun ~first ~last (cmts_before, cmts_after, (op, xarg)) ->
           let very_first = first_grp && first in
           let very_last = last_grp && last in
           let epi, before_arg =
             let break =
               if very_last && is_not_indented xarg then fmt "@ "
               else fmt_if (not very_first) " "
             in
             match cmts_after with
             | Some c -> (noop, op $ break $ c)
             | None -> (op $ break, noop)
           in
           fmt_opt cmts_before $ before_arg
           $ fmt_arg ~epi ~very_last xarg
           $ fmt_if_k (not last) (break 1 0) ) )
    $ fmt_if_k (not last_grp) (break 1 0)
  in
  Params.Exp.Infix_op_arg.wrap c.conf ~parens
    ~parens_nested:(Ast.parenze_nested_exp xexp)
    (Params.Align.infix_op c.conf (list_fl groups fmt_op_arg_group))

and fmt_pat_cons c ~parens args =
  let groups =
    let not_simple arg = not (Pat.is_simple arg.ast) in
    let break args1 args2 = not_simple args1 || not_simple args2 in
    (* [break-infix = wrap-or-vertical] is not applicable for patterns as
       there are no infix operators allowed besides [::], falling back on
       [fit-or-vertical] is arbitrary. *)
    match c.conf.fmt_opts.break_infix.v with
    | `Wrap -> List.group args ~break
    | `Fit_or_vertical | `Wrap_or_vertical -> List.map ~f:(fun x -> [x]) args
  in
  let fmt_op_arg_group ~first:first_grp ~last:last_grp args =
    let indent = if first_grp && parens then -2 else 0 in
    hovbox indent
      (list_fl args (fun ~first ~last xarg ->
           let very_first = first_grp && first in
           let very_last = last_grp && last in
           hvbox 0
             ( fmt_if (not very_first) ":: "
             $ hovbox_if (not very_last) 2 (fmt_pattern c ~box:true xarg) )
           $ fmt_if_k (not last) (break 1 0) ) )
    $ fmt_if_k (not last_grp) (break 1 0)
  in
  Params.Exp.Infix_op_arg.wrap c.conf ~parens ~parens_nested:false
    (list_fl groups fmt_op_arg_group)

and fmt_match c ?epi ~parens ?ext ctx xexp cs e0 keyword =
  let ctx0 = xexp.ctx in
  let indent = Params.match_indent c.conf ~parens ~ctx:ctx0 in
  hvbox indent
    ( fmt_opt epi
    $ Params.Exp.wrap c.conf ~parens ~disambiguate:true
      @@ Params.Align.match_ c.conf ~xexp
      @@ ( hvbox 0
             ( str keyword
             $ fmt_extension_suffix c ext
             $ fmt_attributes c xexp.ast.pexp_attributes
             $ fmt "@;<1 2>"
             $ fmt_expression c (sub_exp ~ctx e0)
             $ fmt "@ with" )
         $ fmt "@ " $ fmt_cases c ctx cs ) )

and maybe_fmt_expression_extension c ~pexp_loc ~fmt_atrs ~has_attr ~parens
    ~ctx exp fmt_normal_expr =
  match Extensions.Expression.of_ast exp with
  | Some eexp ->
      fmt_expression_extension c ~pexp_loc ~fmt_atrs ~has_attr ~parens ~ctx
        eexp
  | None -> fmt_normal_expr ()

and fmt_expression c ?(box = true) ?pro ?epi ?eol ?parens ?(indent_wrap = 0)
    ?ext ({ast= exp; ctx= ctx0} as xexp) =
  protect c (Exp exp)
  @@
  let {pexp_desc; pexp_loc; pexp_attributes; _} = exp in
  update_config_maybe_disabled c pexp_loc pexp_attributes
  @@ fun c ->
  Cmts.relocate_wrongfully_attached_cmts c.cmts c.source exp ;
  let fmt_cmts = Cmts.fmt c ?eol pexp_loc in
  let fmt_atrs = fmt_attributes c ~pre:Space pexp_attributes in
  let has_attr = not (List.is_empty pexp_attributes) in
  let parens = Option.value parens ~default:(parenze_exp xexp) in
  let ctx = Exp exp in
  let fmt_args_grouped ?epi e0 a1N =
    fmt_args_grouped c ctx ?epi ((Nolabel, e0) :: a1N)
  in
  hvbox_if box 0 ~name:"expr"
  @@ fmt_cmts
  @@ (fun fmt -> fmt_opt pro $ fmt)
  @@ maybe_fmt_expression_extension c ~pexp_loc ~fmt_atrs ~has_attr ~parens
       ~ctx exp
  @@ fun () ->
  match pexp_desc with
  | Pexp_apply (_, []) -> impossible "not produced by parser"
  | Pexp_sequence
      ( { pexp_desc=
            Pexp_extension
              ( name
              , PStr
                  [ ( { pstr_desc=
                          Pstr_eval (({pexp_desc= Pexp_fun _; _} as call), [])
                      ; pstr_loc= _ } as pld ) ] )
        ; _ }
      , e2 ) ->
      let xargs, xbody = Sugar.fun_ c.cmts (sub_exp ~ctx:(Str pld) call) in
      let fmt_cstr, xbody = type_constr_and_body c xbody in
      let is_simple x = is_simple c.conf (expression_width c) x in
      let break xexp1 xexp2 = not (is_simple xexp1 && is_simple xexp2) in
      let grps =
        List.group
          (List.map ~f:snd (Sugar.sequence c.cmts (sub_exp ~ctx e2)))
          ~break
      in
      let fmt_grp grp = list grp " ;@ " (fmt_expression c) in
      hvbox 0
        (Params.parens_if parens c.conf
           ( hvbox c.conf.fmt_opts.extension_indent.v
               (wrap "[" "]"
                  ( str "%"
                  $ hovbox 2
                      ( fmt_str_loc c name $ str " fun "
                      $ fmt_attributes c ~suf:" " call.pexp_attributes
                      $ fmt_fun_args c xargs $ fmt_opt fmt_cstr $ fmt "@ ->"
                      )
                  $ fmt "@ " $ fmt_expression c xbody ) )
           $ fmt "@ ;@ "
           $ list grps " ;@;<1000 0>" fmt_grp ) )
  | Pexp_infix
      ( {txt= "|>"; loc}
      , e0
      , { pexp_desc=
            Pexp_extension
              ( name
              , PStr
                  [ ( { pstr_desc=
                          Pstr_eval (({pexp_desc= Pexp_fun _; _} as retn), [])
                      ; pstr_loc= _ } as pld ) ] )
        ; _ } ) ->
      let xargs, xbody = Sugar.fun_ c.cmts (sub_exp ~ctx:(Str pld) retn) in
      let fmt_cstr, xbody = type_constr_and_body c xbody in
      hvbox 0
        (Params.Exp.wrap c.conf ~parens
           ( fmt_expression c (sub_exp ~ctx e0)
           $ fmt "@\n"
           $ Cmts.fmt c loc (fmt "|>@\n")
           $ hvbox c.conf.fmt_opts.extension_indent.v
               (wrap "[" "]"
                  ( str "%"
                  $ hovbox 2
                      ( fmt_str_loc c name $ str " fun "
                      $ fmt_attributes c ~suf:" " retn.pexp_attributes
                      $ fmt_fun_args c xargs $ fmt_opt fmt_cstr $ fmt "@ ->"
                      )
                  $ fmt "@ " $ fmt_expression c xbody ) ) ) )
  | Pexp_infix ({txt= ":="; loc}, r, v)
    when is_simple c.conf (expression_width c) (sub_exp ~ctx r) ->
      let bol_indent = Params.Indent.assignment_operator_bol c.conf in
      let cmts_before =
        let indent, adj =
          (* Use the same break for comment and operator. Comments are placed
             according to indentation. *)
          match c.conf.fmt_opts.assignment_operator.v with
          | `Begin_line -> (bol_indent, noop)
          | `End_line -> (2, fmt "@,")
        in
        Cmts.fmt_before c loc ~pro:(break 1 indent) ~epi:adj ~adj
      in
      let cmts_after = Cmts.fmt_after c loc ~pro:noop ~epi:noop in
      Params.parens_if parens c.conf
        (hovbox 0
           ( match c.conf.fmt_opts.assignment_operator.v with
           | `Begin_line ->
               hvbox 0 (fmt_expression c (sub_exp ~ctx r) $ cmts_before)
               $ break 1 bol_indent $ fmt ":= " $ cmts_after
               $ hvbox 2 (fmt_expression c (sub_exp ~ctx v))
           | `End_line ->
               hvbox 0
                 ( hvbox 0 (fmt_expression c (sub_exp ~ctx r) $ cmts_before)
                 $ str " :=" )
               $ fmt "@;<1 2>" $ cmts_after
               $ hvbox 2 (fmt_expression c (sub_exp ~ctx v)) ) )
  | Pexp_prefix ({txt= ("~-" | "~-." | "~+" | "~+.") as op; loc}, e1) ->
      let op =
        if Location.width loc = String.length op - 1 then
          String.sub op ~pos:1 ~len:(String.length op - 1)
        else op
      in
      let spc = fmt_if (Exp.exposed_left e1) "@ " in
      Params.parens_if parens c.conf
        ( Cmts.fmt c pexp_loc
          @@ hvbox 2 (str op $ spc $ fmt_expression c (sub_exp ~ctx e1))
        $ fmt_atrs )
  | Pexp_infix (({txt= id; _} as op), l, ({pexp_desc= Pexp_ident _; _} as r))
    when Std_longident.String_id.is_hash_getter id ->
      Params.parens_if parens c.conf
        ( fmt_expression c (sub_exp ~ctx l)
        $ hvbox 0 (fmt_str_loc c op)
        $ fmt_expression c (sub_exp ~ctx r) )
  | Pexp_infix
      (op, l, ({pexp_desc= Pexp_fun _; pexp_loc; pexp_attributes; _} as r))
    when not c.conf.fmt_opts.break_infix_before_func.v ->
      (* side effects of Cmts.fmt c.cmts before Sugar.fun_ is important *)
      let cmts_before = Cmts.fmt_before c pexp_loc in
      let cmts_after = Cmts.fmt_after c pexp_loc in
      let xr = sub_exp ~ctx r in
      let parens_r = parenze_exp xr in
      let xargs, xbody = Sugar.fun_ c.cmts xr in
      let fmt_cstr, xbody = type_constr_and_body c xbody in
      let indent_wrap = if parens then -2 else 0 in
      let pre_body, body = fmt_body c ?ext xbody in
      let followed_by_infix_op =
        match xbody.ast.pexp_desc with
        | Pexp_infix (_, _, {pexp_desc= Pexp_fun _ | Pexp_function _; _}) ->
            true
        | _ -> false
      in
      wrap_fits_breaks_if c.conf parens "(" ")"
        ( hovbox 0
            (wrap_if has_attr "(" ")"
               ( hvbox 2
                   ( hvbox indent_wrap
                       ( fmt_expression ~indent_wrap c (sub_exp ~ctx l)
                       $ fmt "@;"
                       $ hovbox 2
                           ( hvbox 0
                               ( fmt_str_loc c op $ fmt "@ " $ cmts_before
                               $ fmt_if parens_r "(" $ str "fun " )
                           $ fmt_attributes c pexp_attributes ~suf:" "
                           $ hvbox_if
                               (not c.conf.fmt_opts.wrap_fun_args.v)
                               4
                               (fmt_fun_args c xargs $ fmt_opt fmt_cstr)
                           $ fmt "@ ->" ) )
                   $ pre_body )
               $ fmt_or followed_by_infix_op "@;<1000 0>" "@ "
               $ body $ fmt_if parens_r ")" $ cmts_after ) )
        $ fmt_atrs )
  | Pexp_infix
      ( op
      , l
      , ({pexp_desc= Pexp_function cs; pexp_loc; pexp_attributes; _} as r) )
    when not c.conf.fmt_opts.break_infix_before_func.v ->
      let cmts_before = Cmts.fmt_before c pexp_loc in
      let cmts_after = Cmts.fmt_after c pexp_loc in
      let xr = sub_exp ~ctx r in
      let parens_r = parenze_exp xr in
      let indent = Params.Indent.function_ c.conf ~parens xr in
      Params.parens_if parens c.conf
        (hvbox indent
           ( hvbox 0
               ( fmt_expression c (sub_exp ~ctx l)
               $ fmt "@;"
               $ hovbox 2
                   ( hvbox 0
                       ( fmt_str_loc c op $ fmt "@ " $ cmts_before
                       $ fmt_if parens_r "( " $ str "function"
                       $ fmt_extension_suffix c ext )
                   $ fmt_attributes c pexp_attributes ) )
           $ fmt "@ " $ fmt_cases c (Exp r) cs $ fmt_if parens_r " )"
           $ cmts_after ) )
  | Pexp_infix _ ->
      let op_args = Sugar.Exp.infix c.cmts (prec_ast (Exp exp)) xexp in
      let inner_wrap = parens || has_attr in
      let outer_wrap =
        match ctx0 with
        (* infix operator used to build a function *)
        | Exp {pexp_desc= Pexp_apply (f, _); _} when phys_equal f exp ->
            has_attr && parens
        | Exp
            { pexp_desc=
                Pexp_apply ({pexp_desc= Pexp_ident {txt= id; loc= _}; _}, _)
            ; _ }
          when not (Std_longident.is_infix id) ->
            has_attr && parens
        | _ -> has_attr && not parens
      in
      let infix_op_args =
        List.map op_args ~f:(fun (op, arg) ->
            match op with
            | Some op ->
                (* side effects of Cmts.fmt_before before fmt_expression is
                   important *)
                let adj = break 1000 0 in
                let fmt_before_cmts =
                  if Cmts.has_before c.cmts op.loc then
                    Some (Cmts.fmt_before ~adj c op.loc)
                  else None
                in
                (* The comments before the first arg are put there, so that
                   they are printed after the operator and the box is
                   correctly broken before the following arguments. Keeping
                   the comments in the arg box would not break properly the
                   current box. OTOH, relocating the comments would put them
                   before the operator in some cases and make the formatting
                   unstable. *)
                let fmt_after_cmts =
                  if
                    Cmts.has_after c.cmts op.loc
                    || Cmts.has_before c.cmts arg.ast.pexp_loc
                  then
                    Some
                      ( Cmts.fmt_after c op.loc
                      $ Cmts.fmt_before ~adj c arg.ast.pexp_loc )
                  else None
                in
                let fmt_op = fmt_str_loc c op in
                (fmt_before_cmts, fmt_after_cmts, (fmt_op, arg))
            | None -> (None, None, (noop, arg)) )
      in
      hvbox_if outer_wrap 0
        (Params.parens_if outer_wrap c.conf
           (hvbox indent_wrap
              ( fmt_infix_op_args ~parens:inner_wrap c xexp infix_op_args
              $ fmt_atrs ) ) )
  | Pexp_apply
      ( {pexp_desc= Pexp_extension ({txt= "extension.local"; _}, PStr []); _}
      , [(Nolabel, sbody)] ) ->
      Params.parens_if parens c.conf
        (fmt "local_@ " $ fmt_expression c (sub_exp ~ctx sbody))
  | Pexp_apply
      ( { pexp_desc= Pexp_extension ({txt= "extension.exclave"; _}, PStr [])
        ; _ }
      , [(Nolabel, sbody)] ) ->
      Params.parens_if parens c.conf
        (fmt "exclave_@ " $ fmt_expression c (sub_exp ~ctx sbody))
  | Pexp_prefix (op, e) ->
      let has_cmts = Cmts.has_before c.cmts e.pexp_loc in
      hvbox 2
        (Params.Exp.wrap c.conf ~parens
           ( fmt_str_loc c op $ fmt_if has_cmts "@,"
           $ fmt_expression c ~box (sub_exp ~ctx e)
           $ fmt_atrs ) )
  | Pexp_apply (e0, e1N1) -> (
      let wrap =
        if c.conf.fmt_opts.wrap_fun_args.v then Fn.id else hvbox 2
      in
      let (lbl, last_arg), args_before =
        match List.rev e1N1 with
        | [] -> assert false
        | hd :: tl -> (hd, List.rev tl)
      in
      let intro_epi, expr_epi =
        (* [intro_epi] should be placed inside the inner most box but before
           anything. [expr_epi] is placed in the outermost box, outside of
           parenthesis. *)
        let dock_fun_arg =
          (* Do not dock the arguments when there's more than one. *)
          (not c.conf.fmt_opts.ocp_indent_compat.v)
          || Location.line_difference e0.pexp_loc last_arg.pexp_loc = 0
        in
        if parens || not dock_fun_arg then (noop, fmt_opt epi)
        else (fmt_opt epi, noop)
      in
      match last_arg.pexp_desc with
      | Pexp_fun (_, _, _, eN1_body)
        when List.for_all args_before ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
          (* Last argument is a [fun _ ->]. *)
          let xlast_arg = sub_exp ~ctx last_arg in
          let args =
            let break_body =
              match eN1_body.pexp_desc with
              | Pexp_function _ ->
                  break 1
                    (Params.Indent.docked_function_after_fun c.conf
                       ~parens:true ~lbl )
              | _ ->
                  break 1
                    (Params.Indent.docked_fun c.conf ~source:c.source
                       ~loc:last_arg.pexp_loc ~lbl )
            in
            let wrap_intro x =
              wrap
                ( intro_epi
                $ fmt_args_grouped e0 args_before
                $ fmt "@ " $ hvbox 0 x )
              $ break_body
            in
            let force_closing_paren =
              if Location.is_single_line pexp_loc c.conf.fmt_opts.margin.v
              then Fit
              else Break
            in
            fmt_fun c ~force_closing_paren ~wrap_intro ~label:lbl
              ~parens:true xlast_arg
          in
          hvbox_if has_attr 0
            (expr_epi $ Params.parens_if parens c.conf (args $ fmt_atrs))
      | Pexp_function [{pc_lhs; pc_guard= None; pc_rhs}]
        when List.for_all args_before ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
          let force =
            if
              Location.is_single_line last_arg.pexp_loc
                c.conf.fmt_opts.margin.v
            then Fit
            else Break
          in
          let ctx = Exp last_arg in
          (* side effects of Cmts.fmt_before before [fmt_pattern] is
             important *)
          let leading_cmt = Cmts.fmt_before c pc_lhs.ppat_loc in
          hvbox 2
            ( expr_epi
            $ Params.parens_if parens c.conf
                ( hovbox 4
                    ( wrap
                        ( intro_epi
                        $ fmt_args_grouped e0 args_before
                        $ fmt "@ "
                        $ Cmts.fmt_before c last_arg.pexp_loc
                        $ fmt_label lbl ":" $ str "(function"
                        $ fmt_attributes c ~pre:Blank
                            last_arg.pexp_attributes )
                    $ fmt "@ " $ leading_cmt
                    $ hvbox 0
                        ( fmt_pattern c ~pro:(if_newline "| ")
                            (sub_pat ~ctx pc_lhs)
                        $ fmt "@ ->" )
                    $ fmt "@ "
                    $ cbox 0 (fmt_expression c (sub_exp ~ctx pc_rhs))
                    $ closing_paren c ~force
                    $ Cmts.fmt_after c last_arg.pexp_loc )
                $ fmt_atrs ) )
      | Pexp_function cs
        when List.for_all args_before ~f:(fun (_, eI) ->
                 is_simple c.conf (fun _ -> 0) (sub_exp ~ctx eI) ) ->
          let wrap =
            if c.conf.fmt_opts.wrap_fun_args.v then hovbox 2 else hvbox 2
          in
          let xlast_arg = sub_exp ~ctx last_arg in
          let ctx'' = Exp last_arg in
          hvbox
            (Params.Indent.docked_function c.conf ~parens xlast_arg)
            ( expr_epi
            $ Params.parens_if parens c.conf
                ( wrap
                    ( intro_epi
                    $ fmt_args_grouped e0 args_before
                    $ fmt "@ "
                    $ Cmts.fmt_before c last_arg.pexp_loc
                    $ fmt_label lbl ":" $ str "(function"
                    $ fmt_attributes c ~pre:Blank last_arg.pexp_attributes )
                $ fmt "@ " $ fmt_cases c ctx'' cs $ closing_paren c
                $ Cmts.fmt_after c last_arg.pexp_loc
                $ fmt_atrs ) )
      | _ ->
          let fmt_atrs =
            fmt_attributes c ~pre:(Break (1, -2)) pexp_attributes
          in
          let force =
            if Location.is_single_line pexp_loc c.conf.fmt_opts.margin.v then
              Fit
            else Break
          in
          fmt_opt epi $ fmt_if parens "("
          $ hvbox 2
              ( fmt_args_grouped ~epi:fmt_atrs e0 e1N1
              $ fmt_if_k parens (closing_paren c ~force ~offset:(-3)) ) )
  | Pexp_array [] ->
      hvbox 0
        (Params.parens_if parens c.conf
           ( wrap_fits_breaks c.conf "[|" "|]" (Cmts.fmt_within c pexp_loc)
           $ fmt_atrs ) )
  | Pexp_array e1N ->
      let p = Params.get_array_expr c.conf in
      hvbox_if has_attr 0
        (Params.parens_if parens c.conf
           ( p.box
               (fmt_expressions c (expression_width c) (sub_exp ~ctx) e1N
                  (sub_exp ~ctx >> fmt_expression c)
                  p pexp_loc )
           $ fmt_atrs ) )
  | Pexp_list e1N ->
      let p = Params.get_list_expr c.conf in
      let offset =
        if c.conf.fmt_opts.dock_collection_brackets.v then 0 else 2
      in
      let cmt_break = break 1 offset in
      hvbox_if has_attr 0
        (Params.parens_if parens c.conf
           ( p.box
               (fmt_expressions c (expression_width c) (sub_exp ~ctx) e1N
                  (fun e ->
                    let fmt_cmts = Cmts.fmt c ~eol:cmt_break e.pexp_loc in
                    fmt_cmts @@ (sub_exp ~ctx >> fmt_expression c) e )
                  p pexp_loc )
           $ fmt_atrs ) )
  | Pexp_assert e0 ->
      let paren_body =
        if Exp.is_symbol e0 || Exp.is_monadic_binding e0 then
          not (List.is_empty e0.pexp_attributes)
        else parenze_exp (sub_exp ~ctx e0)
      in
      hovbox 0
        (Params.parens_if parens c.conf
           (hvbox 0
              ( hvbox 2
                  ( str "assert"
                  $ fmt_extension_suffix c ext
                  $ fmt_or paren_body " (@," "@ "
                  $ fmt_expression c ~parens:false (sub_exp ~ctx e0) )
              $ fmt_if_k paren_body (closing_paren c)
              $ fmt_atrs ) ) )
  | Pexp_constant const ->
      Params.parens_if
        (parens || not (List.is_empty pexp_attributes))
        c.conf
        (fmt_constant c ?epi const $ fmt_atrs)
  | Pexp_constraint (e, t) ->
      hvbox
        (Params.Indent.exp_constraint c.conf)
        ( wrap_fits_breaks ~space:false c.conf "(" ")"
            ( fmt_expression c (sub_exp ~ctx e)
            $ fmt "@ : "
            $ fmt_core_type c (sub_typ ~ctx t) )
        $ fmt_atrs )
  | Pexp_construct ({txt= Lident (("()" | "[]") as txt); loc}, None) ->
      let opn = char txt.[0] and cls = char txt.[1] in
      let pro = str " " and epi = str " " in
      Cmts.fmt c loc
      @@ hvbox 0
           (Params.parens_if parens c.conf
              ( wrap_k opn cls (Cmts.fmt_within c ~pro ~epi pexp_loc)
              $ fmt_atrs ) )
  | Pexp_construct (lid, None) ->
      Params.parens_if parens c.conf (fmt_longident_loc c lid $ fmt_atrs)
  | Pexp_cons l ->
      Cmts.fmt c pexp_loc
        ( hvbox indent_wrap
            (fmt_infix_op_args c ~parens xexp
               (List.mapi l ~f:(fun i e ->
                    (None, None, (fmt_if (i > 0) "::", sub_exp ~ctx e)) ) ) )
        $ fmt_atrs )
  | Pexp_construct (lid, Some arg) ->
      Params.parens_if parens c.conf
        ( hvbox 2
            ( fmt_longident_loc c lid $ fmt "@ "
            $ fmt_expression c (sub_exp ~ctx arg) )
        $ fmt_atrs )
  | Pexp_variant (s, arg) ->
      hvbox
        (Params.Indent.variant c.conf ~parens)
        (Params.parens_if parens c.conf
           ( variant_var c s
           $ opt arg (fmt "@ " >$ (sub_exp ~ctx >> fmt_expression c))
           $ fmt_atrs ) )
  | Pexp_field (exp, lid) ->
      hvbox 2
        (Params.parens_if parens c.conf
           ( fmt_expression c (sub_exp ~ctx exp)
           $ fmt "@,." $ fmt_longident_loc c lid $ fmt_atrs ) )
  | Pexp_newtype _ | Pexp_fun _ ->
      let xargs, xbody = Sugar.fun_ c.cmts xexp in
      let fmt_cstr, xbody = type_constr_and_body c xbody in
      let body_is_function =
        match xbody.ast.pexp_desc with Pexp_function _ -> true | _ -> false
      in
      let pre_body, body = fmt_body c ?ext xbody in
      let indent =
        if body_is_function then
          let default_indent =
            if Option.is_none eol then 2
            else if c.conf.fmt_opts.let_binding_deindent_fun.v then 1
            else 0
          in
          Params.Indent.function_ ~default:default_indent c.conf ~parens xexp
        else Params.Indent.fun_ ?eol c.conf
      in
      let intro =
        let kw =
          str "fun"
          $ fmt_extension_suffix c ext
          $ str " "
          $ fmt_attributes c pexp_attributes ~suf:" "
        and args = fmt_fun_args c xargs in
        Params.Exp.box_fun_decl_args c.conf ~parens ~kw ~args ~annot:fmt_cstr
      in
      hvbox_if (box || body_is_function) indent
        (Params.Exp.wrap c.conf ~parens ~disambiguate:true ~fits_breaks:false
           ~offset_closing_paren:(-2)
           (hovbox 2 (intro $ str " ->" $ pre_body) $ fmt "@ " $ body) )
  | Pexp_function cs ->
      let indent = Params.Indent.function_ c.conf ~parens xexp in
      Params.Exp.wrap c.conf ~parens ~disambiguate:true ~fits_breaks:false
      @@ Params.Align.function_ c.conf ~parens ~ctx0 ~self:exp
      @@ ( hvbox 2
             ( str "function"
             $ fmt_extension_suffix c ext
             $ fmt_attributes c pexp_attributes )
         $ break 1 indent
         $ hvbox 0 (fmt_cases c ctx cs) )
  | Pexp_ident {txt; loc} ->
      let outer_parens = has_attr && parens in
      Cmts.fmt c loc
      @@ wrap_if outer_parens "(" ")"
      @@ (fmt_longident txt $ Cmts.fmt_within c loc $ fmt_atrs)
  | Pexp_ifthenelse (if_branches, else_) ->
      let last_loc =
        match else_ with
        | Some e -> e.pexp_loc
        | None -> (List.last_exn if_branches).if_body.pexp_loc
      in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:pexp_loc ~after:last_loc ;
      let parens_prev_bch = ref false in
      let cnd_exps =
        let with_conds =
          List.map if_branches ~f:(fun x ->
              ( Some (sub_exp ~ctx x.if_cond)
              , sub_exp ~ctx x.if_body
              , x.if_attrs ) )
        in
        match else_ with
        | Some x ->
            List.rev ((None, sub_exp ~ctx x, []) :: List.rev with_conds)
        | None -> with_conds
      in
      hvbox 0
        ( Params.Exp.wrap c.conf ~parens:(parens || has_attr)
            (hvbox 0
               (list_fl cnd_exps
                  (fun ~first ~last (xcond, xbch, pexp_attributes) ->
                    let symbol_parens = Exp.is_symbol xbch.ast in
                    let parens_bch = parenze_exp xbch && not symbol_parens in
                    let parens_exp = false in
                    let p =
                      Params.get_if_then_else c.conf ~first ~last ~parens_bch
                        ~parens_prev_bch:!parens_prev_bch ~xcond ~xbch
                        ~expr_loc:pexp_loc
                        ~fmt_extension_suffix:
                          (Option.map ext ~f:(fun _ ->
                               fmt_extension_suffix c ext ) )
                        ~fmt_attributes:
                          (fmt_attributes c ~pre:Blank pexp_attributes)
                        ~fmt_cond:(fmt_expression ~box:false c)
                    in
                    parens_prev_bch := parens_bch ;
                    p.box_branch
                      ( p.cond
                      $ p.box_keyword_and_expr
                          ( p.branch_pro
                          $ p.wrap_parens
                              ( fmt_expression c ?box:p.box_expr
                                  ~parens:parens_exp ?pro:p.expr_pro
                                  ?eol:p.expr_eol p.branch_expr
                              $ p.break_end_branch ) ) )
                    $ fmt_if_k (not last) p.space_between_branches ) ) )
        $ fmt_atrs )
  | Pexp_let (lbs, body) ->
      let bindings =
        Sugar.Let_binding.of_let_bindings c.cmts ~ctx lbs.pvbs_bindings
      in
      let fmt_expr = fmt_expression c (sub_exp ~ctx body) in
      let ext = lbs.pvbs_extension in
      fmt_let_bindings c ?ext ~parens ~fmt_atrs ~fmt_expr ~has_attr
        lbs.pvbs_rec bindings body
  | Pexp_letop {let_; ands; body} ->
      let bd = Sugar.Let_binding.of_binding_ops c.cmts ~ctx (let_ :: ands) in
      let fmt_expr = fmt_expression c (sub_exp ~ctx body) in
      fmt_let_bindings c ?ext ~parens ~fmt_atrs ~fmt_expr ~has_attr
        Nonrecursive bd body
  | Pexp_letexception (ext_cstr, exp) ->
      let pre =
        str "let exception" $ fmt_extension_suffix c ext $ fmt "@ "
      in
      hvbox 0
        ( Params.parens_if
            (parens || not (List.is_empty pexp_attributes))
            c.conf
            ( hvbox 0
                ( hvbox 2
                    (hvbox 2
                       (pre $ fmt_extension_constructor c ctx ext_cstr) )
                $ fmt "@ in" )
            $ fmt "@;<1000 0>"
            $ fmt_expression c (sub_exp ~ctx exp) )
        $ fmt_atrs )
  | Pexp_letmodule (name, args, pmod, exp) ->
      let keyword = "let module" in
      let xbody = sub_mod ~ctx pmod in
      let xbody, xmty =
        match xbody.ast with
        | { pmod_desc= Pmod_constraint (body_me, body_mt)
          ; pmod_loc
          ; pmod_attributes= [] } ->
            Cmts.relocate c.cmts ~src:pmod_loc ~before:body_me.pmod_loc
              ~after:body_mt.pmty_loc ;
            (sub_mod ~ctx body_me, Some (sub_mty ~ctx body_mt))
        | _ -> (xbody, None)
      in
      let can_sparse =
        match xbody.ast.pmod_desc with
        | Pmod_apply _ | Pmod_apply_unit _ -> true
        | _ -> false
      in
      hvbox 0
        ( Params.parens_if
            (parens || not (List.is_empty pexp_attributes))
            c.conf
            ( hvbox 2
                (fmt_module c ctx keyword ~eqty:":" name args (Some xbody)
                   xmty [] ~epi:(str "in") ~can_sparse ?ext ~rec_flag:false )
            $ fmt "@;<1000 0>"
            $ fmt_expression c (sub_exp ~ctx exp) )
        $ fmt_atrs )
  | Pexp_open (lid, e0) ->
      let can_skip_parens_extension attrs : Extensions.Expression.t -> _ =
        function
        | Eexp_comprehension
            (Cexp_list_comprehension _ | Cexp_array_comprehension _)
         |Eexp_immutable_array (Iaexp_immutable_array _) ->
            List.is_empty attrs
      in
      let can_skip_parens =
        (not (Cmts.has_before c.cmts e0.pexp_loc))
        && (not (Cmts.has_after c.cmts e0.pexp_loc))
        &&
        match Extensions.Expression.of_ast e0 with
        | Some ee0 -> can_skip_parens_extension e0.pexp_attributes ee0
        | None -> (
          match e0.pexp_desc with
          | (Pexp_array _ | Pexp_list _ | Pexp_record _)
            when List.is_empty e0.pexp_attributes ->
              true
          | Pexp_tuple _ -> Poly.(c.conf.fmt_opts.parens_tuple.v = `Always)
          | Pexp_construct ({txt= Lident "[]"; _}, None) -> true
          | _ -> false )
      in
      let outer_parens = has_attr && parens in
      let inner_parens = not can_skip_parens in
      hovbox 0
        (Params.parens_if outer_parens c.conf
           ( hvbox 0
               ( hvbox 0
                   ( fmt_longident_loc c lid $ str "."
                   $ fmt_if inner_parens "(" )
               $ fmt "@;<0 2>"
               $ fmt_expression c (sub_exp ~ctx e0)
               $ fmt_if_k inner_parens (closing_paren c) )
           $ fmt_atrs ) )
  | Pexp_letopen
      ( { popen_override= flag
        ; popen_expr
        ; popen_attributes= attributes
        ; popen_loc }
      , e0 ) ->
      let override = is_override flag in
      let outer_parens = has_attr && parens in
      let inner_parens = has_attr || parens in
      hovbox 0
        (Params.Exp.wrap c.conf ~parens:outer_parens ~fits_breaks:false
           ( hvbox 0
               (Params.Exp.wrap c.conf ~parens:inner_parens
                  ~fits_breaks:false
                  (vbox 0
                     ( hvbox 0
                         ( fmt_module_statement c ~attributes
                             ~keyword:
                               ( hvbox 0
                                   ( str "let" $ break 1 0
                                   $ Cmts.fmt_before c popen_loc
                                   $ fmt_or override "open!" "open"
                                   $ opt ext (fun _ -> fmt_if override " ")
                                   $ fmt_extension_suffix c ext )
                               $ break 1 0 )
                             (sub_mod ~ctx popen_expr)
                         $ Cmts.fmt_after c popen_loc
                         $ str " in" )
                     $ break 1000 0
                     $ fmt_expression c (sub_exp ~ctx e0) ) ) )
           $ fmt_atrs ) )
  | Pexp_try (e0, [{pc_lhs; pc_guard; pc_rhs}])
    when Poly.(
           c.conf.fmt_opts.single_case.v = `Compact
           && c.conf.fmt_opts.break_cases.v <> `All
           && c.conf.fmt_opts.break_cases.v <> `Vertical ) ->
      (* side effects of Cmts.fmt_before before [fmt_pattern] is important *)
      let xpc_rhs = sub_exp ~ctx pc_rhs in
      let leading_cmt = Cmts.fmt_before c pc_lhs.ppat_loc in
      let parens_here, parens_for_exp =
        if c.conf.fmt_opts.leading_nested_match_parens.v then (false, None)
        else (parenze_exp xpc_rhs, Some false)
      in
      Params.Exp.wrap c.conf ~parens ~disambiguate:true
        (hvbox 2
           ( hvbox 0
               ( str "try"
               $ fmt_extension_suffix c ext
               $ fmt_attributes c pexp_attributes
               $ fmt "@;<1 2>"
               $ fmt_expression c (sub_exp ~ctx e0) )
           $ break 1 (-2)
           $ hvbox 0
               ( hvbox 0
                   ( fmt "with@ " $ leading_cmt
                   $ hvbox 0
                       ( fmt_pattern c ~pro:(if_newline "| ")
                           (sub_pat ~ctx pc_lhs)
                       $ opt pc_guard (fun g ->
                             fmt "@ when "
                             $ fmt_expression c (sub_exp ~ctx g) )
                       $ fmt "@ ->" $ fmt_if parens_here " (" ) )
               $ fmt "@;<1 2>"
               $ cbox 0 (fmt_expression c ?parens:parens_for_exp xpc_rhs) )
           $ fmt_if parens_here
               ( match c.conf.fmt_opts.indicate_multiline_delimiters.v with
               | `No -> ")"
               | `Space -> " )"
               | `Closing_on_separate_line -> "@;<1000 -2>)" ) ) )
  | Pexp_match (e0, cs) ->
      fmt_match c ?epi ~parens ?ext ctx xexp cs e0 "match"
  | Pexp_try (e0, cs) -> fmt_match c ?epi ~parens ?ext ctx xexp cs e0 "try"
  | Pexp_pack (me, pt) ->
      let outer_parens = parens && has_attr in
      let blk = fmt_module_expr c (sub_mod ~ctx me) in
      let align = Params.Align.module_pack c.conf ~me in
      let opn_paren =
        match c.conf.fmt_opts.indicate_multiline_delimiters.v with
        | `No | `Closing_on_separate_line -> str "("
        | `Space ->
            let level =
              if Option.is_none pt then 1
              else if Option.is_some blk.opn then 3
              else 2
            in
            fits_breaks ~level "(" "( "
      and cls_paren = closing_paren c ~offset:(-2) in
      let pro =
        fmt_if_k (not align) opn_paren
        $ str "module"
        $ fmt_extension_suffix c ext
        $ char ' '
      and epi = cls_paren in
      let fmt_mod m =
        match pt with
        | Some (id, cnstrs) ->
            hvbox 2
              ( hovbox 0 (m $ fmt "@ : " $ fmt_longident_loc c id)
              $ fmt_package_type c ctx cnstrs )
        | None -> m
      in
      hvbox 0
        (Params.parens_if outer_parens c.conf
           ( fmt_if_k align opn_paren
           $ compose_module ~pro ~epi blk ~f:fmt_mod
           $ fmt_atrs ) )
  | Pexp_record (flds, default) ->
      let fmt_field (lid, (typ1, typ2), exp) =
        let typ1 = Option.map typ1 ~f:(sub_typ ~ctx) in
        let typ2 = Option.map typ2 ~f:(sub_typ ~ctx) in
        let rhs =
          Option.map exp ~f:(fun e -> fmt_expression c (sub_exp ~ctx e))
        in
        hvbox 0 @@ fmt_record_field c ?typ1 ?typ2 ?rhs lid
      in
      let p1, p2 = Params.get_record_expr c.conf in
      let last_loc (lid, (t1, t2), e) =
        match (t1, t2, e) with
        | _, _, Some e -> e.pexp_loc
        | _, Some t2, _ -> t2.ptyp_loc
        | Some t1, _, _ -> t1.ptyp_loc
        | _ -> lid.loc
      in
      let fmt_fields =
        fmt_elements_collection c p1 last_loc pexp_loc fmt_field flds
          ~pro:(break 1 2)
      in
      hvbox_if has_attr 0
        ( p1.box
            ( opt default (fun d ->
                  hvbox 2 (fmt_expression c (sub_exp ~ctx d) $ fmt "@;<1 -2>")
                  $ str "with" $ p2.break_after_with )
            $ fmt_fields )
        $ fmt_atrs )
  | Pexp_extension
      ( ext
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( ( {pexp_desc= Pexp_sequence _; pexp_attributes= []; _} as
                      e1 )
                  , _ )
            ; pstr_loc= _ } ] )
    when Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc
         && List.length (Sugar.sequence c.cmts xexp) > 1 ->
      fmt_sequence ~has_attr c parens (expression_width c) xexp fmt_atrs ~ext
  | Pexp_sequence _ ->
      fmt_sequence ~has_attr c parens (expression_width c) xexp fmt_atrs ?ext
  | Pexp_setfield (e1, lid, e2) ->
      hvbox 0
        (Params.Exp.wrap c.conf ~parens
           ( Params.parens_if has_attr c.conf
               ( fmt_expression c (sub_exp ~ctx e1)
               $ str "." $ fmt_longident_loc c lid $ fmt_assign_arrow c
               $ fmt_expression c (sub_exp ~ctx e2) )
           $ fmt_atrs ) )
  | Pexp_tuple es ->
      let parens =
        match xexp.ctx with
        | Str {pstr_desc= Pstr_eval _; pstr_loc= _} -> false
        | Exp {pexp_desc= Pexp_indexop_access {pia_kind= Builtin idx; _}; _}
          when phys_equal exp idx ->
            false
        | Exp
            { pexp_desc= Pexp_indexop_access {pia_kind= Dotop (_, _, idx); _}
            ; _ }
          when List.exists idx ~f:(phys_equal exp) ->
            false
        | _ -> parens || Poly.(c.conf.fmt_opts.parens_tuple.v = `Always)
      in
      let no_parens_if_break =
        match xexp.ctx with
        | Exp {pexp_desc= Pexp_extension _; _} -> true
        | Pld _ -> true
        | Str {pstr_desc= Pstr_eval _; _} -> true
        | _ -> false
      in
      let outer_wrap = has_attr && parens in
      let inner_wrap = has_attr || parens in
      hvbox_if outer_wrap 0
        (Params.parens_if outer_wrap c.conf
           ( hvbox 0
               (Params.wrap_tuple ~parens:inner_wrap ~no_parens_if_break
                  c.conf
                  (list_k es (Params.tuple_sep c.conf)
                     (sub_exp ~ctx >> fmt_expression c) ) )
           $ fmt_atrs ) )
  | Pexp_lazy e ->
      hvbox 2
        (Params.Exp.wrap c.conf ~parens
           ( str "lazy"
           $ fmt_extension_suffix c ext
           $ fmt "@ "
           $ fmt_expression c (sub_exp ~ctx e)
           $ fmt_atrs ) )
  | Pexp_extension
      ( ext
      , PStr
          [ ( { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc=
                            ( Pexp_while _ | Pexp_for _ | Pexp_match _
                            | Pexp_try _ | Pexp_let _ | Pexp_ifthenelse _
                            | Pexp_new _ | Pexp_letmodule _ | Pexp_object _
                            | Pexp_function _ | Pexp_letexception _
                            | Pexp_open _ | Pexp_assert _ | Pexp_lazy _
                            | Pexp_pack _ | Pexp_fun _ | Pexp_beginend _
                            | Pexp_letopen _ )
                        ; pexp_attributes= []
                        ; _ } as e1 )
                    , _ )
              ; pstr_loc= _ } as str ) ] )
    when Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc ->
      let outer_parens = has_attr && parens in
      let inner_parens = has_attr || parens in
      hvbox 0
        (Params.parens_if outer_parens c.conf
           ( fmt_expression c ~box ?eol ~parens:inner_parens ~ext
               (sub_exp ~ctx:(Str str) e1)
           $ fmt_atrs ) )
  | Pexp_extension
      ( ext
      , PStr
          [ ( { pstr_desc=
                  Pstr_eval
                    ( ( {pexp_desc= Pexp_infix _; pexp_attributes= []; _} as
                        e1 )
                    , _ )
              ; pstr_loc= _ } as str ) ] )
    when List.is_empty pexp_attributes
         && Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc ->
      hvbox 0
        ( fmt_expression c ~box ?eol ~parens ~ext (sub_exp ~ctx:(Str str) e1)
        $ fmt_atrs )
  | Pexp_extension ext ->
      hvbox 0
        (Params.Exp.wrap c.conf ~parens
           ( hvbox c.conf.fmt_opts.extension_indent.v
               (fmt_extension c ctx ext)
           $ fmt_atrs ) )
  | Pexp_for (p1, e1, e2, dir, e3) ->
      hvbox 0
        (Params.Exp.wrap c.conf ~parens
           ( hovbox 0
               ( hvbox 2
                   ( hvbox 0
                       ( str "for"
                       $ fmt_extension_suffix c ext
                       $ fmt "@;<1 2>"
                       $ hovbox 0
                           ( fmt_pattern c (sub_pat ~ctx p1)
                           $ fmt "@ =@;<1 2>"
                           $ fmt_expression c (sub_exp ~ctx e1)
                           $ fmt_direction_flag dir
                           $ fmt_expression c (sub_exp ~ctx e2) )
                       $ fmt "@;do" )
                   $ fmt "@;<1000 0>"
                   $ fmt_expression c (sub_exp ~ctx e3) )
               $ fmt "@;<1000 0>done" )
           $ fmt_atrs ) )
  | Pexp_coerce (e1, t1, t2) ->
      hvbox 2
        (Params.parens_if (parens && has_attr) c.conf
           ( wrap_fits_breaks ~space:false c.conf "(" ")"
               ( fmt_expression c (sub_exp ~ctx e1)
               $ opt t1 (fmt "@ : " >$ (sub_typ ~ctx >> fmt_core_type c))
               $ fmt "@ :> "
               $ fmt_core_type c (sub_typ ~ctx t2) )
           $ fmt_atrs ) )
  | Pexp_while (e1, e2) ->
      hvbox 0
        (Params.Exp.wrap c.conf ~parens
           ( hovbox 0
               ( hvbox 2
                   ( hvbox 0
                       ( str "while"
                       $ fmt_extension_suffix c ext
                       $ fmt "@;<1 2>"
                       $ fmt_expression c (sub_exp ~ctx e1)
                       $ fmt "@;do" )
                   $ fmt "@;<1000 0>"
                   $ fmt_expression c (sub_exp ~ctx e2) )
               $ fmt "@;<1000 0>done" )
           $ fmt_atrs ) )
  | Pexp_unreachable -> str "."
  | Pexp_send (exp, meth) ->
      hvbox 2
        (Params.parens_if parens c.conf
           ( fmt_expression c (sub_exp ~ctx exp)
           $ fmt "@,#" $ fmt_str_loc c meth $ fmt_atrs ) )
  | Pexp_new {txt; loc} ->
      Cmts.fmt c loc
      @@ hvbox 2
           (Params.parens_if parens c.conf
              ( str "new"
              $ fmt_extension_suffix c ext
              $ fmt "@ " $ fmt_longident txt $ fmt_atrs ) )
  | Pexp_object {pcstr_self; pcstr_fields} ->
      hvbox 0
        (Params.parens_if parens c.conf
           ( fmt_class_structure c ~ctx ?ext pcstr_self pcstr_fields
           $ fmt_atrs ) )
  | Pexp_override l -> (
      let fmt_field ({txt; loc}, f) =
        let eol = fmt "@;<1 3>" in
        let txt = Longident.lident txt in
        match f.pexp_desc with
        | Pexp_ident {txt= txt'; loc}
          when Std_longident.field_alias ~field:txt txt'
               && List.is_empty f.pexp_attributes ->
            Cmts.fmt c ~eol loc @@ fmt_longident txt'
        | _ ->
            Cmts.fmt c ~eol loc @@ fmt_longident txt
            $ str " = "
            $ fmt_expression c (sub_exp ~ctx f)
      in
      match l with
      | [] ->
          Params.parens_if parens c.conf
            (wrap "{<" ">}" (Cmts.fmt_within c pexp_loc) $ fmt_atrs)
      | _ ->
          hvbox 0
            (Params.parens_if parens c.conf
               ( wrap_fits_breaks ~space:false c.conf "{<" ">}"
                   (list l "@;<0 1>; " fmt_field)
               $ fmt_atrs ) ) )
  | Pexp_setinstvar (name, expr) ->
      hvbox 0
        (Params.Exp.wrap c.conf ~parens
           ( Params.parens_if has_attr c.conf
               ( fmt_str_loc c name $ fmt_assign_arrow c
               $ hvbox 2 (fmt_expression c (sub_exp ~ctx expr)) )
           $ fmt_atrs ) )
  | Pexp_indexop_access x ->
      fmt_indexop_access c ctx ~fmt_atrs ~has_attr ~parens x
  | Pexp_poly _ ->
      impossible "only used for methods, handled during method formatting"
  | Pexp_hole -> hvbox 0 (fmt_hole () $ fmt_atrs)
  | Pexp_beginend e ->
      let wrap_beginend k =
        let opn = str "begin" $ fmt_extension_suffix c ext
        and cls = str "end" in
        hvbox 0
          (wrap_k opn cls (wrap_k (break 1 2) (break 1000 0) k) $ fmt_atrs)
      in
      wrap_beginend
      @@ fmt_expression c ~box ?pro ?epi ?eol ~parens:false ~indent_wrap ?ext
           (sub_exp ~ctx e)
  | Pexp_parens e ->
      hvbox 0
      @@ fmt_expression c ~box ?pro ?epi ?eol ~parens:true ~indent_wrap ?ext
           (sub_exp ~ctx e)
      $ fmt_atrs

and fmt_expression_extension c ~pexp_loc ~fmt_atrs ~has_attr ~parens ~ctx :
    Extensions.Expression.t -> _ = function
  | Eexp_comprehension cexpr ->
      let punctuation, space_around, comp =
        match cexpr with
        | Cexp_list_comprehension comp ->
            ("", c.conf.fmt_opts.space_around_lists.v, comp)
        | Cexp_array_comprehension (amut, comp) ->
            let punct =
              match amut with Mutable _ -> "|" | Immutable -> ":"
            in
            (punct, c.conf.fmt_opts.space_around_arrays.v, comp)
      in
      hvbox_if has_attr 0
        (Params.parens_if parens c.conf
           ( Params.wrap_comprehension c.conf ~space_around ~punctuation
               (fmt_comprehension c ~ctx comp)
           $ fmt_atrs ) )
  | Eexp_immutable_array (Iaexp_immutable_array []) ->
      hvbox 0
        (Params.parens_if parens c.conf
           ( wrap_fits_breaks c.conf "[:" ":]" (Cmts.fmt_within c pexp_loc)
           $ fmt_atrs ) )
  | Eexp_immutable_array (Iaexp_immutable_array e1N) ->
      let p = Params.get_iarray_expr c.conf in
      hvbox_if has_attr 0
        (Params.parens_if parens c.conf
           ( p.box
               (fmt_expressions c (expression_width c) (sub_exp ~ctx) e1N
                  (sub_exp ~ctx >> fmt_expression c)
                  p pexp_loc )
           $ fmt_atrs ) )

and fmt_comprehension c ~ctx Extensions.Comprehensions.{body; clauses} =
  hvbox 0 (* Don't indent the clauses to the right of the body *)
    ( fmt_expression c (sub_exp ~ctx body)
    $ sequence (List.map clauses ~f:(fmt_comprehension_clause ~ctx c)) )

and fmt_comprehension_clause c ~ctx
    (clause : Extensions.Comprehensions.clause) =
  let subclause kwd formatter item =
    fits_breaks " " ~hint:(1000, 0) ""
    $ hvbox 2 (str kwd $ sp (Break (1, 0)) $ hovbox 2 (formatter c item))
  in
  match clause with
  | For bindings ->
      list_fl bindings (fun ~first ~last:_ ->
          subclause
            (if first then "for" else "and")
            (fmt_comprehension_binding ~ctx) )
  | When cond ->
      subclause "when" (fun c xt -> fmt_expression c xt) (sub_exp ~ctx cond)

and fmt_comprehension_binding c ~ctx
    Extensions.Comprehensions.{pattern; iterator; attributes} =
  fmt_attributes c attributes
  $ fmt_pattern c (sub_pat ~ctx pattern)
  $ sp Space
  (* The harder break is after the [=]/[in] *)
  $ fmt_comprehension_iterator c ~ctx iterator

and fmt_comprehension_iterator c ~ctx :
    Extensions.Comprehensions.iterator -> _ = function
  | Range {start; stop; direction} ->
      fmt "=@;<1 0>"
      $ fmt_expression c (sub_exp ~ctx start)
      $ fmt_direction_flag direction
      $ fmt_expression c (sub_exp ~ctx stop)
  | In seq -> fmt "in@;<1 0>" $ fmt_expression c (sub_exp ~ctx seq)

and fmt_let_bindings c ?ext ~parens ~has_attr ~fmt_atrs ~fmt_expr
    rec_flag bindings body =
  let indent_after_in =
    match body.pexp_desc with
    | Pexp_let _ | Pexp_letmodule _
     |Pexp_extension
        ( _
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( { pexp_desc= Pexp_let _ | Pexp_letmodule _
                      ; pexp_attributes= []
                      ; _ }
                    , _ )
              ; pstr_loc= _ } ] ) ->
        0
    | _ -> c.conf.fmt_opts.indent_after_in.v
  in
  fmt_let c ~ext ~rec_flag ~bindings ~parens ~has_attr ~fmt_atrs ~fmt_expr
    ~body_loc:body.pexp_loc ~indent_after_in

and fmt_class_structure c ~ctx ?ext self_ fields =
  let update_config c i =
    match i.pcf_desc with
    | Pcf_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let self_ =
    opt self_ (fun self_ ->
        fmt "@;"
        $ Params.parens c.conf
            (fmt_pattern c ~parens:false (sub_pat ~ctx self_)) )
  in
  let fmt_item c ctx ~prev:_ ~next:_ i = fmt_class_field c (sub_cf ~ctx i) in
  let ast x = Clf x in
  hvbox 2
    ( hvbox 0 (str "object" $ fmt_extension_suffix c ext $ self_)
    $ ( match fields with
      | {pcf_desc= Pcf_attribute a; _} :: _ when Attr.is_doc a -> str "\n"
      | _ -> noop )
    $ fmt_or_k (List.is_empty fields)
        (Cmts.fmt_within ~epi:noop c (Ast.location ctx))
        (fmt "@;<1000 0>")
    $ fmt_item_list c ctx update_config ast fmt_item fields )
  $ fmt_or (List.is_empty fields) "@ " "@;<1000 0>"
  $ str "end"

(** [epi] is a function to ensure ordered access to comments. *)
and fmt_class_signature c ~ctx ~pro ~epi ?ext self_ fields =
  let update_config c i =
    match i.pctf_desc with
    | Pctf_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let self_ =
    opt self_ (fun self_ ->
        let no_attr typ = List.is_empty typ.ptyp_attributes in
        fmt "@;"
        $ Params.parens_if (no_attr self_) c.conf
            (fmt_core_type c (sub_typ ~ctx self_)) )
  in
  let fmt_item c ctx ~prev:_ ~next:_ i =
    fmt_class_type_field c (sub_ctf ~ctx i)
  in
  let ast x = Ctf x in
  let cmts_within =
    if List.is_empty fields then
      (* Side effect order is important. *)
      Cmts.fmt_within ~pro:noop c (Ast.location ctx)
    else noop
  in
  hvbox 2
    ( hvbox 2 (pro $ str "object" $ fmt_extension_suffix c ext $ self_)
    $ fmt "@ " $ cmts_within
    $ fmt_item_list c ctx update_config ast fmt_item fields
    $ fmt_if (not (List.is_empty fields)) "@;<1000 -2>"
    $ hvbox 0 (str "end" $ epi ()) )

and fmt_class_type ?(pro = noop) c ({ast= typ; _} as xtyp) =
  protect c (Cty typ)
  @@
  let {pcty_desc; pcty_loc; pcty_attributes} = typ in
  update_config_maybe_disabled c pcty_loc pcty_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pcty_attributes in
  let parens = parenze_cty xtyp in
  let ctx = Cty typ in
  let pro ~cmt =
    pro
    $ (if cmt then Cmts.fmt_before c pcty_loc else noop)
    $ fmt_if parens "("
  and epi ~attrs =
    fmt_if parens ")"
    $ (if attrs then fmt_attributes c atrs else noop)
    $ Cmts.fmt_after c pcty_loc
    $ fmt_docstring c ~pro:(fmt "@ ") doc
  in
  match pcty_desc with
  | Pcty_constr (name, params) ->
      let params = List.map params ~f:(fun x -> (x, [])) in
      hvbox 2
        ( pro ~cmt:false
        $ Cmts.fmt_before c pcty_loc
        $ hovbox 0
            ( fmt_class_params c ctx params
            $ fmt_longident_loc c name $ epi ~attrs:true ) )
  | Pcty_signature {pcsig_self; pcsig_fields} ->
      let pro = pro ~cmt:true in
      let epi () = epi ~attrs:true in
      fmt_class_signature c ~ctx ~pro ~epi pcsig_self pcsig_fields
  | Pcty_arrow (args, rhs) ->
      Cmts.relocate c.cmts ~src:pcty_loc
        ~before:(List.hd_exn args).pap_type.ptyp_loc ~after:rhs.pcty_loc ;
      let args = List.map ~f:(fun arg -> (arg, false)) args in
      let pro =
        pro ~cmt:true
        $ fmt_arrow_type c ~ctx ~parens:false ~parent_has_parens:parens args
            None
        $ Params.Pcty.arrow c.conf ~rhs
      in
      fmt_class_type c ~pro (sub_cty ~ctx rhs) $ epi ~attrs:true
  | Pcty_extension ext ->
      hvbox 2 (pro ~cmt:true $ fmt_extension c ctx ext $ epi ~attrs:true)
  | Pcty_open (popen, cl) ->
      let pro =
        pro ~cmt:true
        $ fmt_open_description c ~keyword:"let open" ~kw_attributes:atrs
            popen
        $ str " in"
        $ Params.Pcty.break_let_open c.conf ~rhs:cl
      in
      fmt_class_type c ~pro (sub_cty ~ctx cl) $ epi ~attrs:false

and fmt_class_expr c ({ast= exp; ctx= ctx0} as xexp) =
  protect c (Cl exp)
  @@
  let {pcl_desc; pcl_loc; pcl_attributes} = exp in
  update_config_maybe_disabled c pcl_loc pcl_attributes
  @@ fun c ->
  let parens = parenze_cl xexp in
  let ctx = Cl exp in
  let fmt_args_grouped e0 a1N =
    (* TODO: consider [e0] when grouping *)
    fmt_class_expr c (sub_cl ~ctx e0) $ fmt "@ " $ fmt_args_grouped c ctx a1N
  in
  let fmt_cmts = Cmts.fmt c pcl_loc in
  let fmt_atrs = fmt_attributes c ~pre:Space pcl_attributes in
  hvbox 0 @@ fmt_cmts
  @@
  match pcl_desc with
  | Pcl_constr (name, params) ->
      let params = List.map params ~f:(fun x -> (x, [])) in
      fmt_class_params c ctx params $ fmt_longident_loc c name $ fmt_atrs
  | Pcl_structure {pcstr_fields; pcstr_self} ->
      hvbox 0
        (Params.parens_if parens c.conf
           ( fmt_class_structure c ~ctx ?ext:None pcstr_self pcstr_fields
           $ fmt_atrs ) )
  | Pcl_fun _ ->
      let xargs, xbody = Sugar.cl_fun c.cmts xexp in
      let indent =
        match ctx0 with
        | Cl {pcl_desc= Pcl_fun _; _} -> 0
        | Cl _ -> 3
        | _ -> 0
      in
      hvbox indent
        (Params.parens_if parens c.conf
           ( hovbox 2
               ( box_fun_decl_args c 0
                   ( str "fun "
                   $ fmt_attributes c pcl_attributes ~suf:" "
                   $ wrap_fun_decl_args c (fmt_fun_args c xargs)
                   $ fmt "@ " )
               $ str "->" )
           $ fmt "@ " $ fmt_class_expr c xbody ) )
  | Pcl_apply (e0, e1N1) ->
      Params.parens_if parens c.conf
        (hvbox 2 (fmt_args_grouped e0 e1N1) $ fmt_atrs)
  | Pcl_let (lbs, body) ->
      let indent_after_in =
        match body.pcl_desc with
        | Pcl_let _ -> 0
        | _ -> c.conf.fmt_opts.indent_after_in.v
      in
      let bindings =
        Sugar.Let_binding.of_let_bindings c.cmts ~ctx lbs.pvbs_bindings
      in
      let fmt_expr = fmt_class_expr c (sub_cl ~ctx body) in
      let has_attr = not (List.is_empty pcl_attributes) in
      fmt_let c ~ext:None ~rec_flag:lbs.pvbs_rec ~bindings ~parens ~has_attr
        ~fmt_atrs ~fmt_expr ~body_loc:body.pcl_loc ~indent_after_in
  | Pcl_constraint (e, t) ->
      hvbox 2
        (wrap_fits_breaks ~space:false c.conf "(" ")"
           ( fmt_class_expr c (sub_cl ~ctx e)
           $ fmt " :@ "
           $ fmt_class_type c (sub_cty ~ctx t) ) )
      $ fmt_atrs
  | Pcl_extension ext -> fmt_extension c ctx ext $ fmt_atrs
  | Pcl_open (popen, cl) ->
      hvbox 0
        ( fmt_open_description c ~keyword:"let open"
            ~kw_attributes:pcl_attributes popen
        $ fmt " in@;<1000 0>"
        $ fmt_class_expr c (sub_cl ~ctx cl) )

and fmt_class_field_kind c ctx = function
  | Cfk_virtual typ ->
      (fmt "@ : " $ fmt_core_type c (sub_typ ~ctx typ), noop, noop, noop)
  | Cfk_concrete
      ( _
      , { pexp_desc=
            Pexp_poly
              (e, Some ({ptyp_desc= Ptyp_poly (poly_args, _); _} as poly))
        ; pexp_loc
        ; _ } ) -> (
      let rec cleanup names e args' =
        match (e, args') with
        | {pexp_desc= Pexp_constraint (e, t); _}, [] ->
            Some (List.rev names, t, e)
        | ( {pexp_desc= Pexp_newtype (({txt; _} as newtyp), body); _}
          , {txt= txt'; _} :: args )
          when String.equal txt txt' ->
            cleanup (newtyp :: names) body args
        | _ -> None
      in
      match cleanup [] e poly_args with
      | Some (args, t, e) ->
          let before =
            match args with x :: _ -> x.loc | [] -> e.pexp_loc
          in
          Cmts.relocate c.cmts ~src:pexp_loc ~before ~after:e.pexp_loc ;
          ( fmt "@ : type "
            $ list args "@ " (fmt_str_loc c)
            $ fmt_core_type ~pro:"." ~pro_space:false c (sub_typ ~ctx t)
          , noop
          , fmt "@;<1 2>="
          , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) )
      | None ->
          ( fmt "@ : " $ fmt_core_type c (sub_typ ~ctx poly)
          , noop
          , fmt "@;<1 2>="
          , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) ) )
  | Cfk_concrete (_, {pexp_desc= Pexp_poly (e, poly); pexp_loc; _}) ->
      let xargs, xbody =
        match poly with
        | None ->
            Sugar.fun_ c.cmts ~will_keep_first_ast_node:false
              (sub_exp ~ctx e)
        | Some _ -> ([], sub_exp ~ctx e)
      in
      let ty, e =
        match (xbody.ast, poly) with
        | {pexp_desc= Pexp_constraint (e, t); pexp_loc; _}, None ->
            Cmts.relocate c.cmts ~src:pexp_loc ~before:t.ptyp_loc
              ~after:e.pexp_loc ;
            (Some t, sub_exp ~ctx e)
        | {pexp_desc= Pexp_constraint _; _}, Some _ -> (poly, xbody)
        | _, poly -> (poly, xbody)
      in
      Cmts.relocate c.cmts ~src:pexp_loc ~before:e.ast.pexp_loc
        ~after:e.ast.pexp_loc ;
      ( noop
      , fmt_if (not (List.is_empty xargs)) "@ "
        $ wrap_fun_decl_args c (fmt_fun_args c xargs)
        $ opt ty (fun t -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx t))
      , fmt "@;<1 2>="
      , fmt "@ " $ fmt_expression c e )
  | Cfk_concrete (_, e) ->
      let ty, e =
        match e with
        | {pexp_desc= Pexp_constraint (e, t); _} -> (Some t, e)
        | _ -> (None, e)
      in
      ( opt ty (fun t -> fmt "@ : " $ fmt_core_type c (sub_typ ~ctx t))
      , noop
      , fmt "@;<1 2>="
      , fmt "@ " $ fmt_expression c (sub_exp ~ctx e) )

and fmt_class_field c {ast= cf; _} =
  protect c (Clf cf)
  @@
  let fmt_cmts_before = Cmts.Toplevel.fmt_before c cf.pcf_loc in
  let fmt_cmts_after = Cmts.Toplevel.fmt_after c cf.pcf_loc in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~fit:true c cf.pcf_attributes
  in
  let fmt_atrs = fmt_item_attributes c ~pre:(Break (1, 0)) atrs in
  let ctx = Clf cf in
  (fun k ->
    fmt_cmts_before
    $ hvbox 0 ~name:"clf"
        (hvbox 0 (doc_before $ k $ fmt_atrs $ doc_after) $ fmt_cmts_after) )
  @@
  match cf.pcf_desc with
  | Pcf_inherit (override, cl, parent) ->
      hovbox 2
        ( str "inherit"
        $ fmt_if (is_override override) "!"
        $ fmt "@ "
        $ ( fmt_class_expr c (sub_cl ~ctx cl)
          $ opt parent (fun p -> str " as " $ fmt_str_loc c p) ) )
  | Pcf_method (name, pv, kind) ->
      let typ, args, eq, expr = fmt_class_field_kind c ctx kind in
      hvbox 2
        ( hovbox 2
            ( hovbox 4
                (box_fun_decl_args c
                   (Params.Indent.fun_args c.conf)
                   ( box_fun_sig_args c
                       (Params.Indent.fun_type_annot c.conf)
                       ( str "method" $ virtual_or_override kind
                       $ fmt_private_virtual_flag c pv
                       $ str " " $ fmt_str_loc c name $ typ )
                   $ args ) )
            $ eq )
        $ expr )
  | Pcf_val (name, mv, kind) ->
      let typ, args, eq, expr = fmt_class_field_kind c ctx kind in
      hvbox 2
        ( hovbox 2
            ( hovbox 4
                (box_fun_decl_args c 4
                   ( box_fun_sig_args c 4
                       ( str "val" $ virtual_or_override kind
                       $ fmt_mutable_virtual_flag c mv
                       $ str " " $ fmt_str_loc c name $ typ )
                   $ args ) )
            $ eq )
        $ expr )
  | Pcf_constraint (t1, t2) ->
      fmt "constraint@ "
      $ fmt_core_type c (sub_typ ~ctx t1)
      $ str " = "
      $ fmt_core_type c (sub_typ ~ctx t2)
  | Pcf_initializer e ->
      str "initializer" $ break 1 2 $ fmt_expression c (sub_exp ~ctx e)
  | Pcf_attribute attr -> fmt_floating_attributes_and_docstrings c [attr]
  | Pcf_extension ext -> fmt_item_extension c ctx ext

and fmt_class_type_field c {ast= cf; _} =
  protect c (Ctf cf)
  @@
  let fmt_cmts_before = Cmts.Toplevel.fmt_before c cf.pctf_loc in
  let fmt_cmts_after = Cmts.Toplevel.fmt_after c cf.pctf_loc in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~is_val:true ~fit:true c cf.pctf_attributes
  in
  let fmt_atrs = fmt_item_attributes c ~pre:(Break (1, 0)) atrs in
  let ctx = Ctf cf in
  (fun k ->
    fmt_cmts_before
    $ hvbox 0 ~name:"ctf"
        ( hvbox 0 (doc_before $ hvbox 0 k $ fmt_atrs $ doc_after)
        $ fmt_cmts_after ) )
  @@
  match cf.pctf_desc with
  | Pctf_inherit ct ->
      hovbox 2 (fmt "inherit@ " $ fmt_class_type c (sub_cty ~ctx ct))
  | Pctf_method (name, pv, ty) ->
      box_fun_sig_args c 2
        ( hovbox 4
            ( str "method"
            $ fmt_private_virtual_flag c pv
            $ fmt "@ " $ fmt_str_loc c name )
        $ fmt " :@ "
        $ fmt_core_type c (sub_typ ~ctx ty) )
  | Pctf_val (name, mv, ty) ->
      box_fun_sig_args c 2
        ( hovbox 4
            ( str "val"
            $ fmt_mutable_virtual_flag c mv
            $ fmt "@ " $ fmt_str_loc c name )
        $ fmt " :@ "
        $ fmt_core_type c (sub_typ ~ctx ty) )
  | Pctf_constraint (t1, t2) ->
      fmt "constraint@ "
      $ fmt_core_type c (sub_typ ~ctx t1)
      $ str " = "
      $ fmt_core_type c (sub_typ ~ctx t2)
  | Pctf_attribute attr -> fmt_floating_attributes_and_docstrings c [attr]
  | Pctf_extension ext -> fmt_item_extension c ctx ext

and fmt_cases c ctx cs = list_fl cs (fmt_case c ctx)

and fmt_case c ctx ~first ~last case =
  let {pc_lhs; pc_guard; pc_rhs} = case in
  let xrhs = sub_exp ~ctx pc_rhs in
  (* side effects of Cmts.fmt_before before [fmt_lhs] is important *)
  let leading_cmt = Cmts.fmt_before c pc_lhs.ppat_loc in
  let xlhs = sub_pat ~ctx pc_lhs in
  let paren_lhs =
    match pc_lhs.ppat_desc with
    | Ppat_or _ when Option.is_some pc_guard -> true
    | _ -> parenze_pat xlhs
  in
  let eol =
    Option.some_if
      (Cmts.has_before c.cmts pc_rhs.pexp_loc)
      (fmt "@;<1000 0>")
  in
  let p = Params.get_cases c.conf ~ctx ~first ~last ~xbch:xrhs in
  p.leading_space $ leading_cmt
  $ p.box_all
      ( p.box_pattern_arrow
          ( hvbox 0
              ( fmt_pattern c ~pro:p.bar ~parens:paren_lhs xlhs
              $ opt pc_guard (fun g ->
                    fmt "@;<1 2>when " $ fmt_expression c (sub_exp ~ctx g) )
              )
          $ p.break_before_arrow $ str "->" $ p.break_after_arrow
          $ p.open_paren_branch )
      $ p.break_after_opening_paren
      $ hovbox 0
          ( fmt_expression ?eol c ?parens:p.expr_parens p.branch_expr
          $ p.close_paren_branch ) )

and fmt_value_description ?ext c ctx vd =
  let {pval_name= {txt; loc}; pval_type; pval_prim; pval_attributes; pval_loc}
      =
    vd
  in
  update_config_maybe_disabled c pval_loc pval_attributes
  @@ fun c ->
  let pre = if List.is_empty pval_prim then "val" else "external" in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~is_val:true c pval_attributes
  in
  let fmt_val_prim {txt= s; loc} =
    hvbox 0 @@ Cmts.fmt c loc
    @@
    if String.exists s ~f:(function ' ' | '\n' -> true | _ -> false) then
      wrap "{|" "|}" (str s)
    else wrap "\"" "\"" (str (String.escaped s))
  in
  hvbox 0
    ( doc_before
    $ box_fun_sig_args c 2
        ( str pre
        $ fmt_extension_suffix c ext
        $ str " "
        $ Cmts.fmt c loc
            (wrap_if
               (Std_longident.String_id.is_symbol txt)
               "( " " )" (str txt) )
        $ fmt_core_type c ~pro:":"
            ~box:
              (not
                 ( c.conf.fmt_opts.ocp_indent_compat.v
                 && is_arrow_or_poly pval_type ) )
            ~pro_space:true (sub_typ ~ctx pval_type)
        $ fmt_if (not (List.is_empty pval_prim)) "@ = "
        $ hvbox_if (List.length pval_prim > 1) 0
          @@ list pval_prim "@;" fmt_val_prim )
    $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs
    $ doc_after )

and fmt_tydcl_param c ctx ty =
  fmt_core_type ~tydecl_param:true c (sub_typ ~ctx ty)
  $
  (* CR layouts v1.5: When we added the syntax for layout annotations on type
     parameters to the parser, we also made it possible for people to put
     arbitrary attributes on type parameters. Previously, the parser didn't
     accept attributes at all here, though there has always been a place in
     the parse tree.

     The parser currently allows you to have either a pretty layout
     annotation or arbitrary attributes, but not both. A pretty layout
     annotation only parses if it's the only attribute, so we only print the
     pretty syntax in that case. Probably we'll change this in v1.5.

     In the case of multiple attributes, which may include layouts, they'll
     be printed as normal attributes by [fmt_core_type]. So we do nothing
     here. *)
  match ty.ptyp_attributes with
  | [] | _ :: _ :: _ -> noop
  | [attr] -> fmt_if_k (is_layout attr) (fmt "@ :@ " $ str attr.attr_name.txt)

and fmt_tydcl_params c ctx params =
  let empty, parenize =
    match params with
    | [] -> (true, false)
    | [(p, _)] -> (
        ( false
        , match p.ptyp_attributes with
          | [] | _ :: _ :: _ -> false
          | [attr] -> is_layout attr ) )
    | _ :: _ :: _ -> (false, true)
  in
  fmt_if_k (not empty)
    ( wrap_fits_breaks_if ~space:false c.conf parenize "(" ")"
        (list params (Params.comma_sep c.conf) (fun (ty, vc) ->
             fmt_variance_injectivity c vc $ fmt_tydcl_param c ctx ty ) )
    $ fmt "@ " )

and fmt_class_params c ctx params =
  let fmt_param ~first ~last (ty, vc) =
    fmt_if (first && Exposed.Left.core_type ty) " "
    $ fmt_if_k (not first) (fmt (Params.comma_sep c.conf))
    $ fmt_variance_injectivity c vc
    $ fmt_core_type c (sub_typ ~ctx ty)
    $ fmt_if (last && Exposed.Right.core_type ty) " "
  in
  fmt_if_k
    (not (List.is_empty params))
    (hvbox 0
       (wrap_fits_breaks c.conf "[" "]" (list_fl params fmt_param) $ fmt "@ ") )

and fmt_type_declaration c ?ext ?(pre = "") ?name ?(eq = "=") {ast= decl; _}
    =
  protect c (Td decl)
  @@
  let { ptype_name= {txt; loc}
      ; ptype_params
      ; ptype_cstrs
      ; ptype_kind
      ; ptype_private= priv
      ; ptype_manifest= m
      ; ptype_attributes
      ; ptype_loc } =
    decl
  in
  update_config_maybe_disabled c ptype_loc ptype_attributes
  @@ fun c ->
  let ctx = Td decl in
  let fmt_abstract_manifest = function
    | Some m ->
        str " " $ str eq $ fmt_private_flag c priv $ fmt "@ "
        $ fmt_core_type c (sub_typ ~ctx m)
    | None -> noop
  in
  let fmt_manifest = function
    | Some m ->
        str " " $ str eq $ break 1 4
        $ fmt_core_type c (sub_typ ~ctx m)
        $ str " =" $ fmt_private_flag c priv
    | None -> str " " $ str eq $ fmt_private_flag c priv
  in
  let box_manifest k =
    hvbox c.conf.fmt_opts.type_decl_indent.v
      ( str pre
      $ fmt_extension_suffix c ext
      $ str " "
      $ hvbox_if
          (not (List.is_empty ptype_params))
          0
          ( fmt_tydcl_params c ctx ptype_params
          $ Option.value_map name ~default:(str txt) ~f:(fmt_longident_loc c)
          )
      $ k )
  in
  let fmt_manifest_kind =
    match ptype_kind with
    | Ptype_abstract -> box_manifest (fmt_abstract_manifest m)
    | Ptype_variant [] -> box_manifest (fmt_manifest m) $ fmt "@ |"
    | Ptype_variant ctor_decls ->
        box_manifest (fmt_manifest m)
        $ fmt "@ "
        $ list_fl ctor_decls (fmt_constructor_declaration c ctx)
    | Ptype_record lbl_decls ->
        let p = Params.get_record_type c.conf in
        let fmt_decl ~first ~last x =
          fmt_if_k (not first) p.sep_before
          $ fmt_label_declaration c ctx x ~last
          $ fmt_if
              ( last && (not p.box_spaced)
              && Exposed.Right.label_declaration x )
              " "
          $ fmt_if_k (not last) p.sep_after
        in
        box_manifest (fmt_manifest m $ p.docked_before)
        $ p.break_before
        $ p.box_record (list_fl lbl_decls fmt_decl)
        $ p.break_after $ p.docked_after
    | Ptype_open -> box_manifest (fmt_manifest m $ str " ..")
  in
  let fmt_cstr (t1, t2, loc) =
    Cmts.fmt c loc
      (hvbox 2
         ( fmt "constraint@ "
         $ fmt_core_type c (sub_typ ~ctx t1)
         $ fmt " =@ "
         $ fmt_core_type c (sub_typ ~ctx t2) ) )
  in
  let fmt_cstrs cstrs =
    fmt_if_k
      (not (List.is_empty cstrs))
      (fmt "@ " $ hvbox 0 (list cstrs "@ " fmt_cstr))
  in
  (* Docstring cannot be placed after variant declarations *)
  let force_before =
    match ptype_kind with Ptype_variant _ -> true | _ -> false
  in
  let doc_before, doc_after, atrs =
    let fit = Tyd.is_simple decl in
    fmt_docstring_around_item ~force_before ~fit c ptype_attributes
  in
  Cmts.fmt c loc @@ Cmts.fmt c ptype_loc
  @@ hvbox 0
       ( doc_before
       $ hvbox 0
           ( hvbox c.conf.fmt_opts.type_decl_indent.v
               (fmt_manifest_kind $ fmt_cstrs ptype_cstrs)
           $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs )
       $ doc_after )

and fmt_label_declaration c ctx ?(last = false) decl =
  let {pld_mutable; pld_name; pld_type; pld_loc; pld_attributes} = decl in
  update_config_maybe_disabled c pld_loc pld_attributes
  @@ fun c ->
  let doc, atrs = doc_atrs pld_attributes in
  let cmt_after_type = Cmts.fmt_after c pld_type.ptyp_loc in
  let field_loose =
    match c.conf.fmt_opts.field_space.v with
    | `Loose -> true
    | `Tight_decl | `Tight -> false
  in
  let fmt_semicolon =
    match c.conf.fmt_opts.break_separators.v with
    | `Before -> noop
    | `After ->
        fmt_or_k last
          (fmt_if_k c.conf.fmt_opts.dock_collection_brackets.v
             (fits_breaks ~level:5 "" ";") )
          (str ";")
  in
  let is_global, atrs = split_global_flags_from_attrs atrs in
  hvbox 0
    ( Cmts.fmt_before c pld_loc
    $ hvbox
        (Params.Indent.record_docstring c.conf)
        ( hvbox 3
            ( hvbox 4
                ( hvbox 2
                    ( hovbox 2
                        ( fmt_mutable_flag ~pro:noop ~epi:(fmt "@ ") c
                            pld_mutable
                        $ fmt_if is_global "global_ "
                        $ fmt_str_loc c pld_name $ fmt_if field_loose " "
                        $ fmt ":" )
                    $ fmt "@ "
                    $ fmt_core_type c (sub_typ ~ctx pld_type)
                    $ fmt_semicolon )
                $ cmt_after_type )
            $ fmt_attributes c ~pre:(Break (1, 1)) atrs )
        $ fmt_docstring_padded c doc )
    $ Cmts.fmt_after c pld_loc )

and fmt_constructor_declaration c ctx ~first ~last:_ cstr_decl =
  let { pcd_name= {txt; loc}
      ; pcd_vars
      ; pcd_args
      ; pcd_res
      ; pcd_attributes
      ; pcd_loc } =
    cstr_decl
  in
  update_config_maybe_disabled c pcd_loc pcd_attributes
  @@ fun c ->
  let has_cmt_before = Cmts.has_before c.cmts pcd_loc in
  let sparse = Poly.( = ) c.conf.fmt_opts.type_decl.v `Sparse in
  (* Force break if comment before pcd_loc, it would interfere with an
     eventual comment placed after the previous constructor *)
  fmt_if_k (not first) (fmt_or (sparse || has_cmt_before) "@;<1000 0>" "@ ")
  $ Cmts.fmt_before ~epi:(break 1000 0) c pcd_loc
  $ hvbox ~name:"constructor_decl" 2
      ( hovbox
          (Params.Indent.constructor_docstring c.conf)
          ( hvbox 2
              ( fmt_or_k first (if_newline "| ") (str "| ")
              $ Cmts.fmt_before c loc
              $ hvbox 2
                  ( hovbox ~name:"constructor_decl_name" 2
                      ( wrap_if
                          (Std_longident.String_id.is_symbol txt)
                          "( " " )" (str txt)
                      $ Cmts.fmt_after c loc )
                  $ fmt_constructor_arguments_result c ctx pcd_vars pcd_args
                      pcd_res ) )
          $ fmt_attributes_and_docstrings c pcd_attributes )
      $ Cmts.fmt_after c pcd_loc )

and fmt_core_type_gf c ctx typ =
  let {ptyp_attributes; _} = typ in
  let is_global, _ = split_global_flags_from_attrs ptyp_attributes in
  fmt_if is_global "global_ " $ fmt_core_type c (sub_typ ~ctx typ)

and fmt_constructor_arguments ?vars c ctx ~pre = function
  | Pcstr_tuple [] -> noop
  | Pcstr_tuple typs ->
      pre $ fmt "@ " $ fmt_opt vars
      $ hvbox 0 (list typs "@ * " (fmt_core_type_gf c ctx))
  | Pcstr_record (loc, lds) ->
      let p = Params.get_record_type c.conf in
      let fmt_ld ~first ~last x =
        fmt_if_k (not first) p.sep_before
        $ fmt_label_declaration c ctx x ~last
        $ fmt_if
            (last && (not p.box_spaced) && Exposed.Right.label_declaration x)
            " "
        $ fmt_if_k (not last) p.sep_after
      in
      pre
      $ Cmts.fmt c loc ~pro:(break 1 0) ~epi:noop
        @@ wrap_k p.docked_before p.docked_after
        @@ wrap_k p.break_before p.break_after
        @@ p.box_record @@ list_fl lds fmt_ld

and fmt_constructor_arguments_result c ctx vars args res =
  let pre = fmt_or (Option.is_none res) " of" " :" in
  let before_type = match args with Pcstr_tuple [] -> ": " | _ -> "-> " in
  let fmt_type typ =
    fmt "@ " $ str before_type $ fmt_core_type c (sub_typ ~ctx typ)
  in
  let fmt_vars =
    match vars with
    | [] -> noop
    | _ ->
        hvbox 0 (list vars "@ " (fun {txt; _} -> fmt_type_var txt))
        $ fmt ".@ "
  in
  fmt_constructor_arguments c ctx ~pre ~vars:fmt_vars args $ opt res fmt_type

and fmt_type_extension ?ext c ctx
    { ptyext_attributes
    ; ptyext_params
    ; ptyext_path
    ; ptyext_constructors
    ; ptyext_private
    ; ptyext_loc } =
  let c = update_config c ptyext_attributes in
  let doc, atrs = doc_atrs ptyext_attributes in
  let fmt_ctor ctor = hvbox 0 (fmt_extension_constructor c ctx ctor) in
  Cmts.fmt c ptyext_loc
  @@ hvbox 2
       ( fmt_docstring c ~epi:(fmt "@,") doc
       $ hvbox c.conf.fmt_opts.type_decl_indent.v
           ( str "type"
           $ fmt_extension_suffix c ext
           $ str " "
           $ hvbox_if
               (not (List.is_empty ptyext_params))
               0
               (fmt_tydcl_params c ctx ptyext_params)
           $ fmt_longident_loc c ptyext_path
           $ str " +="
           $ fmt_private_flag c ptyext_private
           $ list_fl ptyext_constructors (fun ~first ~last:_ x ->
                 let bar_fits = if first then "" else "| " in
                 cbreak ~fits:("", 1, bar_fits) ~breaks:("", 0, "| ")
                 $ fmt_ctor x ) )
       $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs )

and fmt_type_exception ~pre c ctx
    {ptyexn_attributes; ptyexn_constructor; ptyexn_loc} =
  let doc1, atrs = doc_atrs ptyexn_attributes in
  let doc1 = Option.value ~default:[] doc1 in
  let {pext_attributes; _} = ptyexn_constructor in
  (* On 4.08 the doc is attached to the constructor *)
  let doc1, pext_attributes = doc_atrs ~acc:doc1 pext_attributes in
  let doc2, pext_attributes = doc_atrs pext_attributes in
  let doc_before, doc_after = fmt_docstring_around_item' c doc1 doc2 in
  let ptyexn_constructor = {ptyexn_constructor with pext_attributes} in
  Cmts.fmt c ptyexn_loc
    (hvbox 0
       ( doc_before
       $ hvbox 2 (pre $ fmt_extension_constructor c ctx ptyexn_constructor)
       $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs
       $ doc_after ) )

and fmt_extension_constructor c ctx ec =
  let {pext_name; pext_kind; pext_attributes; pext_loc} = ec in
  update_config_maybe_disabled c pext_loc pext_attributes
  @@ fun c ->
  let sep =
    match pext_kind with
    | Pext_decl (_, _, Some _) -> fmt " :@ "
    | Pext_decl (_, _, None) | Pext_rebind _ -> fmt " of@ "
  in
  Cmts.fmt c pext_loc
  @@ hvbox 4
       ( hvbox 2
           ( fmt_str_loc c pext_name
           $
           match pext_kind with
           | Pext_decl (_, (Pcstr_tuple [] | Pcstr_record (_, [])), None) ->
               noop
           | Pext_decl (_, (Pcstr_tuple [] | Pcstr_record (_, [])), Some res)
             ->
               sep $ fmt_core_type c (sub_typ ~ctx res)
           | Pext_decl (vars, args, res) ->
               fmt_constructor_arguments_result c ctx vars args res
           | Pext_rebind lid -> str " = " $ fmt_longident_loc c lid )
       $ fmt_attributes_and_docstrings c pext_attributes )

and fmt_functor_param c ctx {loc; txt= arg} =
  match arg with
  | Unit -> Cmts.fmt c loc (wrap "(" ")" (Cmts.fmt_within c loc))
  | Named (name, mt) ->
      let xmt = sub_mty ~ctx mt in
      hvbox 0
        (Cmts.fmt c loc
           (wrap "(" ")"
              (hovbox 0
                 ( hovbox 0 (fmt_str_loc_opt c name $ fmt "@ : ")
                 $ compose_module (fmt_module_type c xmt) ~f:Fn.id ) ) ) )

and fmt_module_type c ?(rec_ = false) ({ast= mty; _} as xmty) =
  let ctx = Mty mty in
  let {pmty_desc; pmty_loc; pmty_attributes} = mty in
  update_config_maybe_disabled_block c pmty_loc pmty_attributes
  @@ fun c ->
  let parens =
    parenze_mty xmty
    || match pmty_desc with Pmty_with _ when rec_ -> true | _ -> false
  in
  match pmty_desc with
  | Pmty_ident lid ->
      { empty with
        bdy= fmt_longident_loc c lid
      ; epi= Some (fmt_attributes c pmty_attributes ~pre:(Break (1, 0))) }
  | Pmty_signature s ->
      let empty = List.is_empty s && not (Cmts.has_within c.cmts pmty_loc) in
      let before = Cmts.fmt_before c pmty_loc in
      let within = Cmts.fmt_within c ~pro:noop pmty_loc in
      let after = Cmts.fmt_after c pmty_loc in
      { opn= None
      ; pro= Some (before $ str "sig" $ fmt_if empty " ")
      ; psp= fmt_if (not empty) "@;<1000 2>"
      ; bdy= (within $ if empty then noop else fmt_signature c ctx s)
      ; cls= noop
      ; esp= fmt_if (not empty) "@;<1000 0>"
      ; epi=
          Some
            ( str "end" $ after
            $ fmt_attributes_and_docstrings c pmty_attributes ) }
  | Pmty_functor (args, mt) ->
      let blk = fmt_module_type c (sub_mty ~ctx mt) in
      { blk with
        pro=
          Some
            ( Cmts.fmt_before c pmty_loc
            $ str "functor"
            $ fmt_attributes c ~pre:Blank pmty_attributes
            $ fmt "@;<1 2>"
            $ list args "@;<1 2>" (fmt_functor_param c ctx)
            $ fmt "@;<1 2>->"
            $ opt blk.pro (fun pro -> str " " $ pro) )
      ; epi= Some (fmt_opt blk.epi $ Cmts.fmt_after c pmty_loc)
      ; psp=
          fmt_or_k (Option.is_none blk.pro)
            (fits_breaks " " ~hint:(1, 2) "")
            blk.psp }
  | Pmty_gen (gen_loc, mt) ->
      let blk = fmt_module_type c (sub_mty ~ctx mt) in
      { blk with
        pro=
          Some
            ( Cmts.fmt_before c pmty_loc
            $ Cmts.fmt c gen_loc (wrap "(" ")" (Cmts.fmt_within c gen_loc))
            $ fmt "@;<1 2>->"
            $ opt blk.pro (fun pro -> str " " $ pro) )
      ; epi= Some (fmt_opt blk.epi $ Cmts.fmt_after c pmty_loc)
      ; psp=
          fmt_or_k (Option.is_none blk.pro)
            (fits_breaks " " ~hint:(1, 2) "")
            blk.psp }
  | Pmty_with _ ->
      let wcs, mt = Sugar.mod_with (sub_mty ~ctx mty) in
      let fmt_cstr ~first ~last:_ wc =
        let pre = if first then "with" else " and" in
        fmt_or first "@ " "@," $ fmt_with_constraint c ctx ~pre wc
      in
      let fmt_cstrs ~first:_ ~last:_ (wcs_and, loc, attr) =
        Cmts.fmt c loc
          ( list_fl wcs_and fmt_cstr
          $ fmt_attributes c ~pre:(Break (1, -1)) attr )
      in
      let {pro; psp; bdy; esp; epi; opn= _; cls= _} = fmt_module_type c mt in
      { empty with
        pro=
          Option.map pro ~f:(fun pro ->
              open_hvbox 0 $ fmt_if parens "(" $ pro )
      ; psp
      ; bdy=
          fmt_if_k (Option.is_none pro)
            (open_hvbox (Params.Indent.mty_with c.conf) $ fmt_if parens "(")
          $ hvbox 0 bdy
          $ fmt_if_k (Option.is_some epi) esp
          $ fmt_opt epi $ list_fl wcs fmt_cstrs $ fmt_if parens ")"
          $ close_box
      ; esp= fmt_if_k (Option.is_none epi) esp
      ; epi= Some (Cmts.fmt_after c pmty_loc) }
  | Pmty_typeof me -> (
      let blk = fmt_module_expr c (sub_mod ~ctx me) in
      let epi =
        fmt_opt blk.epi $ Cmts.fmt_after c pmty_loc $ fmt_if parens ")"
        $ fmt_attributes c pmty_attributes ~pre:(Break (1, 0))
      in
      match blk.pro with
      | Some pro ->
          { blk with
            pro=
              Some
                ( Cmts.fmt_before c pmty_loc
                $ fmt_if parens "(" $ str "module type of " $ pro )
          ; epi= Some epi }
      | _ ->
          { blk with
            bdy=
              Cmts.fmt c pmty_loc
              @@ hvbox 2
                   (fmt_if parens "(" $ fmt "module type of@ " $ blk.bdy)
          ; epi= Some epi } )
  | Pmty_extension ext ->
      { empty with
        bdy= fmt_extension c ctx ext
      ; epi= Some (fmt_attributes c pmty_attributes ~pre:(Break (1, 0))) }
  | Pmty_alias lid ->
      { empty with
        bdy= fmt_longident_loc c lid
      ; epi= Some (fmt_attributes c pmty_attributes ~pre:(Break (1, 0))) }

and fmt_signature c ctx itms =
  let update_config c i =
    match i.psig_desc with
    | Psig_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let fmt_item c ctx ~prev:_ ~next:_ i =
    fmt_signature_item c (sub_sig ~ctx i)
  in
  let ast x = Sig x in
  fmt_item_list c ctx update_config ast fmt_item itms

and fmt_signature_item c ?ext {ast= si; _} =
  protect c (Sig si)
  @@
  let fmt_cmts_before = Cmts.Toplevel.fmt_before c si.psig_loc in
  let fmt_cmts_after = Cmts.Toplevel.fmt_after c si.psig_loc in
  (fun k -> fmt_cmts_before $ hvbox 0 (k $ fmt_cmts_after))
  @@
  let ctx = Sig si in
  match si.psig_desc with
  | Psig_attribute attr -> fmt_floating_attributes_and_docstrings c [attr]
  | Psig_exception exc ->
      let pre = str "exception" $ fmt_extension_suffix c ext $ fmt "@ " in
      hvbox 2 (fmt_type_exception ~pre c ctx exc)
  | Psig_extension (ext, atrs) ->
      let doc_before, doc_after, atrs = fmt_docstring_around_item c atrs in
      let box =
        match snd ext with
        | PTyp _ | PPat _ | PStr [_] | PSig [_] -> true
        | PStr _ | PSig _ -> false
      in
      hvbox_if box c.conf.fmt_opts.stritem_extension_indent.v
        ( doc_before
        $ hvbox_if (not box) 0 (fmt_item_extension c ctx ext)
        $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs
        $ doc_after )
  | Psig_include {pincl_mod; pincl_attributes; pincl_loc} ->
      let pincl_attributes, isfunctor =
        check_include_functor_attr pincl_attributes
      in
      update_config_maybe_disabled c pincl_loc pincl_attributes
      @@ fun c ->
      let doc_before, doc_after, atrs =
        let force_before = not (Mty.is_simple pincl_mod) in
        fmt_docstring_around_item c ~force_before ~fit:true pincl_attributes
      in
      let keyword, ({pro; psp; bdy; esp; epi; _} as blk) =
        let incl =
          if isfunctor then fmt "include@ functor" else str "include"
        in
        let kwd = incl $ fmt_extension_suffix c ext in
        match pincl_mod with
        | {pmty_desc= Pmty_typeof me; pmty_loc; pmty_attributes= _} ->
            ( kwd
              $ Cmts.fmt c ~pro:(str " ") ~epi:noop pmty_loc
                  (fmt "@ module type of")
            , fmt_module_expr c (sub_mod ~ctx me) )
        | _ -> (kwd, fmt_module_type c (sub_mty ~ctx pincl_mod))
      in
      let box = blk_box blk in
      hvbox 0
        ( doc_before
        $ hvbox 0
            ( box
                ( hvbox 2 (keyword $ opt pro (fun pro -> str " " $ pro))
                $ fmt_or_k (Option.is_some pro) psp (fmt "@;<1 2>")
                $ bdy )
            $ esp $ fmt_opt epi
            $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs )
        $ doc_after )
  | Psig_modtype mtd -> fmt_module_type_declaration ?ext c ctx mtd
  | Psig_modtypesubst mtd ->
      fmt_module_type_declaration ?ext ~eqty:":=" c ctx mtd
  | Psig_module md ->
      hvbox 0
        (fmt_module_declaration ?ext c ~rec_flag:false ~first:true
           (sub_md ~ctx md) )
  | Psig_modsubst ms -> hvbox 0 (fmt_module_substitution ?ext c ctx ms)
  | Psig_open od -> fmt_open_description ?ext c ~kw_attributes:[] od
  | Psig_recmodule mds ->
      fmt_recmodule c ctx mds
        (fmt_module_declaration ?ext)
        (fun x -> Md x)
        sub_md
  | Psig_type (rec_flag, decls) -> fmt_type c ?ext rec_flag decls ctx
  | Psig_typext te -> fmt_type_extension ?ext c ctx te
  | Psig_value vd -> fmt_value_description ?ext c ctx vd
  | Psig_class cl -> fmt_class_types ?ext c ctx ~pre:"class" ~sep:":" cl
  | Psig_class_type cl ->
      fmt_class_types ?ext c ctx ~pre:"class type" ~sep:"=" cl
  | Psig_typesubst decls -> fmt_type c ?ext ~eq:":=" Recursive decls ctx

and fmt_class_types ?ext c ctx ~pre ~sep cls =
  list_fl cls (fun ~first ~last:_ cl ->
      update_config_maybe_disabled c cl.pci_loc cl.pci_attributes
      @@ fun c ->
      let doc_before, doc_after, atrs =
        let force_before = not (Cty.is_simple cl.pci_expr) in
        fmt_docstring_around_item ~force_before c cl.pci_attributes
      in
      let class_types =
        let pro =
          hovbox 2
            ( str (if first then pre else "and")
            $ fmt_if_k first (fmt_extension_suffix c ext)
            $ fmt_virtual_flag c cl.pci_virt
            $ fmt "@ "
            $ fmt_class_params c ctx cl.pci_params
            $ fmt_str_loc c cl.pci_name $ fmt " " $ str sep )
          $ fmt "@ "
        in
        hovbox 2
          ( fmt_class_type c ~pro (sub_cty ~ctx cl.pci_expr)
          $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs )
      in
      fmt_if (not first) "\n@;<1000 0>"
      $ hovbox 0
        @@ Cmts.fmt c cl.pci_loc (doc_before $ class_types $ doc_after) )

and fmt_class_exprs ?ext c ctx cls =
  hvbox 0
  @@ list_fl cls (fun ~first ~last:_ cl ->
         update_config_maybe_disabled c cl.pci_loc cl.pci_attributes
         @@ fun c ->
         let xargs, xbody =
           match cl.pci_expr.pcl_attributes with
           | [] ->
               Sugar.cl_fun c.cmts ~will_keep_first_ast_node:false
                 (sub_cl ~ctx cl.pci_expr)
           | _ -> ([], sub_cl ~ctx cl.pci_expr)
         in
         let ty, e =
           match xbody.ast with
           | {pcl_desc= Pcl_constraint (e, t); _} -> (Some t, sub_cl ~ctx e)
           | _ -> (None, xbody)
         in
         let doc_before, doc_after, atrs =
           let force_before = not (Cl.is_simple cl.pci_expr) in
           fmt_docstring_around_item ~force_before c cl.pci_attributes
         in
         let class_exprs =
           let pro =
             box_fun_decl_args c 2
               ( hovbox 2
                   ( str (if first then "class" else "and")
                   $ fmt_if_k first (fmt_extension_suffix c ext)
                   $ fmt_virtual_flag c cl.pci_virt
                   $ fmt "@ "
                   $ fmt_class_params c ctx cl.pci_params
                   $ fmt_str_loc c cl.pci_name )
               $ fmt_if (not (List.is_empty xargs)) "@ "
               $ wrap_fun_decl_args c (fmt_fun_args c xargs) )
           in
           let intro =
             match ty with
             | Some ty ->
                 let pro = pro $ fmt " :@ " in
                 fmt_class_type c ~pro (sub_cty ~ctx ty)
             | None -> pro
           in
           hovbox 2
             (hovbox 2 (intro $ fmt "@ =") $ fmt "@;" $ fmt_class_expr c e)
           $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs
         in
         fmt_if (not first) "\n@;<1000 0>"
         $ hovbox 0
           @@ Cmts.fmt c cl.pci_loc (doc_before $ class_exprs $ doc_after) )

and fmt_module c ctx ?rec_ ?ext ?epi ?(can_sparse = false) keyword
    ?(eqty = "=") name xargs xbody xmty attributes ~rec_flag =
  let blk_t =
    Option.value_map xmty ~default:empty ~f:(fun xmty ->
        let blk = fmt_module_type ?rec_ c xmty in
        { blk with
          pro=
            Some (str " " $ str eqty $ opt blk.pro (fun pro -> str " " $ pro))
        ; psp= fmt_if (Option.is_none blk.pro) "@;<1 2>" $ blk.psp } )
  in
  let blk_b = Option.value_map xbody ~default:empty ~f:(fmt_module_expr c) in
  let args_p = Params.Mod.get_args c.conf xargs in
  let fmt_name_and_mt ~pro ~loc name mt =
    let xmt = sub_mty ~ctx mt in
    let blk = fmt_module_type c ?rec_ xmt in
    let align_opn, align_cls =
      if args_p.arg_align then (open_hvbox 0, close_box) else (noop, noop)
    in
    let pro =
      pro $ Cmts.fmt_before c loc $ str "(" $ align_opn
      $ fmt_str_loc_opt c name $ str " :"
      $ fmt_or_k (Option.is_some blk.pro) (str " ")
          (break 1 (Params.Indent.mty c.conf))
    and epi = str ")" $ Cmts.fmt_after c loc $ align_cls in
    compose_module' ~box:false ~pro ~epi blk
  in
  (* Carry the [epi] to be placed in the next argument's box. *)
  let fmt_arg ~pro {loc; txt} =
    let pro = pro $ args_p.arg_psp in
    match txt with
    | Unit ->
        (pro $ Cmts.fmt c loc (wrap "(" ")" (Cmts.fmt_within c loc)), noop)
    | Named (name, mt) ->
        if args_p.dock then
          (* All signatures, put the [epi] into the box of the next arg and
             don't break. *)
          fmt_name_and_mt ~pro ~loc name mt
        else
          let bdy, epi = fmt_name_and_mt ~pro:noop ~loc name mt in
          let bdy_indent = if args_p.arg_align then 1 else 0 in
          (pro $ hvbox bdy_indent bdy $ epi, noop)
  in
  let rec fmt_args ~pro = function
    | [] -> pro
    | hd :: tl ->
        let bdy, epi = fmt_arg ~pro hd in
        bdy $ fmt_args ~pro:epi tl
  in
  let intro =
    str keyword
    $ fmt_extension_suffix c ext
    $ fmt_if rec_flag " rec" $ str " " $ fmt_str_loc_opt c name
  in
  let single_line =
    Option.for_all xbody ~f:(fun x -> Mod.is_simple x.ast)
    && Option.for_all xmty ~f:(fun x -> Mty.is_simple x.ast)
    && List.for_all xargs ~f:(function {txt= Unit; _} -> true | _ -> false)
  in
  let compact =
    Poly.(c.conf.fmt_opts.let_module.v = `Compact) || not can_sparse
  in
  let fmt_pro = opt blk_b.pro (fun pro -> fmt "@ " $ pro) in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item c ~force_before:(not single_line) ~fit:true
      attributes
  in
  hvbox
    (if compact then 0 else 2)
    ( doc_before
    $ blk_box blk_b
        ( (if Option.is_some blk_t.epi then hovbox else hvbox)
            0
            ( blk_box blk_t
                ( hvbox args_p.indent
                    (fmt_args ~pro:intro xargs $ fmt_opt blk_t.pro)
                $ blk_t.psp $ blk_t.bdy )
            $ blk_t.esp $ fmt_opt blk_t.epi
            $ fmt_if (Option.is_some xbody) " ="
            $ fmt_if_k compact fmt_pro )
        $ fmt_if_k (not compact) fmt_pro
        $ blk_b.psp
        $ fmt_if (Option.is_none blk_b.pro && Option.is_some xbody) "@ "
        $ blk_b.bdy )
    $ blk_b.esp $ fmt_opt blk_b.epi
    $ fmt_item_attributes c ~pre:(Break (1, 0)) atrs
    $ doc_after
    $ opt epi (fun epi ->
          fmt_or_k compact
            (fmt_or
               ( Option.is_some blk_b.epi
               && not c.conf.fmt_opts.ocp_indent_compat.v )
               " " "@ " )
            (fmt "@;<1 -2>")
          $ epi ) )

and fmt_module_declaration ?ext c ~rec_flag ~first {ast= pmd; _} =
  protect c (Md pmd)
  @@
  let {pmd_name; pmd_args; pmd_type; pmd_attributes; pmd_loc} = pmd in
  update_config_maybe_disabled c pmd_loc pmd_attributes
  @@ fun c ->
  let ctx = Md pmd in
  let ext = if first then ext else None in
  let keyword = if first then "module" else "and" in
  let xmty = sub_mty ~ctx pmd_type in
  let eqty =
    match xmty.ast.pmty_desc with Pmty_alias _ -> None | _ -> Some ":"
  in
  Cmts.fmt c pmd_loc
    (fmt_module ~rec_:rec_flag ?ext c ctx keyword pmd_name pmd_args None
       ?eqty (Some xmty) ~rec_flag:(rec_flag && first) pmd_attributes )

and fmt_module_substitution ?ext c ctx pms =
  let {pms_name; pms_manifest; pms_attributes; pms_loc} = pms in
  update_config_maybe_disabled c pms_loc pms_attributes
  @@ fun c ->
  let xmty =
    (* TODO: improve *)
    sub_mty ~ctx
      { pmty_desc= Pmty_ident pms_manifest
      ; pmty_loc= pms_loc
      ; pmty_attributes= [] }
  in
  let pms_name = {pms_name with txt= Some pms_name.txt} in
  Cmts.fmt c pms_loc
    (fmt_module ?ext c ctx "module" ~eqty:":=" pms_name [] None (Some xmty)
       pms_attributes ~rec_flag:false )

and fmt_module_type_declaration ?ext ?eqty c ctx pmtd =
  let {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} = pmtd in
  update_config_maybe_disabled c pmtd_loc pmtd_attributes
  @@ fun c ->
  let pmtd_name = {pmtd_name with txt= Some pmtd_name.txt} in
  fmt_module ?ext ?eqty c ctx "module type" pmtd_name [] None ~rec_flag:false
    (Option.map pmtd_type ~f:(sub_mty ~ctx))
    pmtd_attributes

and fmt_open_description ?ext c ?(keyword = "open") ~kw_attributes
    {popen_expr= popen_lid; popen_override; popen_attributes; popen_loc} =
  update_config_maybe_disabled c popen_loc popen_attributes
  @@ fun c ->
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~fit:true c popen_attributes
  in
  let keyword =
    fmt_or_k
      (is_override popen_override)
      ( str keyword $ str "!"
      $ opt ext (fun _ -> str " " $ fmt_extension_suffix c ext) )
      (str keyword $ fmt_extension_suffix c ext)
  in
  hovbox 0
    ( doc_before $ keyword
    $ Cmts.fmt c popen_loc
        ( fmt_attributes c kw_attributes
        $ str " "
        $ fmt_longident_loc c popen_lid
        $ fmt_item_attributes c ~pre:Blank atrs )
    $ doc_after )

(** TODO: merge with `fmt_module_declaration` *)
and fmt_module_statement c ~attributes ?keyword mod_expr =
  let blk = fmt_module_expr c mod_expr in
  let force_before = not (Mod.is_simple mod_expr.ast) in
  let doc_before, doc_after, atrs =
    fmt_docstring_around_item ~force_before ~fit:true c attributes
  in
  let has_kwd = Option.is_some keyword in
  let kwd_and_pro = Option.is_some blk.pro && has_kwd in
  doc_before
  $ blk_box blk
      (hvbox_if (Option.is_none blk.pro) 2
         ( hvbox_if kwd_and_pro 2 (fmt_opt keyword $ fmt_opt blk.pro)
         $ blk.psp $ blk.bdy ) )
  $ blk.esp $ fmt_opt blk.epi
  $ fmt_item_attributes c ~pre:Blank atrs
  $ doc_after

and fmt_with_constraint c ctx ~pre = function
  | Pwith_type (lid, td) ->
      fmt_type_declaration ~pre:(pre ^ " type") c ~name:lid (sub_td ~ctx td)
  | Pwith_module (m1, m2) ->
      str pre $ str " module " $ fmt_longident_loc c m1 $ str " = "
      $ fmt_longident_loc c m2
  | Pwith_typesubst (lid, td) ->
      fmt_type_declaration ~pre:(pre ^ " type") c ~eq:":=" ~name:lid
        (sub_td ~ctx td)
  | Pwith_modsubst (m1, m2) ->
      str pre $ str " module " $ fmt_longident_loc c m1 $ str " := "
      $ fmt_longident_loc c m2
  | Pwith_modtype (m1, m2) ->
      let m1 = {m1 with txt= Some (str_longident m1.txt)} in
      let m2 = Some (sub_mty ~ctx m2) in
      str pre $ break 1 2
      $ fmt_module c ctx "module type" m1 [] None ~rec_flag:false m2 []
  | Pwith_modtypesubst (m1, m2) ->
      let m1 = {m1 with txt= Some (str_longident m1.txt)} in
      let m2 = Some (sub_mty ~ctx m2) in
      str pre $ break 1 2
      $ fmt_module c ctx ~eqty:":=" "module type" m1 [] None ~rec_flag:false
          m2 []

and fmt_mod_apply c ctx loc attrs ~parens ~dock_struct me_f arg =
  match me_f.pmod_desc with
  | Pmod_ident _ -> (
    match arg with
    | `Unit x ->
        { empty with
          bdy=
            Cmts.fmt c loc
              ( hvbox 2
                  ( compose_module
                      (fmt_module_expr c (sub_mod ~ctx me_f))
                      ~f:Fn.id
                  $ break 1 0 $ x )
              $ fmt_attributes_and_docstrings c attrs ) }
    | `Block (blk_a, arg_is_simple) ->
        let ocp_indent_compat blk =
          if c.conf.fmt_opts.ocp_indent_compat.v && not arg_is_simple then
            (* Indent body of docked struct. *)
            { blk with
              opn= Some (open_hvbox 2)
            ; psp= fmt "@;<1000 2>"
            ; cls= close_box
            ; bdy= blk.bdy $ blk.esp $ fmt_opt blk.epi
            ; esp= noop
            ; epi= None }
          else blk
        in
        let fmt_rator =
          let break_struct =
            c.conf.fmt_opts.break_struct.v && (not dock_struct)
            && not arg_is_simple
          in
          compose_module (fmt_module_expr c (sub_mod ~ctx me_f)) ~f:Fn.id
          $ break (if break_struct then 1000 else 1) 0
          $ str "("
        in
        let epi =
          fmt_opt blk_a.epi $ str ")"
          $ fmt_attributes_and_docstrings c attrs
          $ Cmts.fmt_after c loc
        in
        if Option.is_some blk_a.pro then
          ocp_indent_compat
            { blk_a with
              pro=
                Some
                  ( Cmts.fmt_before c loc $ hvbox 2 fmt_rator
                  $ fmt_opt blk_a.pro )
            ; epi= Some epi }
        else
          { blk_a with
            opn= Some (open_hvbox 2 $ fmt_opt blk_a.opn)
          ; bdy= Cmts.fmt_before c loc $ open_hvbox 2 $ fmt_rator $ blk_a.bdy
          ; cls= close_box $ blk_a.cls $ close_box
          ; epi= Some epi } )
  | _ ->
      let blk_f = fmt_module_expr ~dock_struct:false c (sub_mod ~ctx me_f) in
      let has_epi = Cmts.has_after c.cmts loc || not (List.is_empty attrs) in
      { empty with
        opn= Some (fmt_opt blk_f.opn $ open_hvbox 2)
      ; bdy=
          hvbox 2
            ( Cmts.fmt_before c loc
            $ wrap_if parens "(" ")"
                (fmt_opt blk_f.pro $ blk_f.psp $ blk_f.bdy $ blk_f.esp)
            $ fmt_opt blk_f.epi $ break 1 0
            $
            match arg with
            | `Unit x -> x
            | `Block (x, _) -> wrap "(" ")" (compose_module x ~f:Fn.id) )
      ; cls= close_box $ blk_f.cls
      ; epi=
          Option.some_if has_epi
            (Cmts.fmt_after c loc $ fmt_attributes_and_docstrings c attrs) }

and fmt_module_expr ?(dock_struct = true) c ({ast= m; _} as xmod) =
  let ctx = Mod m in
  let {pmod_desc; pmod_loc; pmod_attributes} = m in
  update_config_maybe_disabled_block c pmod_loc pmod_attributes
  @@ fun c ->
  let parens = parenze_mod xmod in
  match pmod_desc with
  | Pmod_apply_unit (me, loc) ->
      let arg =
        Cmts.fmt c loc @@ hvbox 0 @@ wrap "(" ")" @@ Cmts.fmt_within c loc
      in
      fmt_mod_apply c ctx ~parens ~dock_struct pmod_loc pmod_attributes me
        (`Unit arg)
  | Pmod_apply (me_f, me_a) ->
      let dock_struct =
        match me_f.pmod_desc with
        | Pmod_apply _ -> false
        | Pmod_ident _ -> dock_struct
        | _ -> true
      in
      let blk_a = fmt_module_expr c (sub_mod ~ctx me_a) in
      fmt_mod_apply c ctx ~parens ~dock_struct pmod_loc pmod_attributes me_f
        (`Block (blk_a, Mod.is_simple me_a))
  | Pmod_constraint (me, mt) ->
      let blk_e = fmt_module_expr c (sub_mod ~ctx me) in
      let blk_t = fmt_module_type c (sub_mty ~ctx mt) in
      let has_epi =
        Cmts.has_after c.cmts pmod_loc || not (List.is_empty pmod_attributes)
      in
      { opn=
          Some
            ( fmt_opt blk_t.opn $ fmt_opt blk_e.opn
            $ open_hovbox (Params.Indent.mod_constraint c.conf ~lhs:me) )
      ; pro= Some (Cmts.fmt_before c pmod_loc $ str "(")
      ; psp= fmt "@,"
      ; bdy=
          hvbox 0
            ( fmt_opt blk_e.pro $ blk_e.psp $ blk_e.bdy $ blk_e.esp
            $ fmt_opt blk_e.epi $ fmt " :"
            $ Params.Mod.break_constraint c.conf ~rhs:mt
            $ hvbox 0
                ( fmt_opt blk_t.pro $ blk_t.psp $ blk_t.bdy $ blk_t.esp
                $ fmt_opt blk_t.epi ) )
          $ closing_paren c ~offset:(-2)
      ; cls= close_box $ blk_e.cls $ blk_t.cls
      ; esp= noop
      ; epi=
          Option.some_if has_epi
            ( Cmts.fmt_after c pmod_loc
            $ fmt_attributes_and_docstrings c pmod_attributes ) }
  | Pmod_functor (args, me) ->
      let doc, atrs = doc_atrs pmod_attributes in
      { empty with
        bdy=
          Cmts.fmt c pmod_loc
            ( fmt_docstring c ~epi:(fmt "@,") doc
            $ hvbox 0
                (wrap_if parens "(" ")"
                   ( str "functor"
                   $ fmt_attributes c ~pre:Blank atrs
                   $ fmt "@;<1 2>"
                   $ list args "@;<1 2>" (fmt_functor_param c ctx)
                   $ fmt "@;<1 2>->@;<1 2>"
                   $ compose_module
                       (fmt_module_expr c (sub_mod ~ctx me))
                       ~f:(hvbox 0) ) ) ) }
  | Pmod_ident lid ->
      { empty with
        opn= Some (open_hvbox 2)
      ; bdy=
          Cmts.fmt c pmod_loc
            ( fmt_longident_loc c lid
            $ fmt_attributes_and_docstrings c pmod_attributes )
      ; cls= close_box }
  | Pmod_structure sis ->
      let empty =
        List.is_empty sis && not (Cmts.has_within c.cmts pmod_loc)
      in
      let before = Cmts.fmt_before c pmod_loc in
      let within = Cmts.fmt_within c ~pro:noop pmod_loc in
      let after = Cmts.fmt_after c pmod_loc in
      { opn= None
      ; pro= Some (before $ str "struct" $ fmt_if empty " ")
      ; psp=
          fmt_if_k (not empty)
            (fmt_or c.conf.fmt_opts.break_struct.v "@;<1000 2>" "@;<1 2>")
      ; bdy= within $ fmt_structure c ctx sis
      ; cls= noop
      ; esp=
          fmt_if_k (not empty)
            (fmt_or c.conf.fmt_opts.break_struct.v "@;<1000 0>" "@;<1 0>")
      ; epi=
          Some
            ( hovbox_if (not empty) 0
                (str "end" $ fmt_attributes_and_docstrings c pmod_attributes)
            $ after ) }
  | Pmod_unpack (e, ty1, ty2) ->
      let package_type sep (lid, cstrs) =
        break 1 (Params.Indent.mod_unpack_annot c.conf)
        $ hvbox 0
            ( hovbox 0 (str sep $ fmt_longident_loc c lid)
            $ fmt_package_type c ctx cstrs )
      in
      { empty with
        opn= Some (open_hvbox 2)
      ; cls= close_box
      ; bdy=
          Cmts.fmt c pmod_loc
            ( hvbox 2
                (wrap_fits_breaks ~space:false c.conf "(" ")"
                   ( str "val "
                   $ fmt_expression c (sub_exp ~ctx e)
                   $ opt ty1 (package_type ": ")
                   $ opt ty2 (package_type ":> ") ) )
            $ fmt_attributes_and_docstrings c pmod_attributes ) }
  | Pmod_extension x1 ->
      { empty with
        opn= Some (open_hvbox 2)
      ; cls= close_box
      ; bdy=
          Cmts.fmt c pmod_loc
            ( fmt_extension c ctx x1
            $ fmt_attributes_and_docstrings c pmod_attributes ) }
  | Pmod_hole ->
      { empty with
        opn= Some (open_hvbox 2)
      ; cls= close_box
      ; bdy=
          Cmts.fmt c pmod_loc
            (fmt_hole () $ fmt_attributes_and_docstrings c pmod_attributes)
      }

and fmt_structure c ctx itms =
  let update_config c i =
    match i.pstr_desc with
    | Pstr_attribute atr -> update_config c [atr]
    | _ -> c
  in
  let fmt_item c ctx ~prev:_ ~next i =
    let semisemi =
      match next with
      | Some ({pstr_desc= Pstr_eval _; _}, _) -> true
      | _ -> false
    in
    fmt_structure_item c ~last:(Option.is_none next) ~semisemi
      (sub_str ~ctx i)
  in
  let ast x = Str x in
  fmt_item_list c ctx update_config ast fmt_item itms

and fmt_type c ?ext ?eq rec_flag decls ctx =
  let update_config c td = update_config c td.ptype_attributes in
  let is_rec = Asttypes.is_recursive rec_flag in
  let fmt_decl c ctx ~prev ~next:_ decl =
    let first = Option.is_none prev in
    let pre =
      if first then if is_rec then "type" else "type nonrec" else "and"
    in
    let ext = if first then ext else None in
    fmt_type_declaration c ~pre ?eq ?ext (sub_td ~ctx decl)
  in
  let ast x = Td x in
  fmt_item_list c ctx update_config ast fmt_decl decls

and fmt_structure_item c ~last:last_item ?ext ~semisemi
    {ctx= parent_ctx; ast= si} =
  protect c (Str si)
  @@
  let ctx = Str si in
  let fmt_cmts_before = Cmts.Toplevel.fmt_before c si.pstr_loc in
  let fmt_cmts_after = Cmts.Toplevel.fmt_after c si.pstr_loc in
  (fun k ->
    fmt_cmts_before
    $ hvbox 0 ~name:"stri"
        (box_semisemi c ~parent_ctx semisemi (k $ fmt_cmts_after)) )
  @@
  match si.pstr_desc with
  | Pstr_attribute attr -> fmt_floating_attributes_and_docstrings c [attr]
  | Pstr_eval (exp, atrs) ->
      let doc, atrs = doc_atrs atrs in
      fmt_docstring c doc
      $ cbox 0 ~name:"eval" (fmt_expression c (sub_exp ~ctx exp))
      $ fmt_item_attributes c ~pre:Space atrs
  | Pstr_exception extn_constr ->
      let pre = str "exception" $ fmt_extension_suffix c ext $ fmt "@ " in
      hvbox 2 ~name:"exn" (fmt_type_exception ~pre c ctx extn_constr)
  | Pstr_include {pincl_mod; pincl_attributes= attributes; pincl_loc} ->
      let attributes, isfunctor = check_include_functor_attr attributes in
      update_config_maybe_disabled c pincl_loc attributes
      @@ fun c ->
      let incl =
        if isfunctor then fmt "include@ functor" else str "include"
      in
      let keyword = incl $ fmt_extension_suffix c ext $ fmt "@ " in
      fmt_module_statement c ~attributes ~keyword (sub_mod ~ctx pincl_mod)
  | Pstr_module mb ->
      fmt_module_binding ?ext c ~rec_flag:false ~first:true (sub_mb ~ctx mb)
  | Pstr_open
      {popen_expr; popen_override; popen_attributes= attributes; popen_loc}
    ->
      update_config_maybe_disabled c popen_loc attributes
      @@ fun c ->
      let keyword =
        fmt_or_k
          (is_override popen_override)
          ( str "open!"
          $ opt ext (fun _ -> str " " $ fmt_extension_suffix c ext) )
          (str "open" $ fmt_extension_suffix c ext)
        $ fmt "@ "
      in
      fmt_module_statement c ~attributes ~keyword (sub_mod ~ctx popen_expr)
  | Pstr_primitive vd -> fmt_value_description ?ext c ctx vd
  | Pstr_recmodule mbs ->
      fmt_recmodule c ctx mbs (fmt_module_binding ?ext)
        (fun x -> Mb x)
        sub_mb
  | Pstr_type (rec_flag, decls) -> fmt_type c ?ext rec_flag decls ctx
  | Pstr_typext te -> fmt_type_extension ?ext c ctx te
  | Pstr_value {pvbs_rec= rec_flag; pvbs_bindings= bindings; pvbs_extension}
    ->
      let update_config c i = update_config ~quiet:true c i.pvb_attributes in
      let ast x = Lb x in
      let fmt_item c ctx ~prev ~next b =
        let first = Option.is_none prev in
        let last = Option.is_none next in
        let b = Sugar.Let_binding.of_let_binding c.cmts ~ctx ~first b in
        let epi =
          match c.conf.fmt_opts.let_binding_spacing.v with
          | `Compact -> None
          | `Sparse when last && last_item -> None
          | `Sparse -> Some (fits_breaks "" "\n")
          | `Double_semicolon ->
              Option.some_if (last && not semisemi)
                (fits_breaks "" ~hint:(1000, 0) ";;")
        in
        let rec_flag = first && Asttypes.is_recursive rec_flag in
        let ext = if first then pvbs_extension else None in
        fmt_value_binding c ~rec_flag ?ext ?epi b
      in
      fmt_item_list c ctx update_config ast fmt_item bindings
  | Pstr_modtype mtd -> fmt_module_type_declaration ?ext c ctx mtd
  | Pstr_extension (ext, atrs) ->
      let doc_before, doc_after, atrs = fmt_docstring_around_item c atrs in
      let box =
        match snd ext with
        | PTyp _ | PPat _ | PStr [_] | PSig [_] -> true
        | PStr _ | PSig _ -> false
      in
      hvbox_if box c.conf.fmt_opts.stritem_extension_indent.v ~name:"ext1"
        ( doc_before
        $ hvbox_if (not box) 0 ~name:"ext2" (fmt_item_extension c ctx ext)
        $ fmt_item_attributes c ~pre:Space atrs
        $ doc_after )
  | Pstr_class_type cl ->
      fmt_class_types ?ext c ctx ~pre:"class type" ~sep:"=" cl
  | Pstr_class cls -> fmt_class_exprs ?ext c ctx cls

and fmt_let c ~ext ~rec_flag ~bindings ~parens ~fmt_atrs ~fmt_expr ~body_loc
    ~has_attr ~indent_after_in =
  let parens = parens || has_attr in
  let fmt_in indent =
    match c.conf.fmt_opts.break_before_in.v with
    | `Fit_or_vertical -> break 1 (-indent) $ str "in"
    | `Auto -> fits_breaks " in" ~hint:(1, -indent) "in"
  in
  let fmt_binding ~first ~last binding =
    let ext = if first then ext else None in
    let in_ indent = fmt_if_k last (fmt_in indent) in
    let rec_flag = first && Asttypes.is_recursive rec_flag in
    fmt_value_binding c ~rec_flag ?ext ~in_ binding
    $ fmt_if (not last)
        ( match c.conf.fmt_opts.let_and.v with
        | `Sparse -> "@;<1000 0>"
        | `Compact -> "@ " )
  in
  let blank_line_after_in =
    let last_bind = List.last_exn bindings in
    sequence_blank_line c last_bind.lb_loc body_loc
  in
  Params.Exp.wrap c.conf ~parens:(parens || has_attr) ~fits_breaks:false
    (vbox 0
       ( hvbox 0 (list_fl bindings fmt_binding)
       $ ( if blank_line_after_in then fmt "\n@,"
           else break 1000 indent_after_in )
       $ hvbox 0 fmt_expr ) )
  $ fmt_atrs

and fmt_value_binding c ~rec_flag ?ext ?in_ ?epi
    {lb_op; lb_pat; lb_args; lb_typ; lb_exp; lb_attrs; lb_local; lb_loc; lb_pun} =
  update_config_maybe_disabled c lb_loc lb_attrs
  @@ fun c ->
  let lb_pun =
    Ocaml_version.(
      compare c.conf.opr_opts.ocaml_version.v Releases.v4_13_0 >= 0 )
    && lb_pun
  in
  let doc1, atrs = doc_atrs lb_attrs in
  let doc2, atrs = doc_atrs atrs in
  let fmt_cstr =
    let fmt_sep x =
      match c.conf.fmt_opts.break_colon.v with
      | `Before -> fmt "@ " $ str x $ char ' '
      | `After -> char ' ' $ str x $ fmt "@ "
    in
    match lb_typ with
    | `Polynewtype (pvars, xtyp) ->
        fmt_sep ":"
        $ hvbox 0
            ( str "type "
            $ list pvars " " (fmt_str_loc c)
            $ fmt ".@ " $ fmt_core_type c xtyp )
    | `Coerce (xtyp1, xtyp2) ->
        opt xtyp1 (fun xtyp1 -> fmt_sep ":" $ fmt_core_type c xtyp1)
        $ fmt_sep ":>" $ fmt_core_type c xtyp2
    | `Other xtyp -> fmt_type_cstr c xtyp
    | `None -> noop
  in
  let cstr_indent =
    match lb_typ with
    | `Other {ast= {ptyp_desc= Ptyp_poly _; _}; _} -> 6
    | _ -> Params.Indent.fun_type_annot c.conf
  in
  let indent =
    match lb_exp.ast.pexp_desc with
    | Pexp_function _ -> c.conf.fmt_opts.function_indent.v
    | (Pexp_fun _ | Pexp_newtype _)
      when c.conf.fmt_opts.let_binding_deindent_fun.v ->
        max (c.conf.fmt_opts.let_binding_indent.v - 1) 0
    | _ -> c.conf.fmt_opts.let_binding_indent.v
  in
  let f {attr_name= {loc; _}; _} =
    Location.compare_start loc lb_exp.ast.pexp_loc < 1
  in
  let at_attrs, at_at_attrs = List.partition_tf atrs ~f in
  let pre_body, body = fmt_body c lb_exp in
  let pat_has_cmt = Cmts.has_before c.cmts lb_pat.ast.ppat_loc in
  let toplevel, in_, epi, cmts_before, cmts_after =
    match in_ with
    | Some in_ ->
        ( false
        , fmt_item_attributes c ~pre:(Break (1, 2)) at_at_attrs $ in_ indent
        , fmt_opt epi
        , Cmts.fmt_before c lb_loc
        , Cmts.fmt_after c lb_loc )
    | None ->
        let epi =
          fmt_item_attributes c ~pre:(Break (1, 0)) at_at_attrs $ fmt_opt epi
        in
        ( true
        , noop
        , epi
        , Cmts.Toplevel.fmt_before c lb_loc
        , Cmts.Toplevel.fmt_after c lb_loc )
  in
  let decl_args =
    let decl =
      fmt_str_loc c lb_op
      $ fmt_extension_suffix c ext
      $ fmt_attributes c at_attrs $ fmt_if rec_flag " rec"
      $ fmt_if lb_local " local_"
      $ fmt_or pat_has_cmt "@ " " "
    and pattern = fmt_pattern c lb_pat
    and args =
      fmt_if_k
        (not (List.is_empty lb_args))
        (fmt "@ " $ wrap_fun_decl_args c (fmt_fun_args c lb_args))
    in
    box_fun_decl_args c 4 (Params.Align.fun_decl c.conf ~decl ~pattern ~args)
  in
  fmt_docstring c ~epi:(fmt "@\n") doc1
  $ cmts_before
  $ hvbox 0
      ( hvbox indent
          ( hvbox_if toplevel 0
              ( hvbox_if toplevel indent
                  ( hovbox 2
                      ( hovbox cstr_indent (decl_args $ fmt_cstr)
                      $ fmt_if_k (not lb_pun)
                          (fmt_or_k c.conf.fmt_opts.ocp_indent_compat.v
                             (fits_breaks " =" ~hint:(1000, 0) "=")
                             (fmt "@;<1 2>=") )
                      $ fmt_if_k (not lb_pun) pre_body )
                  $ fmt_if (not lb_pun) "@ "
                  $ fmt_if_k (not lb_pun) body )
              $ cmts_after )
          $ in_ )
      $ epi )
  $ fmt_docstring c ~pro:(fmt "@\n") doc2

and fmt_module_binding ?ext c ~rec_flag ~first {ast= pmb; _} =
  protect c (Mb pmb)
  @@ update_config_maybe_disabled c pmb.pmb_loc pmb.pmb_attributes
  @@ fun c ->
  let ctx = Mb pmb in
  let ext = if first then ext else None in
  let keyword = if first then "module" else "and" in
  let xbody = sub_mod ~ctx pmb.pmb_expr in
  let xbody, xmty =
    match xbody.ast with
    | { pmod_desc= Pmod_constraint (body_me, body_mt)
      ; pmod_loc
      ; pmod_attributes= [] } ->
        Cmts.relocate c.cmts ~src:pmod_loc ~before:body_me.pmod_loc
          ~after:body_mt.pmty_loc ;
        (sub_mod ~ctx body_me, Some (sub_mty ~ctx body_mt))
    | _ -> (xbody, None)
  in
  Cmts.fmt c pmb.pmb_loc
    (fmt_module ~rec_:rec_flag ?ext c ctx keyword
       ~rec_flag:(rec_flag && first) ~eqty:":" pmb.pmb_name pmb.pmb_args
       (Some xbody) xmty pmb.pmb_attributes )

let fmt_toplevel_directive c ~semisemi dir =
  let fmt_dir_arg = function
    | Pdir_string s -> str (Printf.sprintf "%S" s)
    | Pdir_int (lit, Some m) -> str (Printf.sprintf "%s%c" lit m)
    | Pdir_int (lit, None) -> str lit
    | Pdir_ident longident -> fmt_longident longident
    | Pdir_bool bool -> str (Bool.to_string bool)
  in
  let {pdir_name= name; pdir_arg; pdir_loc} = dir in
  let name = fmt_str_loc c name ~pre:"#" in
  let args =
    match pdir_arg with
    | None -> noop
    | Some {pdira_desc; pdira_loc; _} ->
        str " "
        $ Cmts.fmt_before ~epi:(str " ") c pdira_loc
        $ fmt_dir_arg pdira_desc
        $ Cmts.fmt_after c pdira_loc
  in
  Cmts.fmt c pdir_loc (box_semisemi c ~parent_ctx:Top semisemi (name $ args))

let flatten_ptop =
  List.concat_map ~f:(function
    | Ptop_def items -> List.map items ~f:(fun i -> `Item i)
    | Ptop_dir d -> [`Directive d] )

let fmt_toplevel ?(force_semisemi = false) c ctx itms =
  let itms = flatten_ptop itms in
  let update_config c = function
    | `Item {pstr_desc= Pstr_attribute atr; _} -> update_config c [atr]
    | _ -> c
  in
  let fmt_item c ctx ~prev:_ ~next itm =
    let last = Option.is_none next in
    let semisemi =
      match (itm, next) with
      | _, Some (`Item {pstr_desc= Pstr_eval _; _}, _) -> true
      | `Item {pstr_desc= Pstr_attribute _; _}, _ -> false
      | `Item _, Some (`Directive _, _) -> true
      | _ -> force_semisemi && last
    in
    match itm with
    | `Item i -> fmt_structure_item c ~last ~semisemi (sub_str ~ctx i)
    | `Directive d -> fmt_toplevel_directive c ~semisemi d
  in
  let ast x = Tli x in
  fmt_item_list c ctx update_config ast fmt_item itms

let fmt_repl_phrase c ctx {prepl_phrase; prepl_output} =
  str "# "
  $ fmt_toplevel ~force_semisemi:true c ctx [prepl_phrase]
  $ fmt_if_k
      (not (String.is_empty prepl_output))
      (break 1000 0 $ str prepl_output)

let fmt_repl_file c _ itms =
  vbox 0 @@ list itms "@;<1000 0>" @@ fmt_repl_phrase c Rep

(** Entry points *)

module Chunk = struct
  open Chunk

  let fmt_item (type a) (fg : a list item) : c -> Ast.t -> a list -> Fmt.t =
    match fg with
    | Structure -> fmt_structure
    | Signature -> fmt_signature
    | Use_file -> fmt_toplevel ?force_semisemi:None

  let update_conf c state = {c with conf= Conf.update_state c.conf state}

  let fmt fg c ctx chunks =
    List.foldi chunks ~init:(c, noop, [])
      ~f:(fun i (c, output, locs) chunk ->
        let c = update_conf c chunk.state in
        let output, locs =
          match chunk.state with
          | `Disable ->
              let output =
                output
                $ Cmts.fmt_before c chunk.attr_loc ~eol:(fmt "\n@;<1000 0>")
                $ fmt_if (i > 0) "\n@;<1000 0>"
                $ str
                    (String.strip
                       (Source.string_at c.source chunk.chunk_loc) )
              in
              (output, chunk.chunk_loc :: locs)
          | `Enable ->
              let output =
                output
                $ fmt_if (i > 0) "@;<1000 0>"
                $ fmt_item fg c ctx chunk.items
              in
              (output, locs)
        in
        (c, output, locs) )
    |> fun ((_ : c), output, locs) ->
    List.iter locs ~f:(Cmts.drop_inside c.cmts) ;
    output

  let split_and_fmt fg c ctx l =
    let state = if c.conf.opr_opts.disable.v then `Disable else `Enable in
    fmt fg c ctx @@ split fg l ~state
end

let fmt_file (type a) ~ctx ~fmt_code ~debug (fragment : a Extended_ast.t)
    source cmts conf (itms : a) =
  let c = {source; cmts; conf; debug; fmt_code} in
  match (fragment, itms) with
  | Structure, [] | Signature, [] | Use_file, [] ->
      Cmts.fmt_after ~pro:noop c Location.none
  | Structure, l -> Chunk.split_and_fmt Structure c ctx l
  | Signature, l -> Chunk.split_and_fmt Signature c ctx l
  | Use_file, l -> Chunk.split_and_fmt Use_file c ctx l
  | Core_type, ty -> fmt_core_type c (sub_typ ~ctx:(Pld (PTyp ty)) ty)
  | Module_type, mty ->
      compose_module ~f:Fn.id
        (fmt_module_type c (sub_mty ~ctx:(Mty mty) mty))
  | Expression, e ->
      fmt_expression c (sub_exp ~ctx:(Str (Ast_helper.Str.eval e)) e)
  | Repl_file, l -> fmt_repl_file c ctx l
  | Documentation, d ->
      (* TODO: [source] and [cmts] should have never been computed when
         formatting doc. *)
      Fmt_odoc.fmt_ast c.conf ~fmt_code:c.fmt_code d

let fmt_parse_result conf ~debug ast_kind ast source comments ~fmt_code =
  let cmts = Cmts.init ast_kind ~debug source ast comments in
  let ctx = Top in
  let code =
    set_margin conf.Conf.fmt_opts.margin.v
    $ fmt_file ~ctx ~debug ast_kind source cmts conf ast ~fmt_code
  in
  Ok (Format_.asprintf "%a" Fmt.eval code)

let fmt_code ~debug =
  let rec fmt_code (conf : Conf.t) ~offset s =
    let {Conf.fmt_opts; _} = conf in
    let conf =
      (* Adjust margin according to [offset]. *)
      let margin = {fmt_opts.margin with v= fmt_opts.margin.v - offset} in
      {conf with fmt_opts= {fmt_opts with margin}}
    in
    let warn = fmt_opts.parse_toplevel_phrases.v in
    let input_name = !Location.input_name in
    match
      Parse_with_comments.parse_toplevel ~disable_deprecated:true conf
        ~input_name ~source:s
    with
    | Either.First {ast; comments; source; prefix= _} ->
        fmt_parse_result conf ~debug Use_file ast source comments ~fmt_code
    | Second {ast; comments; source; prefix= _} ->
        fmt_parse_result conf ~debug Repl_file ast source comments ~fmt_code
    | exception Syntaxerr.Error (Expecting (_, x)) when warn ->
        Error (`Msg (Format.asprintf "expecting: %s" x))
    | exception Syntaxerr.Error (Not_expecting (_, x)) when warn ->
        Error (`Msg (Format.asprintf "not expecting: %s" x))
    | exception Syntaxerr.Error (Other _) when warn ->
        Error (`Msg (Format.asprintf "invalid toplevel or OCaml syntax"))
    | exception e when warn -> Error (`Msg (Format.asprintf "%a" Exn.pp e))
    | exception _ -> Error (`Msg "")
  in
  fmt_code

let fmt_ast fragment ~debug source cmts conf l =
  (* [Ast.init] should be called only once per file. In particular, we don't
     want to call it when formatting comments *)
  Ast.init conf ;
  let fmt_code = fmt_code ~debug in
  fmt_file ~ctx:Top ~fmt_code ~debug fragment source cmts conf l
