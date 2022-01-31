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
open Asttypes
open Ast
open Extended_ast

<<<<<<< HEAD
let mk_function_param {Location.loc_start; _} {Location.loc_end; _} p =
  let pparam_loc = {Location.loc_start; loc_end; loc_ghost= true} in
  {pparam_desc= p; pparam_loc}
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
(* Temporary. Won't be necessary once the type [function_param] is used in
   [Pexp_fun] and [Pcl_fun]. *)
let mk_function_param pparam_desc =
  let pparam_loc =
    let init, locs =
      match pparam_desc with
      | Pparam_val (lbl, e, p) ->
          let locs =
            match lbl with
            | Nolabel -> []
            | Labelled x -> [x.loc]
            | Optional x -> [x.loc]
          in
          let locs =
            match e with Some e -> e.pexp_loc :: locs | None -> locs
          in
          (p.ppat_loc, locs)
      | Pparam_newtype types -> (
        match types with
        | [] -> failwith "Pparam_newtype always contains at least one type"
        | hd :: tl ->
            let locs = List.map tl ~f:(fun x -> x.loc) in
            (hd.loc, locs) )
    in
    let min acc x = if Location.compare_start acc x < 0 then acc else x in
    let max acc x = if Location.compare_end acc x > 0 then acc else x in
    let loc_start = (List.fold_left locs ~init ~f:min).loc_start in
    let loc_end = (List.fold_left locs ~init ~f:max).loc_end in
    {Location.loc_start; loc_end; loc_ghost= true}
  in
  {pparam_desc; pparam_loc}
=======
(* Temporary. Won't be necessary once the type [function_param] is used in
   [Pexp_fun] and [Pcl_fun]. *)
let mk_function_param pparam_desc =
  let pparam_loc =
    let init, locs =
      match pparam_desc with
      | Pparam_val (_islocal, lbl, e, p) ->
          let locs =
            match lbl with
            | Nolabel -> []
            | Labelled x -> [x.loc]
            | Optional x -> [x.loc]
          in
          let locs =
            match e with Some e -> e.pexp_loc :: locs | None -> locs
          in
          (p.ppat_loc, locs)
      | Pparam_newtype types -> (
        match types with
        | [] -> failwith "Pparam_newtype always contains at least one type"
        | hd :: tl ->
            let locs = List.map tl ~f:(fun x -> x.loc) in
            (hd.loc, locs) )
    in
    let min acc x = if Location.compare_start acc x < 0 then acc else x in
    let max acc x = if Location.compare_end acc x > 0 then acc else x in
    let loc_start = (List.fold_left locs ~init ~f:min).loc_start in
    let loc_end = (List.fold_left locs ~init ~f:max).loc_end in
    {Location.loc_start; loc_end; loc_ghost= true}
  in
  {pparam_desc; pparam_loc}
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)

let check_local_attr conf attrs =
  match
    List.partition_tf attrs ~f:(fun attr ->
        Conf.is_jane_street_local_annotation conf "local"
          ~test:attr.attr_name.txt )
  with
  | [], _ -> (attrs, false)
  | _ :: _, rest -> (rest, true)

(* This function pulls apart an arrow type, pulling out local attributes into
   bools and producing a context without those attributes. This addresses the
   problem that we need to remove the local attributes so that they can be
   printed specially, and that the context needs to be updated to reflect
   this to pass some internal ocamlformat sanity checks. It's not the
   cleanest solution in a vacuum, but is perhaps the one that will cause the
   fewest merge conflicts in the future. *)
let decompose_arrow conf ctx ctl ct2 =
  let pull_out_local ap =
    let ptyp_attributes, local =
      check_local_attr conf ap.pap_type.ptyp_attributes
    in
    ({ap with pap_type= {ap.pap_type with ptyp_attributes}}, local)
  in
  let args = List.map ~f:pull_out_local ctl in
  let ((res_ap, _) as res) =
    let ptyp_attributes, local = check_local_attr conf ct2.ptyp_attributes in
    let ap =
      { pap_label= Nolabel
      ; pap_loc= ct2.ptyp_loc
      ; pap_type= {ct2 with ptyp_attributes} }
    in
    (ap, local)
  in
  let ctx_typ = Ptyp_arrow (List.map ~f:fst args, res_ap.pap_type) in
  let ctx =
    match ctx with
    | Typ cty -> Typ {cty with ptyp_desc= ctx_typ}
    | _ -> assert false
  in
  (args, res, ctx)

let fun_ conf cmts ?(will_keep_first_ast_node = true) xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; pexp_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pexp_attributes then
      match pexp_desc with
      | Pexp_fun (p, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:p.pparam_loc
              ~after:body.pexp_loc ;
<<<<<<< HEAD
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          (p :: xargs, xbody)
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          ( mk_function_param (Pparam_val (label, default, pattern)) :: xargs
          , xbody )
=======
          let xargs, xbody = fun_ (sub_exp conf ~ctx body) in
          let islocal, pattern =
            match check_local_attr conf pattern.ppat_attributes with
            | _, false -> (false, pattern)
            | ppat_attributes, true -> (true, {pattern with ppat_attributes})
          in
          ( mk_function_param (Pparam_val (islocal, label, default, pattern))
            :: xargs
          , xbody )
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
      | Pexp_newtype (name, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:body.pexp_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = fun_ (sub_exp conf ~ctx body) in
          let xargs =
            match xargs with
            | {pparam_desc= Pparam_newtype names; pparam_loc} :: xargs ->
                let param = Pparam_newtype (name :: names) in
                mk_function_param name.loc pparam_loc param :: xargs
            | xargs ->
                let param = Pparam_newtype [name] in
                mk_function_param name.loc name.loc param :: xargs
          in
          (xargs, xbody)
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

let cl_fun ?(will_keep_first_ast_node = true) conf cmts xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Cl exp in
    let {pcl_desc; pcl_loc; pcl_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pcl_attributes then
      match pcl_desc with
      | Pcl_fun (label, default, pattern, body) ->
          let before = pattern.ppat_loc and after = body.pcl_loc in
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pcl_loc ~before ~after ;
          let xargs, xbody = fun_ (sub_cl ~ctx body) in
<<<<<<< HEAD
          let param = Pparam_val (label, default, pattern) in
          (mk_function_param before after param :: xargs, xbody)
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
          ( mk_function_param (Pparam_val (label, default, pattern)) :: xargs
          , xbody )
=======
          let islocal, pattern =
            match check_local_attr conf pattern.ppat_attributes with
            | _, false -> (false, pattern)
            | ppat_attributes, true -> (true, {pattern with ppat_attributes})
          in
          ( mk_function_param (Pparam_val (islocal, label, default, pattern))
            :: xargs
          , xbody )
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

module Exp = struct
  let infix conf cmts prec xexp =
    let assoc = Option.value_map prec ~default:Assoc.Non ~f:Assoc.of_prec in
    let rec infix_ ?(child_expr = true) xop xexp =
      let ctx = Exp xexp.ast in
      match (assoc, xexp.ast) with
      | _, {pexp_attributes= _ :: _; _} when child_expr ->
          (* Avoid dropping attributes on child expressions, e.g. [(a + b)
             [@attr] + c] *)
          [(xop, xexp)]
      | ( Left
        , {pexp_desc= Pexp_infix ({txt= op; loc}, e1, e2); pexp_loc= src; _}
        )
        when Option.equal Prec.equal prec (prec_ast ctx) ->
          let op_args1 = infix_ None (sub_exp conf ~ctx e1) in
          let before =
            match op_args1 with
            | (Some {loc; _}, _) :: _ -> loc
            | (None, {ast= {pexp_loc; _}; _}) :: _ -> pexp_loc
            | _ -> loc
          in
          if child_expr then
            Cmts.relocate cmts ~src ~before ~after:e2.pexp_loc ;
          op_args1 @ [(Some {txt= op; loc}, sub_exp conf ~ctx e2)]
      | ( Right
        , {pexp_desc= Pexp_infix ({txt= op; loc}, e1, e2); pexp_loc= src; _}
        )
        when Option.equal Prec.equal prec (prec_ast ctx) ->
          let op_args2 =
            infix_ (Some {txt= op; loc}) (sub_exp conf ~ctx e2)
          in
          let before =
            match xop with Some op -> op.loc | None -> e1.pexp_loc
          in
          let after =
            match List.last op_args2 with
            | Some (_, {ast= {pexp_loc; _}; _}) -> pexp_loc
            | None -> e1.pexp_loc
          in
          if child_expr then Cmts.relocate cmts ~src ~before ~after ;
          (xop, sub_exp conf ~ctx e1) :: op_args2
      | _ -> [(xop, xexp)]
    in
    infix_ None ~child_expr:false xexp
end

let sequence conf cmts xexp =
  let rec sequence_ ?(allow_attribute = true) ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; _} = exp in
    match pexp_desc with
    | Pexp_extension
        ( ext
        , PStr
            [ { pstr_desc=
                  Pstr_eval
                    ( ( { pexp_desc= Pexp_sequence (e1, e2)
                        ; pexp_attributes
                        ; _ } as exp )
                    , _ )
              ; pstr_loc } ] )
      when List.is_empty pexp_attributes
           && Source.extension_using_sugar ~name:ext ~payload:e1.pexp_loc ->
        let ctx = Exp exp in
        if (not allow_attribute) && not (List.is_empty exp.pexp_attributes)
        then [(None, xexp)]
        else (
          Cmts.relocate cmts ~src:pstr_loc ~before:e1.pexp_loc
            ~after:e2.pexp_loc ;
          Cmts.relocate cmts ~src:pexp_loc ~before:e1.pexp_loc
            ~after:e2.pexp_loc ;
          if Ast.exposed_right_exp conf Ast.Let_match e1 then
            [(None, sub_exp conf ~ctx e1); (Some ext, sub_exp conf ~ctx e2)]
          else
            let l1 =
              sequence_ ~allow_attribute:false (sub_exp conf ~ctx e1)
            in
            let l2 =
              match
                sequence_ ~allow_attribute:false (sub_exp conf ~ctx e2)
              with
              | [] -> []
              | (_, e2) :: l2 -> (Some ext, e2) :: l2
            in
            List.append l1 l2 )
    | Pexp_sequence (e1, e2) ->
        if (not allow_attribute) && not (List.is_empty exp.pexp_attributes)
        then [(None, xexp)]
        else (
          Cmts.relocate cmts ~src:pexp_loc ~before:e1.pexp_loc
            ~after:e2.pexp_loc ;
          if Ast.exposed_right_exp conf Ast.Let_match e1 then
            [(None, sub_exp conf ~ctx e1); (None, sub_exp conf ~ctx e2)]
          else
            List.append
              (sequence_ ~allow_attribute:false (sub_exp conf ~ctx e1))
              (sequence_ ~allow_attribute:false (sub_exp conf ~ctx e2)) )
    | _ -> [(None, xexp)]
  in
  sequence_ xexp

let mod_with pmty =
  let rec mod_with_ ({ast= me; _} as xme) =
    let ctx = Mty me in
    match me with
    | {pmty_desc= Pmty_with (mt, wcs); pmty_attributes; pmty_loc} ->
        let args, rest = mod_with_ (sub_mty ~ctx mt) in
        ((wcs, pmty_loc, pmty_attributes) :: args, rest)
    | _ -> ([], xme)
  in
  let l_rev, m = mod_with_ pmty in
  (List.rev l_rev, m)

module Let_binding = struct
  type t =
    { lb_op: string loc
    ; lb_pat: pattern xt
    ; lb_args: function_param list
    ; lb_typ: value_constraint option
    ; lb_exp: expression xt
    ; lb_pun: bool
    ; lb_attrs: attribute list
    ; lb_local: bool
    ; lb_loc: Location.t }

  let split_annot conf cmts xargs ({ast= body; _} as xbody) =
    let ctx = Exp body in
    match body.pexp_desc with
    | Pexp_constraint (exp, typ)
      when Source.type_constraint_is_first typ exp.pexp_loc ->
        Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
          ~after:exp.pexp_loc ;
        let exp_ctx =
          (* The type constraint is moved to the pattern, so we need to
             replace the context from [Pexp_constraint] to [Pexp_fun]. This
             won't be necessary once the normalization is moved to
             [Extended_ast]. *)
          let pat = Ast_helper.Pat.any () in
          let param =
            { pparam_desc= Pparam_val (Nolabel, None, pat)
            ; pparam_loc= pat.ppat_loc }
          in
          Exp (Ast_helper.Exp.fun_ param exp)
        in
<<<<<<< HEAD
        ( xargs
        , Some (Pvc_constraint {locally_abstract_univars= []; typ})
        , sub_exp ~ctx:exp_ctx exp )
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
        (xargs, `Other (sub_typ ~ctx:typ_ctx typ), sub_exp ~ctx:exp_ctx exp)
=======
        ( xargs
        , `Other (sub_typ ~ctx:typ_ctx typ)
        , sub_exp conf ~ctx:exp_ctx exp )
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
    (* The type constraint is always printed before the declaration for
       functions, for other value bindings we preserve its position. *)
    | Pexp_constraint (exp, typ) when not (List.is_empty xargs) ->
        Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
          ~after:exp.pexp_loc ;
<<<<<<< HEAD
        ( xargs
        , Some (Pvc_constraint {locally_abstract_univars= []; typ})
        , sub_exp ~ctx exp )
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
        (xargs, `Other (sub_typ ~ctx typ), sub_exp ~ctx exp)
=======
        (xargs, `Other (sub_typ ~ctx typ), sub_exp conf ~ctx exp)
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
    | Pexp_coerce (exp, typ1, typ2)
      when Source.type_constraint_is_first typ2 exp.pexp_loc ->
        Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
          ~after:exp.pexp_loc ;
<<<<<<< HEAD
        ( xargs
        , Some (Pvc_coercion {ground= typ1; coercion= typ2})
        , sub_exp ~ctx exp )
    | _ -> (xargs, None, xbody)
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
        let typ1 = Option.map typ1 ~f:(sub_typ ~ctx) in
        (xargs, `Coerce (typ1, sub_typ ~ctx typ2), sub_exp ~ctx exp)
    | _ -> (xargs, `None, xbody)
=======
        let typ1 = Option.map typ1 ~f:(sub_typ ~ctx) in
        (xargs, `Coerce (typ1, sub_typ ~ctx typ2), sub_exp conf ~ctx exp)
    | _ -> (xargs, `None, xbody)
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)

  let split_fun_args conf cmts xpat xbody =
    let xargs, xbody =
      match xpat.ast with
      | {ppat_desc= Ppat_var _; ppat_attributes= []; _} ->
          fun_ conf cmts ~will_keep_first_ast_node:false xbody
      | _ -> ([], xbody)
    in
    match (xbody.ast.pexp_desc, xpat.ast.ppat_desc) with
<<<<<<< HEAD
    | Pexp_constraint _, Ppat_constraint _ -> (xargs, None, xbody)
    | _ -> split_annot cmts xargs xbody
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
    | Pexp_constraint _, Ppat_constraint _ -> (xargs, `None, xbody)
    | _ -> split_annot cmts xargs xbody
=======
    | Pexp_constraint _, Ppat_constraint _ -> (xargs, `None, xbody)
    | _ -> split_annot conf cmts xargs xbody
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)

  (** Conservatively decides when to use the [let local_ ...] sugar.

      Putting [local_] on the left-hand-side of a simple variable binding
      is sugar for putting it on the right-hand-side.

      {[
        let x = local_ expression (* parses the exact same as *)
        let local_ x = expression
      ]}

      We have to be careful about trying to sugar something that isn't
      already sugared, though.

      {[
        let (x, y) = local_ expression (* parses while *)
        let local_ (x, y) = expression (* does not *)

        let local_ f x = expression (* parses the same as *)
        let f = local_ fun x -> expression (* and not as *)
        let f x = local_ expression
      ]}

      Ocamlformat checks that the formatting output does not change
      the parsed ast, which catches and fails on these cases.
      It also, however, fails even on changes to the ast that don't
      have any semantic difference.

      {[
        let x : string = local_ expression (* means the same thing as *)
        let local_ x : string = expression (* but parses differently *)
      ]}

      Currently, if there is any type annotation or coercion on the let
      binding, then sugaring the [local_] will create a different
      parsetree, so we just avoid sugaring in these cases.
      *)
  let local_pattern_can_be_sugared conf pvb_pat pvb_constraint exp_loc cmts =
    (* If the original code was sugared, preserve that always. *)
    let _, already_sugared = check_local_attr conf pvb_pat.ppat_attributes in
    (* Don't wipe away comments before [local_]. *)
    let comment_before = Cmts.has_before cmts exp_loc in
    already_sugared
    || (not comment_before)
       &&
       match pvb_pat.ppat_desc with
       | Ppat_var _ -> (
         match pvb_constraint with
         | None ->
             (* [ let x = local_ "hi" ] *)
             true
         | Some (Pvc_constraint _) ->
             (* [ let x : string = local_ "hi" ] [ let x : 'a. string =
                local_ "hi" ] *)
             false
         | Some (Pvc_coercion _) ->
             (* [ let x : string :> string = local_ "hi" ] [ let x :> string
                = local_ "hi" ] *)
             false )
       | _ -> false

  let maybe_sugar_local conf cmts ~ctx pvb_pat pvb_expr pvb_is_pun
      pvb_constraint =
    let is_local_pattern, ctx, pvb_pat, pvb_expr =
      match pvb_expr.pexp_desc with
      | Pexp_apply
          ( { pexp_desc= Pexp_extension ({txt= extension_local; _}, PStr [])
            ; _ }
          , [(Nolabel, sbody)] )
        when Conf.is_jane_street_local_annotation conf "local"
               ~test:extension_local ->
          let is_local_pattern, sbody =
            (* The pattern part must still be rewritten as the parser
               duplicated the type annotations and extensions into the
               pattern and the expression. *)
            if
              local_pattern_can_be_sugared conf pvb_pat pvb_constraint
                pvb_expr.pexp_loc cmts
            then
              let sattrs, _ = check_local_attr conf sbody.pexp_attributes in
              (true, {sbody with pexp_attributes= sattrs})
            else (false, pvb_expr)
          in
          let pattrs, _ = check_local_attr conf pvb_pat.ppat_attributes in
          let pat = {pvb_pat with ppat_attributes= pattrs} in
          let fake_ctx =
            Lb
              { pvb_pat= pat
              ; pvb_expr= sbody
              ; pvb_is_pun
              ; pvb_attributes= []
              ; pvb_loc= Location.none
              ; pvb_constraint= None }
          in
          (is_local_pattern, fake_ctx, pat, sbody)
      | _ -> (false, ctx, pvb_pat, pvb_expr)
    in
    let lb_pat = sub_pat ~ctx pvb_pat
    and lb_exp = sub_exp conf ~ctx pvb_expr in
    (is_local_pattern, lb_pat, lb_exp)

  let type_cstr conf cmts ~ctx pvb_pat pvb_expr pvb_is_pun pvb_constraint =
    let is_local_pattern, lb_pat, lb_exp =
      maybe_sugar_local conf cmts ~ctx pvb_pat pvb_expr pvb_is_pun
        pvb_constraint
    in
    let ({ast= pat; _} as xpat) =
      match (lb_pat.ast.ppat_desc, lb_exp.ast.pexp_desc) with
      (* recognize and undo the pattern of code introduced by
         ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
         https://caml.inria.fr/mantis/view.php?id=7344 *)
      | ( Ppat_constraint
            ( ({ppat_desc= Ppat_var _; _} as pat)
            , {ptyp_desc= Ptyp_poly ([], typ1); _} )
        , Pexp_constraint (_, typ2) )
        when equal_core_type typ1 typ2 ->
          Cmts.relocate cmts ~src:lb_pat.ast.ppat_loc ~before:pat.ppat_loc
            ~after:pat.ppat_loc ;
          sub_pat ~ctx:(Pat lb_pat.ast) pat
      | ( Ppat_constraint (_, {ptyp_desc= Ptyp_poly (_, typ1); _})
        , Pexp_coerce (_, _, typ2) )
        when equal_core_type typ1 typ2 ->
          sub_pat ~ctx lb_pat.ast
      | _ -> sub_pat ~ctx lb_pat.ast
    in
    let pat_is_extension {ppat_desc; _} =
      match ppat_desc with Ppat_extension _ -> true | _ -> false
    in
<<<<<<< HEAD
    let xbody = sub_exp ~ctx lb_exp in
    if
      (not (List.is_empty xbody.ast.pexp_attributes)) || pat_is_extension pat
    then (xpat, [], None, xbody)
    else
      let xpat =
        match xpat.ast.ppat_desc with
        | Ppat_constraint (p, {ptyp_desc= Ptyp_poly ([], _); _}) ->
            sub_pat ~ctx:xpat.ctx p
        | _ -> xpat
      in
      let xargs, typ, xbody = split_fun_args cmts xpat xbody in
      (xpat, xargs, typ, xbody)
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
    let xbody = sub_exp ~ctx lb_exp in
    if
      (not (List.is_empty xbody.ast.pexp_attributes)) || pat_is_extension pat
    then (xpat, [], `None, xbody)
    else
      let xpat =
        match xpat.ast.ppat_desc with
        | Ppat_constraint (p, {ptyp_desc= Ptyp_poly ([], _); _}) ->
            sub_pat ~ctx:xpat.ctx p
        | _ -> xpat
      in
      let xargs, typ, xbody = split_fun_args cmts xpat xbody in
      (xpat, xargs, typ, xbody)
=======
    let xbody = sub_exp conf ~ctx lb_exp.ast in
    let pat, xargs, typ, exp =
      if
        (not (List.is_empty xbody.ast.pexp_attributes))
        || pat_is_extension pat
      then (xpat, [], `None, xbody)
      else
        let xpat =
          match xpat.ast.ppat_desc with
          | Ppat_constraint (p, {ptyp_desc= Ptyp_poly ([], _); _}) ->
              sub_pat ~ctx:xpat.ctx p
          | _ -> xpat
        in
        let xargs, typ, xbody = split_fun_args conf cmts xpat xbody in
        (xpat, xargs, typ, xbody)
    in
    (is_local_pattern, pat, xargs, typ, exp)
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)

  let should_desugar_args pat typ =
    match (pat.ast, typ) with
    | {ppat_desc= Ppat_var _; ppat_attributes= []; _}, None -> true
    | _ -> false

  let of_let_binding conf cmts ~ctx ~first
      {pvb_pat; pvb_expr; pvb_constraint; pvb_is_pun; pvb_attributes; pvb_loc}
      =
<<<<<<< HEAD
    let lb_exp = sub_exp ~ctx pvb_expr
    and lb_pat = sub_pat ~ctx pvb_pat
    and lb_typ = pvb_constraint in
||||||| parent of 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
    let lb_exp = sub_exp ~ctx pvb_expr
    and lb_pat = sub_pat ~ctx pvb_pat
    and lb_typ = typ_of_pvb_constraint ~ctx pvb_constraint in
=======
    let islocal, lb_pat, lb_exp =
      maybe_sugar_local conf cmts ~ctx pvb_pat pvb_expr pvb_is_pun
        pvb_constraint
    and lb_typ = typ_of_pvb_constraint ~ctx pvb_constraint in
>>>>>>> 21fd82a8 (Rebase of janestreet/jane 0d7b8ee..62a1097 onto ocaml-ppx/main b178884.)
    let lb_args, lb_typ, lb_exp =
      if should_desugar_args lb_pat lb_typ then
        split_fun_args conf cmts lb_pat lb_exp
      else ([], lb_typ, lb_exp)
    in
    { lb_op= Location.{txt= (if first then "let" else "and"); loc= none}
    ; lb_pat
    ; lb_args
    ; lb_typ
    ; lb_exp
    ; lb_pun= pvb_is_pun
    ; lb_attrs= pvb_attributes
    ; lb_local= islocal
    ; lb_loc= pvb_loc }

  let of_let_bindings conf cmts ~ctx =
    List.mapi ~f:(fun i -> of_let_binding conf cmts ~ctx ~first:(i = 0))

  let of_binding_ops conf cmts ~ctx bos =
    List.map bos ~f:(fun bo ->
        let islocal, lb_pat, lb_args, lb_typ, lb_exp =
          type_cstr conf cmts ~ctx bo.pbop_pat bo.pbop_exp false None
        in
        { lb_op= bo.pbop_op
        ; lb_pat
        ; lb_args
        ; lb_typ
        ; lb_exp
        ; lb_pun= bo.pbop_is_pun
        ; lb_attrs= []
        ; lb_local= islocal
        ; lb_loc= bo.pbop_loc } )
end
