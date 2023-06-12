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

let check_local_attr attrs =
  match
    List.partition_tf attrs ~f:(fun attr ->
        String.equal attr.attr_name.txt "extension.local" )
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
let decompose_arrow ctx ctl ct2 =
  let pull_out_local ap =
    let ptyp_attributes, local =
      check_local_attr ap.pap_type.ptyp_attributes
    in
    ({ap with pap_type= {ap.pap_type with ptyp_attributes}}, local)
  in
  let args = List.map ~f:pull_out_local ctl in
  let ((res_ap, _) as res) =
    let ptyp_attributes, local = check_local_attr ct2.ptyp_attributes in
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
  (args @ [res], ctx)

type arg_kind =
  | Val of bool * arg_label * pattern xt * expression xt option
  | Newtypes of string loc list

let fun_ cmts ?(will_keep_first_ast_node = true) xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Exp exp in
    let {pexp_desc; pexp_loc; pexp_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pexp_attributes then
      match pexp_desc with
      | Pexp_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:pattern.ppat_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          let islocal, pat =
            match check_local_attr pattern.ppat_attributes with
            | _, false -> (false, sub_pat ~ctx pattern)
            | ppat_attributes, true ->
                let pattern = {pattern with ppat_attributes} in
                let ctx =
                  Exp
                    { exp with
                      pexp_desc= Pexp_fun (label, default, pattern, body) }
                in
                (true, sub_pat ~ctx pattern)
          in
          ( Val (islocal, label, pat, Option.map default ~f:(sub_exp ~ctx))
            :: xargs
          , xbody )
      | Pexp_newtype (name, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pexp_loc ~before:body.pexp_loc
              ~after:body.pexp_loc ;
          let xargs, xbody = fun_ (sub_exp ~ctx body) in
          let xargs =
            match xargs with
            | Newtypes names :: xargs -> Newtypes (name :: names) :: xargs
            | xargs -> Newtypes [name] :: xargs
          in
          (xargs, xbody)
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

let cl_fun ?(will_keep_first_ast_node = true) cmts xexp =
  let rec fun_ ?(will_keep_first_ast_node = false) ({ast= exp; _} as xexp) =
    let ctx = Cl exp in
    let {pcl_desc; pcl_loc; pcl_attributes; _} = exp in
    if will_keep_first_ast_node || List.is_empty pcl_attributes then
      match pcl_desc with
      | Pcl_fun (label, default, pattern, body) ->
          if not will_keep_first_ast_node then
            Cmts.relocate cmts ~src:pcl_loc ~before:pattern.ppat_loc
              ~after:body.pcl_loc ;
          let xargs, xbody = fun_ (sub_cl ~ctx body) in
          let islocal, pat =
            match check_local_attr pattern.ppat_attributes with
            | _, false -> (false, sub_pat ~ctx pattern)
            | ppat_attributes, true ->
                let pattern = {pattern with ppat_attributes} in
                let ctx =
                  Cl
                    { exp with
                      pcl_desc= Pcl_fun (label, default, pattern, body) }
                in
                (true, sub_pat ~ctx pattern)
          in
          ( Val (islocal, label, pat, Option.map default ~f:(sub_exp ~ctx))
            :: xargs
          , xbody )
      | _ -> ([], xexp)
    else ([], xexp)
  in
  fun_ ~will_keep_first_ast_node xexp

module Exp = struct
  let infix cmts prec xexp =
    let assoc = Option.value_map prec ~default:Assoc.Non ~f:Assoc.of_prec in
    let rec infix_ ?(relocate = true) xop xexp =
      let ctx = Exp xexp.ast in
      match (assoc, xexp.ast) with
      | ( Left
        , {pexp_desc= Pexp_infix ({txt= op; loc}, e1, e2); pexp_loc= src; _}
        )
        when Option.equal Prec.equal prec (prec_ast ctx) ->
          let op_args1 = infix_ None (sub_exp ~ctx e1) in
          let before =
            match op_args1 with
            | (Some {loc; _}, _) :: _ -> loc
            | (None, {ast= {pexp_loc; _}; _}) :: _ -> pexp_loc
            | _ -> loc
          in
          if relocate then Cmts.relocate cmts ~src ~before ~after:e2.pexp_loc ;
          op_args1 @ [(Some {txt= op; loc}, sub_exp ~ctx e2)]
      | ( Right
        , {pexp_desc= Pexp_infix ({txt= op; loc}, e1, e2); pexp_loc= src; _}
        )
        when Option.equal Prec.equal prec (prec_ast ctx) ->
          let op_args2 = infix_ (Some {txt= op; loc}) (sub_exp ~ctx e2) in
          let before =
            match xop with Some op -> op.loc | None -> e1.pexp_loc
          in
          let after =
            match List.last op_args2 with
            | Some (_, {ast= {pexp_loc; _}; _}) -> pexp_loc
            | None -> e1.pexp_loc
          in
          if relocate then Cmts.relocate cmts ~src ~before ~after ;
          (xop, sub_exp ~ctx e1) :: op_args2
      | _ -> [(xop, xexp)]
    in
    infix_ None ~relocate:false xexp
end

let sequence cmts xexp =
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
          if Ast.exposed_right_exp Ast.Let_match e1 then
            [(None, sub_exp ~ctx e1); (Some ext, sub_exp ~ctx e2)]
          else
            let l1 = sequence_ ~allow_attribute:false (sub_exp ~ctx e1) in
            let l2 =
              match sequence_ ~allow_attribute:false (sub_exp ~ctx e2) with
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
          if Ast.exposed_right_exp Ast.Let_match e1 then
            [(None, sub_exp ~ctx e1); (None, sub_exp ~ctx e2)]
          else
            List.append
              (sequence_ ~allow_attribute:false (sub_exp ~ctx e1))
              (sequence_ ~allow_attribute:false (sub_exp ~ctx e2)) )
    | _ -> [(None, xexp)]
  in
  sequence_ xexp

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec functor_type cmts ~for_functor_kw ~source_is_long
    ({ast= mty; _} as xmty) =
  let ctx = Mty mty in
  match mty with
  | {pmty_desc= Pmty_functor (fp, body); pmty_loc; pmty_attributes}
    when for_functor_kw
         || (List.is_empty pmty_attributes && not (source_is_long mty)) ->
      let body = sub_mty ~ctx body in
      let xargs, xbody =
        match pmty_attributes with
        | [] -> functor_type cmts ~for_functor_kw ~source_is_long body
        | _ -> ([], body)
      in
      (Location.mkloc fp pmty_loc :: xargs, xbody)
  | _ -> ([], xmty)

(* The sugar is different when used with the [functor] keyword. The syntax
   M(A : A)(B : B) cannot handle [_] as module name. *)
let rec functor_ cmts ~for_functor_kw ~source_is_long ({ast= me; _} as xme) =
  let ctx = Mod me in
  match me with
  | {pmod_desc= Pmod_functor (fp, body); pmod_loc; pmod_attributes}
    when for_functor_kw
         || (List.is_empty pmod_attributes && not (source_is_long me)) ->
      let body = sub_mod ~ctx body in
      let xargs, xbody_me =
        match pmod_attributes with
        | [] -> functor_ cmts ~for_functor_kw ~source_is_long body
        | _ -> ([], body)
      in
      (Location.mkloc fp pmod_loc :: xargs, xbody_me)
  | _ -> ([], xme)

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

let rec polynewtype_ cmts pvars body relocs =
  let ctx = Exp body in
  match (pvars, body.pexp_desc) with
  | [], Pexp_constraint (exp, typ) ->
      let relocs = (body.pexp_loc, exp.pexp_loc) :: relocs in
      Some (sub_typ ~ctx typ, sub_exp ~ctx exp, relocs)
  | pvar :: pvars, Pexp_newtype (nvar, exp)
    when String.equal pvar.txt nvar.txt ->
      let relocs = (nvar.loc, pvar.loc) :: relocs in
      polynewtype_ cmts pvars exp relocs
  | _ -> None

(** [polynewtype cmts pat exp] returns expression of a type-constrained
    pattern [pat] with body [exp]. e.g.:

    {v
      let f: 'r 's. 'r 's t = fun (type r) -> fun (type s) -> (e : r s t)
    v}

    Can be rewritten as:

    {[
      let f : type r s. r s t = e
    ]} *)
let polynewtype cmts pat body =
  let ctx = Pat pat in
  match pat.ppat_desc with
  | Ppat_constraint (pat2, {ptyp_desc= Ptyp_poly (pvars, _); _}) -> (
    match polynewtype_ cmts pvars body [(pat.ppat_loc, pat2.ppat_loc)] with
    | Some (typ, exp, relocs) ->
        List.iter relocs ~f:(fun (src, dst) ->
            Cmts.relocate cmts ~src ~before:dst ~after:dst ) ;
        Some (sub_pat ~ctx pat2, pvars, typ, exp)
    | None -> None )
  | _ -> None

module Let_binding = struct
  type t =
    { lb_op: string loc
    ; lb_pat: pattern xt
    ; lb_typ:
        [ `Polynewtype of label loc list * core_type xt
        | `Coerce of core_type xt option * core_type xt
        | `Other of arg_kind list * core_type xt
        | `None of arg_kind list ]
    ; lb_exp: expression xt
    ; lb_pun: bool
    ; lb_attrs: attribute list
    ; lb_local: bool
    ; lb_loc: Location.t }

  let type_cstr cmts ~ctx lb_pat lb_exp lb_is_pun =
    let islocal, ctx, lb_pat, lb_exp =
      match lb_exp.pexp_desc with
      | Pexp_apply
          ( { pexp_desc= Pexp_extension ({txt= "extension.local"; _}, PStr [])
            ; _ }
          , [(Nolabel, sbody)] ) ->
          let islocal, sbody =
            (* The desugaring is only valid for some patterns. The pattern
               part must still be rewritten as the parser duplicated the type
               annotations and extensions into the pattern and the
               expression. *)
            match lb_pat.ppat_desc with
            | Ppat_var _ | Ppat_constraint ({ppat_desc = Ppat_var _;_}, _) ->
                let sattrs, _ = check_local_attr sbody.pexp_attributes in
                (true, {sbody with pexp_attributes= sattrs})
            | _ -> (false, lb_exp)
          in
          let pattrs, _ = check_local_attr lb_pat.ppat_attributes in
          let pat = {lb_pat with ppat_attributes= pattrs} in
          let fake_ctx =
            Lb
              { lb_pattern= pat
              ; lb_expression= sbody
              ; lb_is_pun
              ; lb_attributes= []
              ; lb_loc= Location.none }
          in
          ( islocal
          , fake_ctx
          , sub_pat ~ctx:fake_ctx pat
          , sub_exp ~ctx:fake_ctx sbody )
      | _ -> (false, ctx, sub_pat ~ctx lb_pat, sub_exp ~ctx lb_exp)
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
      | ( Ppat_constraint (pat, {ptyp_desc= Ptyp_poly ([], typ1); _})
        , Pexp_coerce (_, _, typ2) )
        when equal_core_type typ1 typ2 ->
          Cmts.relocate cmts ~src:lb_pat.ast.ppat_loc ~before:pat.ppat_loc
            ~after:pat.ppat_loc ;
          sub_pat ~ctx:(Pat lb_pat.ast) pat
      | _ -> sub_pat ~ctx lb_pat.ast
    in
    let pat_is_extension {ppat_desc; _} =
      match ppat_desc with Ppat_extension _ -> true | _ -> false
    in
    let ({ast= body; _} as xbody) = sub_exp ~ctx lb_exp.ast in
    let pat, typ, exp =
      if
        (not (List.is_empty xbody.ast.pexp_attributes))
        || pat_is_extension pat
      then (xpat, `None [], xbody)
      else
        match polynewtype cmts pat body with
        | Some (xpat, pvars, xtyp, xbody) ->
            (xpat, `Polynewtype (pvars, xtyp), xbody)
        | None -> (
            let xpat =
              match xpat.ast.ppat_desc with
              | Ppat_constraint (p, {ptyp_desc= Ptyp_poly ([], _); _}) ->
                  sub_pat ~ctx:xpat.ctx p
              | _ -> xpat
            in
            let xargs, ({ast= body; _} as xbody) =
              match pat with
              | {ppat_desc= Ppat_var _; ppat_attributes= []; _} ->
                  fun_ cmts ~will_keep_first_ast_node:false xbody
              | _ -> ([], xbody)
            in
            let ctx = Exp body in
            match (body.pexp_desc, pat.ppat_desc) with
            | Pexp_constraint _, Ppat_constraint _ ->
                (xpat, `None xargs, xbody)
            | Pexp_constraint (exp, typ), _
              when Source.type_constraint_is_first typ exp.pexp_loc ->
                Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                  ~after:exp.pexp_loc ;
                let typ_ctx = Exp xbody.ast in
                let exp_ctx =
                  (* The type constraint is moved to the pattern, so we need
                     to replace the context from [Pexp_constraint] to
                     [Pexp_fun]. This won't be necessary once the
                     normalization is moved to [Extended_ast]. *)
                  let pat = Ast_helper.Pat.any () in
                  Exp (Ast_helper.Exp.fun_ Nolabel None pat exp)
                in
                ( xpat
                , `Other (xargs, sub_typ ~ctx:typ_ctx typ)
                , sub_exp ~ctx:exp_ctx exp )
            (* The type constraint is always printed before the declaration
               for functions, for other value bindings we preserve its
               position. *)
            | Pexp_constraint (exp, typ), _ when not (List.is_empty xargs) ->
                Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                  ~after:exp.pexp_loc ;
                (xpat, `Other (xargs, sub_typ ~ctx typ), sub_exp ~ctx exp)
            | Pexp_coerce (exp, typ1, typ2), _
              when Source.type_constraint_is_first typ2 exp.pexp_loc ->
                Cmts.relocate cmts ~src:body.pexp_loc ~before:exp.pexp_loc
                  ~after:exp.pexp_loc ;
                let typ1 = Option.map typ1 ~f:(sub_typ ~ctx) in
                (xpat, `Coerce (typ1, sub_typ ~ctx typ2), sub_exp ~ctx exp)
            | _ -> (xpat, `None xargs, xbody) )
    in
    (islocal, pat, typ, exp)

  let of_let_binding cmts ~ctx ~first lb =
    let islocal, pat, typ, exp =
      type_cstr cmts ~ctx lb.lb_pattern lb.lb_expression lb.lb_is_pun
    in
    { lb_op= Location.{txt= (if first then "let" else "and"); loc= none}
    ; lb_pat= pat
    ; lb_typ= typ
    ; lb_exp= exp
    ; lb_pun= false
    ; lb_attrs= lb.lb_attributes
    ; lb_local= islocal
    ; lb_loc= lb.lb_loc }

  let of_let_bindings cmts ~ctx =
    List.mapi ~f:(fun i -> of_let_binding cmts ~ctx ~first:(i = 0))

  let of_binding_ops cmts ~ctx bos =
    List.map bos ~f:(fun bo ->
        let islocal, pat, typ, exp =
          type_cstr cmts ~ctx bo.pbop_pat bo.pbop_exp false
        in
        { lb_op= bo.pbop_op
        ; lb_pat= pat
        ; lb_typ= typ
        ; lb_exp= exp
        ; lb_pun=
            ( match (pat.ast.ppat_desc, exp.ast.pexp_desc) with
            | Ppat_var {txt= v; _}, Pexp_ident {txt= Lident e; _} ->
                String.equal v e
            | _ -> false )
        ; lb_attrs= []
        ; lb_local= islocal
        ; lb_loc= bo.pbop_loc } )
end
