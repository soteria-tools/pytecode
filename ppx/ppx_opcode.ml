(* [@@deriving opcode] — boilerplate for a constant-constructor opcode enum.

   Derives:
   - [to_string : t -> string]
   - [of_string : string -> t option]
   - [all : t array] (constructors in declaration order)
   - one [t -> bool] predicate per flag attribute below; a constructor is in
     the predicate's truth set iff it carries the attribute, e.g.

     {[
       type t =
         | RESUME [@has_arg]
         | LOAD_CONST [@has_arg] [@has_const]
         | POP_JUMP_IF_FALSE [@has_arg] [@is_jump]
       [@@deriving opcode]
     ]}

   In signatures, [@@deriving opcode] declares the corresponding vals
   (attributes not required). *)

open Ppxlib
module B = Ast_builder.Default

let flag_names =
  [
    "has_arg";
    "is_jump";
    "has_const";
    "has_name";
    "has_local";
    "has_free";
    "has_exc";
  ]

let flag_attrs =
  List.map
    (fun name ->
      ( name,
        Attribute.declare ("opcode." ^ name)
          Attribute.Context.constructor_declaration
          Ast_pattern.(pstr nil)
          () ))
    flag_names

let get_variant ~loc = function
  | [ ({ ptype_kind = Ptype_variant cds; _ } as td) ] ->
      List.iter
        (fun cd ->
          match (cd.pcd_args, cd.pcd_res) with
          | Pcstr_tuple [], None -> ()
          | _ ->
              Location.raise_errorf ~loc:cd.pcd_loc
                "deriving opcode: constructors must be constant")
        cds;
      (td, cds)
  | _ ->
      Location.raise_errorf ~loc
        "deriving opcode: expected a single variant type declaration"

let construct_expr ~loc cd =
  B.pexp_construct ~loc { txt = Lident cd.pcd_name.txt; loc } None

let construct_pat ~loc cd =
  B.ppat_construct ~loc { txt = Lident cd.pcd_name.txt; loc } None

let td_type ~loc td =
  B.ptyp_constr ~loc { txt = Lident td.ptype_name.txt; loc } []

let to_string_item ~loc cds =
  let cases =
    List.map
      (fun cd ->
        B.case ~lhs:(construct_pat ~loc cd) ~guard:None
          ~rhs:(B.estring ~loc cd.pcd_name.txt))
      cds
  in
  [%stri let to_string = fun x -> [%e B.pexp_match ~loc [%expr x] cases]]

let of_string_item ~loc cds =
  let cases =
    List.map
      (fun cd ->
        B.case
          ~lhs:(B.pstring ~loc cd.pcd_name.txt)
          ~guard:None
          ~rhs:[%expr Some [%e construct_expr ~loc cd]])
      cds
    @ [ B.case ~lhs:(B.ppat_any ~loc) ~guard:None ~rhs:[%expr None] ]
  in
  [%stri let of_string = fun s -> [%e B.pexp_match ~loc [%expr s] cases]]

let all_item ~loc cds =
  [%stri let all = [%e B.pexp_array ~loc (List.map (construct_expr ~loc) cds)]]

let flag_item ~loc td cds (name, attr) =
  let marked = List.filter (fun cd -> Attribute.get attr cd <> None) cds in
  let ty = td_type ~loc td in
  let expr =
    if marked = [] then [%expr fun (_ : [%t ty]) -> false]
    else if List.length marked = List.length cds then
      [%expr fun (_ : [%t ty]) -> true]
    else
      let or_pat =
        match List.map (construct_pat ~loc) marked with
        | [] -> assert false
        | p :: ps -> List.fold_left (B.ppat_or ~loc) p ps
      in
      let cases =
        [
          B.case ~lhs:or_pat ~guard:None ~rhs:[%expr true];
          B.case ~lhs:(B.ppat_any ~loc) ~guard:None ~rhs:[%expr false];
        ]
      in
      [%expr fun x -> [%e B.pexp_match ~loc [%expr x] cases]]
  in
  B.pstr_value ~loc Nonrecursive
    [ B.value_binding ~loc ~pat:(B.ppat_var ~loc { txt = name; loc }) ~expr ]

let generate_impl ~ctxt (_rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let td, cds = get_variant ~loc tds in
  to_string_item ~loc cds :: of_string_item ~loc cds :: all_item ~loc cds
  :: List.map (flag_item ~loc td cds) flag_attrs

let val_item ~loc name type_ =
  B.psig_value ~loc
    (B.value_description ~loc ~name:{ txt = name; loc } ~type_ ~prim:[])

let generate_intf ~ctxt (_rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let td, _cds = get_variant ~loc tds in
  let t = td_type ~loc td in
  [
    val_item ~loc "to_string" [%type: [%t t] -> string];
    val_item ~loc "of_string" [%type: string -> [%t t] option];
    val_item ~loc "all" [%type: [%t t] array];
  ]
  @ List.map
      (fun (name, _) -> val_item ~loc name [%type: [%t t] -> bool])
      flag_attrs

let impl_generator =
  Deriving.Generator.V2.make_noarg
    ~attributes:(List.map (fun (_, a) -> Attribute.T a) flag_attrs)
    generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let (_ : Deriving.t) =
  Deriving.add "opcode" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
