exception Unsupported of string

type var = Fast of int | Deref of int | Name of string | Global of string

type binop =
  | Add
  | And
  | Floor_div
  | Lshift
  | Mat_mul
  | Mul
  | Mod
  | Or
  | Pow
  | Rshift
  | Sub
  | Div
  | Xor

type cmpop = Lt | Le | Eq | Ne | Gt | Ge
type unop = Negative | Not | Invert | To_bool
type conv = Str_conv | Repr_conv | Ascii_conv
type cond = If_true | If_false | If_none | If_not_none
type func_attr = Defaults | Kw_defaults | Annotations | Closure

type intrinsic_1 =
  | Print
  | Import_star
  | Stopiteration_error
  | Async_gen_wrap
  | Unary_positive
  | List_to_tuple
  | Typevar
  | Paramspec
  | Typevartuple
  | Subscript_generic
  | Typealias

type intrinsic_2 =
  | Prep_reraise_star
  | Typevar_with_bound
  | Typevar_with_constraints
  | Set_function_type_params
  | Set_typeparam_default

(* The same frame/debug/exception-table shape as raw bytecode (see {!Ast.code}),
   but carrying Phir instructions. Phir inlines constants and variable reads, so
   the [consts]/[names] tables are left empty; the lone surviving constant, the
   docstring, is copied into [docstring]. *)
type code = instr Ast.code
and value = Stack | Null | Const of Ast.const | Code of code | Var of var

and instr =
  | Assign of var * value
  | Delete of var
  | Push of value
  | Pop_top of value
  | Copy of int
  | Swap of int
  | Unary of unop * value
  | Binary_op of { op : binop; inplace : bool; l : value; r : value }
  | Compare of { op : cmpop; coerce_bool : bool; l : value; r : value }
  | Is_op of { invert : bool; l : value; r : value }
  | Contains_op of { invert : bool; item : value; seq : value }
  | Subscript of value * value
  | Store_subscr of { value : value; obj : value; index : value }
  | Delete_subscr of { obj : value; index : value }
  | Binary_slice of { obj : value; start : value; stop : value }
  | Store_slice of { value : value; obj : value; start : value; stop : value }
  | Load_attr of { obj : value; name : string; meth : bool }
  | Load_super_attr of {
      super : value;
      cls : value;
      self : value;
      name : string;
      meth : bool;
      two_arg : bool;
    }
  | Store_attr of { value : value; obj : value; name : string }
  | Delete_attr of { obj : value; name : string }
  | Call of { f : value; self : value; args : value array }
  | Call_kw of { f : value; self : value; args : value array; kw_names : value }
  | Call_ex of { f : value; null : value; args : value; kwargs : value option }
  | Intrinsic_1 of intrinsic_1 * value
  | Intrinsic_2 of intrinsic_2 * value * value
  | Jump of int
  | Cond_jump of { cond : cond; v : value; target : int }
  | Get_iter of value
  | For_iter of int
  | End_for
  | Get_yield_from_iter of value
  | Return of value
  | Return_generator
  | Yield of { v : value; arg : int }
  | Send of { v : value; on_stop : int }
  | End_send
  | Cleanup_throw
  | Raise of { exc : value option; cause : value option }
  | Reraise of int
  | Push_exc_info
  | Pop_except
  | Check_exc_match of value
  | Check_eg_match of { exc : value; pattern : value }
  | With_except_start
  | Build_tuple of value array
  | Build_list of value array
  | Build_set of value array
  | Build_map of (value * value) array
  | Build_string of value array
  | Build_slice of value array
  | Build_const_key_map of { keys : value; values : value array }
  | List_append of int * value
  | Set_add of int * value
  | Map_add of int * value * value
  | List_extend of int * value
  | Set_update of int * value
  | Dict_update of int * value
  | Dict_merge of int * value
  | Unpack_sequence of int * value
  | Unpack_ex of { before : int; after : int; v : value }
  | Format_simple of value
  | Format_with_spec of value * value
  | Convert_value of conv * value
  | Make_function of value
  | Set_function_attribute of { attr : func_attr; v : value; f : value }
  | Make_cell of int
  | Copy_free_vars of int
  | Load_build_class
  | Load_assertion_error
  | Setup_annotations
  | Load_locals
  | Load_from_dict_or_globals of value * string
  | Load_from_dict_or_deref of value * int
  | Load_fast_and_clear of int
  | Import_name of { name : string; level : value; from_list : value }
  | Import_from of string
  | Get_awaitable of int * value
  | Get_aiter of value
  | Get_anext
  | End_async_for
  | Before_with of value
  | Before_async_with of value
  | Match_class of { count : int; subject : value; cls : value; names : value }
  | Match_mapping
  | Match_sequence
  | Match_keys
  | Get_len

let values : instr -> value list = function
  | Assign (_, v)
  | Push v
  | Pop_top v
  | Unary (_, v)
  | Get_iter v
  | Get_yield_from_iter v
  | Return v
  | Yield { v; _ }
  | Send { v; _ }
  | Check_exc_match v
  | List_append (_, v)
  | Set_add (_, v)
  | List_extend (_, v)
  | Set_update (_, v)
  | Dict_update (_, v)
  | Dict_merge (_, v)
  | Unpack_sequence (_, v)
  | Unpack_ex { v; _ }
  | Format_simple v
  | Convert_value (_, v)
  | Make_function v
  | Load_from_dict_or_globals (v, _)
  | Load_from_dict_or_deref (v, _)
  | Get_awaitable (_, v)
  | Get_aiter v
  | Before_with v
  | Before_async_with v
  | Cond_jump { v; _ }
  | Intrinsic_1 (_, v) ->
      [ v ]
  | Binary_op { l; r; _ }
  | Compare { l; r; _ }
  | Is_op { l; r; _ }
  | Contains_op { item = l; seq = r; _ }
  | Subscript (l, r)
  | Format_with_spec (l, r)
  | Intrinsic_2 (_, l, r)
  | Map_add (_, l, r)
  | Check_eg_match { exc = l; pattern = r }
  | Delete_subscr { obj = l; index = r }
  | Store_attr { value = l; obj = r; _ }
  | Set_function_attribute { v = l; f = r; _ }
  | Import_name { level = l; from_list = r; _ } ->
      [ l; r ]
  | Store_subscr { value; obj; index } -> [ value; obj; index ]
  | Binary_slice { obj; start; stop } -> [ obj; start; stop ]
  | Store_slice { value; obj; start; stop } -> [ value; obj; start; stop ]
  | Load_attr { obj; _ } | Delete_attr { obj; _ } -> [ obj ]
  | Load_super_attr { super; cls; self; _ } -> [ super; cls; self ]
  | Call { f; self; args } -> f :: self :: Array.to_list args
  | Call_kw { f; self; args; kw_names } ->
      (f :: self :: Array.to_list args) @ [ kw_names ]
  | Call_ex { f; null; args; kwargs } -> (
      [ f; null; args ] @ match kwargs with Some k -> [ k ] | None -> [])
  | Raise { exc; cause } -> List.filter_map Fun.id [ exc; cause ]
  | Build_tuple vs
  | Build_list vs
  | Build_set vs
  | Build_string vs
  | Build_slice vs ->
      Array.to_list vs
  | Build_map pairs ->
      List.concat_map (fun (k, v) -> [ k; v ]) (Array.to_list pairs)
  | Build_const_key_map { keys; values } -> Array.to_list values @ [ keys ]
  | Match_class { subject; cls; names; _ } -> [ subject; cls; names ]
  | Delete _ | Copy _ | Swap _ | Jump _ | For_iter _ | End_for
  | Return_generator | End_send | Cleanup_throw | Reraise _ | Push_exc_info
  | Pop_except | With_except_start | Make_cell _ | Copy_free_vars _
  | Load_build_class | Load_assertion_error | Setup_annotations | Load_locals
  | Load_fast_and_clear _ | Import_from _ | Get_anext | End_async_for
  | Match_mapping | Match_sequence | Match_keys | Get_len ->
      []

(* ------------------------------------------------------------------ *)
(* Transformation                                                      *)
(* ------------------------------------------------------------------ *)

let binop_table =
  [|
    Add;
    And;
    Floor_div;
    Lshift;
    Mat_mul;
    Mul;
    Mod;
    Or;
    Pow;
    Rshift;
    Sub;
    Div;
    Xor;
  |]

let cmpop_table = [| Lt; Le; Eq; Ne; Gt; Ge |]

let rec contains_code : Ast.const -> bool = function
  | Code _ -> true
  | Tuple xs | Frozenset xs -> Array.exists contains_code xs
  | _ -> false

let op1 = function [ a ] -> a | _ -> assert false
let op2 = function [ a; b ] -> (a, b) | _ -> assert false
let op3 = function [ a; b; c ] -> (a, b, c) | _ -> assert false
let op4 = function [ a; b; c; d ] -> (a, b, c, d) | _ -> assert false

let split_last l =
  match List.rev l with
  | last :: rev_init -> (List.rev rev_init, last)
  | [] -> assert false

let rec pair_up = function
  | [] -> []
  | k :: v :: tl -> (k, v) :: pair_up tl
  | _ -> assert false

let rec of_code (ast : Ast.instr Ast.code) : code =
  let fail msg = raise (Unsupported (ast.qualname ^ ": " ^ msg)) in
  let unsupported (op : Opcode.t) =
    fail ("unsupported opcode " ^ Opcode.to_string op)
  in
  let value_of_const (c : Ast.const) : value =
    match c with
    | Code nested -> Code (of_code nested)
    | c ->
        if contains_code c then fail "code object inside composite constant"
        else Const c
  in
  let intrinsic_1_of = function
    | 1 -> Print
    | 2 -> Import_star
    | 3 -> Stopiteration_error
    | 4 -> Async_gen_wrap
    | 5 -> Unary_positive
    | 6 -> List_to_tuple
    | 7 -> Typevar
    | 8 -> Paramspec
    | 9 -> Typevartuple
    | 10 -> Subscript_generic
    | 11 -> Typealias
    | k -> fail ("CALL_INTRINSIC_1 " ^ string_of_int k)
  in
  let intrinsic_2_of = function
    | 1 -> Prep_reraise_star
    | 2 -> Typevar_with_bound
    | 3 -> Typevar_with_constraints
    | 4 -> Set_function_type_params
    | 5 -> Set_typeparam_default
    | k -> fail ("CALL_INTRINSIC_2 " ^ string_of_int k)
  in
  let func_attr_of = function
    | 1 -> Defaults
    | 2 -> Kw_defaults
    | 4 -> Annotations
    | 8 -> Closure
    | k -> fail ("SET_FUNCTION_ATTRIBUTE " ^ string_of_int k)
  in
  let conv_of = function
    | 1 -> Str_conv
    | 2 -> Repr_conv
    | 3 -> Ascii_conv
    | k -> fail ("CONVERT_VALUE " ^ string_of_int k)
  in
  let binop_of arg =
    if arg >= 0 && arg < 13 then (binop_table.(arg), false)
    else if arg >= 13 && arg < 26 then (binop_table.(arg - 13), true)
    else fail ("BINARY_OP " ^ string_of_int arg)
  in
  let n = Array.length ast.instrs in
  (* Folding never crosses a jump target or an exception-table boundary:
     other paths land there with their own stack contents. *)
  let barrier = Array.make (n + 1) false in
  Array.iter
    (fun { Ast.op; arg } -> if Opcode.is_jump op then barrier.(arg) <- true)
    ast.instrs;
  Array.iter
    (fun (e : Ast.exn_entry) ->
      barrier.(e.start_idx) <- true;
      barrier.(e.end_idx) <- true;
      barrier.(e.target_idx) <- true)
    ast.exn_table;
  let out : (instr * int) Dynarray.t = Dynarray.create () in
  let new_idx = Array.make (n + 1) (-1) in
  let emit ~old ins =
    if new_idx.(old) = -1 then new_idx.(old) <- Dynarray.length out;
    Dynarray.add_last out (ins, old)
  in
  (* Pending foldable pushes, most recent first, with their original index. *)
  let pending : (value * int) list ref = ref [] in
  let flush () =
    List.iter (fun (v, oi) -> emit ~old:oi (Push v)) (List.rev !pending);
    pending := []
  in
  let push_pending old v = pending := (v, old) :: !pending in
  (* The k operands of a consumer, in push order. The most recent pending
     pushes fold in (operand suffix); anything pending below them is
     materialized first; missing operands come from the runtime stack. *)
  let operands k =
    let p = !pending in
    pending := [];
    let fold_n = min k (List.length p) in
    let rec take i acc rest =
      if i = 0 then (acc, rest)
      else
        match rest with
        | x :: tl -> take (i - 1) (x :: acc) tl
        | [] -> assert false
    in
    let folded, deeper = take fold_n [] p in
    List.iter (fun (v, oi) -> emit ~old:oi (Push v)) (List.rev deeper);
    List.init (k - fold_n) (fun _ -> Stack) @ List.map fst folded
  in
  for i = 0 to n - 1 do
    if barrier.(i) then flush ();
    let { Ast.op; arg } = ast.instrs.(i) in
    let e ins = emit ~old:i ins in
    let name idx = ast.names.(idx) in
    match op with
    (* -- foldable pushes: enter the pending window -- *)
    | LOAD_CONST -> push_pending i (value_of_const ast.consts.(arg))
    | LOAD_FAST | LOAD_FAST_CHECK -> push_pending i (Var (Fast arg))
    | LOAD_FAST_LOAD_FAST ->
        push_pending i (Var (Fast (arg lsr 4)));
        push_pending i (Var (Fast (arg land 0xf)))
    | LOAD_DEREF -> push_pending i (Var (Deref arg))
    | LOAD_NAME -> push_pending i (Var (Name (name arg)))
    | LOAD_GLOBAL ->
        push_pending i (Var (Global (name (arg lsr 1))));
        if arg land 1 = 1 then push_pending i Null
    | PUSH_NULL -> push_pending i Null
    (* -- dropped -- *)
    | NOP | RESUME -> ()
    (* -- data movement -- *)
    | STORE_FAST -> e (Assign (Fast arg, op1 (operands 1)))
    | STORE_DEREF -> e (Assign (Deref arg, op1 (operands 1)))
    | STORE_NAME -> e (Assign (Name (name arg), op1 (operands 1)))
    | STORE_GLOBAL -> e (Assign (Global (name arg), op1 (operands 1)))
    | STORE_FAST_STORE_FAST ->
        (* SETLOCAL(arg >> 4, TOS); SETLOCAL(arg & 15, TOS1) *)
        let v2, v1 = op2 (operands 2) in
        e (Assign (Fast (arg lsr 4), v1));
        e (Assign (Fast (arg land 0xf), v2))
    | STORE_FAST_LOAD_FAST ->
        e (Assign (Fast (arg lsr 4), op1 (operands 1)));
        push_pending i (Var (Fast (arg land 0xf)))
    | DELETE_FAST ->
        flush ();
        e (Delete (Fast arg))
    | DELETE_DEREF ->
        flush ();
        e (Delete (Deref arg))
    | DELETE_NAME ->
        flush ();
        e (Delete (Name (name arg)))
    | DELETE_GLOBAL ->
        flush ();
        e (Delete (Global (name arg)))
    | POP_TOP -> e (Pop_top (op1 (operands 1)))
    | COPY ->
        flush ();
        e (Copy arg)
    | SWAP ->
        flush ();
        e (Swap arg)
    (* -- operators -- *)
    | UNARY_NEGATIVE -> e (Unary (Negative, op1 (operands 1)))
    | UNARY_NOT -> e (Unary (Not, op1 (operands 1)))
    | UNARY_INVERT -> e (Unary (Invert, op1 (operands 1)))
    | TO_BOOL -> e (Unary (To_bool, op1 (operands 1)))
    | BINARY_OP ->
        let op, inplace = binop_of arg in
        let l, r = op2 (operands 2) in
        e (Binary_op { op; inplace; l; r })
    | COMPARE_OP ->
        let idx = arg lsr 5 in
        if idx > 5 then fail ("COMPARE_OP " ^ string_of_int arg);
        let l, r = op2 (operands 2) in
        e
          (Compare
             { op = cmpop_table.(idx); coerce_bool = arg land 16 <> 0; l; r })
    | IS_OP ->
        let l, r = op2 (operands 2) in
        e (Is_op { invert = arg = 1; l; r })
    | CONTAINS_OP ->
        let item, seq = op2 (operands 2) in
        e (Contains_op { invert = arg = 1; item; seq })
    | BINARY_SUBSCR ->
        let obj, index = op2 (operands 2) in
        e (Subscript (obj, index))
    | STORE_SUBSCR ->
        let value, obj, index = op3 (operands 3) in
        e (Store_subscr { value; obj; index })
    | DELETE_SUBSCR ->
        let obj, index = op2 (operands 2) in
        e (Delete_subscr { obj; index })
    | BINARY_SLICE ->
        let obj, start, stop = op3 (operands 3) in
        e (Binary_slice { obj; start; stop })
    | STORE_SLICE ->
        let value, obj, start, stop = op4 (operands 4) in
        e (Store_slice { value; obj; start; stop })
    (* -- attributes -- *)
    | LOAD_ATTR ->
        e
          (Load_attr
             {
               obj = op1 (operands 1);
               name = name (arg lsr 1);
               meth = arg land 1 = 1;
             })
    | LOAD_SUPER_ATTR ->
        let super, cls, self = op3 (operands 3) in
        e
          (Load_super_attr
             {
               super;
               cls;
               self;
               name = name (arg lsr 2);
               meth = arg land 1 = 1;
               two_arg = arg land 2 <> 0;
             })
    | STORE_ATTR ->
        let value, obj = op2 (operands 2) in
        e (Store_attr { value; obj; name = name arg })
    | DELETE_ATTR -> e (Delete_attr { obj = op1 (operands 1); name = name arg })
    (* -- calls -- *)
    | CALL -> (
        match operands (arg + 2) with
        | f :: self :: args -> e (Call { f; self; args = Array.of_list args })
        | _ -> assert false)
    | CALL_KW -> (
        match operands (arg + 3) with
        | f :: self :: rest ->
            let args, kw_names = split_last rest in
            e (Call_kw { f; self; args = Array.of_list args; kw_names })
        | _ -> assert false)
    | CALL_FUNCTION_EX ->
        if arg land 1 = 1 then
          let f, null, args, kw = op4 (operands 4) in
          e (Call_ex { f; null; args; kwargs = Some kw })
        else
          let f, null, args = op3 (operands 3) in
          e (Call_ex { f; null; args; kwargs = None })
    | CALL_INTRINSIC_1 -> e (Intrinsic_1 (intrinsic_1_of arg, op1 (operands 1)))
    | CALL_INTRINSIC_2 ->
        let a, b = op2 (operands 2) in
        e (Intrinsic_2 (intrinsic_2_of arg, a, b))
    (* -- control flow (targets remapped in the post-pass) -- *)
    | JUMP_FORWARD | JUMP_BACKWARD | JUMP_BACKWARD_NO_INTERRUPT ->
        flush ();
        e (Jump arg)
    | POP_JUMP_IF_TRUE ->
        e (Cond_jump { cond = If_true; v = op1 (operands 1); target = arg })
    | POP_JUMP_IF_FALSE ->
        e (Cond_jump { cond = If_false; v = op1 (operands 1); target = arg })
    | POP_JUMP_IF_NONE ->
        e (Cond_jump { cond = If_none; v = op1 (operands 1); target = arg })
    | POP_JUMP_IF_NOT_NONE ->
        e (Cond_jump { cond = If_not_none; v = op1 (operands 1); target = arg })
    | GET_ITER -> e (Get_iter (op1 (operands 1)))
    | FOR_ITER ->
        flush ();
        e (For_iter arg)
    | END_FOR ->
        flush ();
        e End_for
    | GET_YIELD_FROM_ITER -> e (Get_yield_from_iter (op1 (operands 1)))
    | RETURN_VALUE -> e (Return (op1 (operands 1)))
    | RETURN_CONST ->
        flush ();
        e (Return (value_of_const ast.consts.(arg)))
    | RETURN_GENERATOR ->
        flush ();
        e Return_generator
    | YIELD_VALUE -> e (Yield { v = op1 (operands 1); arg })
    | SEND -> e (Send { v = op1 (operands 1); on_stop = arg })
    | END_SEND ->
        flush ();
        e End_send
    | CLEANUP_THROW ->
        flush ();
        e Cleanup_throw
    (* -- exceptions -- *)
    | RAISE_VARARGS -> (
        match arg with
        | 0 ->
            flush ();
            e (Raise { exc = None; cause = None })
        | 1 -> e (Raise { exc = Some (op1 (operands 1)); cause = None })
        | 2 ->
            let exc, cause = op2 (operands 2) in
            e (Raise { exc = Some exc; cause = Some cause })
        | k -> fail ("RAISE_VARARGS " ^ string_of_int k))
    | RERAISE ->
        flush ();
        e (Reraise arg)
    | PUSH_EXC_INFO ->
        flush ();
        e Push_exc_info
    | POP_EXCEPT ->
        flush ();
        e Pop_except
    | CHECK_EXC_MATCH -> e (Check_exc_match (op1 (operands 1)))
    | CHECK_EG_MATCH ->
        let exc, pattern = op2 (operands 2) in
        e (Check_eg_match { exc; pattern })
    | WITH_EXCEPT_START ->
        flush ();
        e With_except_start
    (* -- builds -- *)
    | BUILD_TUPLE -> e (Build_tuple (Array.of_list (operands arg)))
    | BUILD_LIST -> e (Build_list (Array.of_list (operands arg)))
    | BUILD_SET -> e (Build_set (Array.of_list (operands arg)))
    | BUILD_STRING -> e (Build_string (Array.of_list (operands arg)))
    | BUILD_SLICE -> e (Build_slice (Array.of_list (operands arg)))
    | BUILD_MAP -> e (Build_map (Array.of_list (pair_up (operands (2 * arg)))))
    | BUILD_CONST_KEY_MAP ->
        let values, keys = split_last (operands (arg + 1)) in
        e (Build_const_key_map { keys; values = Array.of_list values })
    | LIST_APPEND -> e (List_append (arg, op1 (operands 1)))
    | SET_ADD -> e (Set_add (arg, op1 (operands 1)))
    | MAP_ADD ->
        let k, v = op2 (operands 2) in
        e (Map_add (arg, k, v))
    | LIST_EXTEND -> e (List_extend (arg, op1 (operands 1)))
    | SET_UPDATE -> e (Set_update (arg, op1 (operands 1)))
    | DICT_UPDATE -> e (Dict_update (arg, op1 (operands 1)))
    | DICT_MERGE -> e (Dict_merge (arg, op1 (operands 1)))
    | UNPACK_SEQUENCE -> e (Unpack_sequence (arg, op1 (operands 1)))
    | UNPACK_EX ->
        e
          (Unpack_ex
             { before = arg land 0xff; after = arg lsr 8; v = op1 (operands 1) })
    | FORMAT_SIMPLE -> e (Format_simple (op1 (operands 1)))
    | FORMAT_WITH_SPEC ->
        let v, spec = op2 (operands 2) in
        e (Format_with_spec (v, spec))
    | CONVERT_VALUE -> e (Convert_value (conv_of arg, op1 (operands 1)))
    (* -- functions, classes, scopes -- *)
    | MAKE_FUNCTION -> e (Make_function (op1 (operands 1)))
    | SET_FUNCTION_ATTRIBUTE ->
        let v, f = op2 (operands 2) in
        e (Set_function_attribute { attr = func_attr_of arg; v; f })
    | MAKE_CELL ->
        flush ();
        e (Make_cell arg)
    | COPY_FREE_VARS ->
        flush ();
        e (Copy_free_vars arg)
    | LOAD_BUILD_CLASS ->
        flush ();
        e Load_build_class
    | LOAD_ASSERTION_ERROR ->
        flush ();
        e Load_assertion_error
    | SETUP_ANNOTATIONS ->
        flush ();
        e Setup_annotations
    | LOAD_LOCALS ->
        flush ();
        e Load_locals
    | LOAD_FROM_DICT_OR_GLOBALS ->
        e (Load_from_dict_or_globals (op1 (operands 1), name arg))
    | LOAD_FROM_DICT_OR_DEREF ->
        e (Load_from_dict_or_deref (op1 (operands 1), arg))
    | LOAD_FAST_AND_CLEAR ->
        flush ();
        e (Load_fast_and_clear arg)
    (* -- imports -- *)
    | IMPORT_NAME ->
        let level, from_list = op2 (operands 2) in
        e (Import_name { name = name arg; level; from_list })
    | IMPORT_FROM ->
        flush ();
        e (Import_from (name arg))
    (* -- async -- *)
    | GET_AWAITABLE -> e (Get_awaitable (arg, op1 (operands 1)))
    | GET_AITER -> e (Get_aiter (op1 (operands 1)))
    | GET_ANEXT ->
        flush ();
        e Get_anext
    | END_ASYNC_FOR ->
        flush ();
        e End_async_for
    | BEFORE_WITH -> e (Before_with (op1 (operands 1)))
    | BEFORE_ASYNC_WITH -> e (Before_async_with (op1 (operands 1)))
    (* -- pattern matching -- *)
    | MATCH_CLASS ->
        let subject, cls, names = op3 (operands 3) in
        e (Match_class { count = arg; subject; cls; names })
    | MATCH_MAPPING ->
        flush ();
        e Match_mapping
    | MATCH_SEQUENCE ->
        flush ();
        e Match_sequence
    | MATCH_KEYS ->
        flush ();
        e Match_keys
    | GET_LEN ->
        flush ();
        e Get_len
    (* -- cannot occur in freshly compiled pinned-version code -- *)
    | ( CACHE | EXTENDED_ARG | RESERVED | ENTER_EXECUTOR | EXIT_INIT_CHECK
      | INTERPRETER_EXIT | INSTRUMENTED_CALL | INSTRUMENTED_CALL_FUNCTION_EX
      | INSTRUMENTED_CALL_KW | INSTRUMENTED_END_FOR | INSTRUMENTED_END_SEND
      | INSTRUMENTED_FOR_ITER | INSTRUMENTED_INSTRUCTION
      | INSTRUMENTED_JUMP_BACKWARD | INSTRUMENTED_JUMP_FORWARD
      | INSTRUMENTED_LINE | INSTRUMENTED_LOAD_SUPER_ATTR
      | INSTRUMENTED_POP_JUMP_IF_FALSE | INSTRUMENTED_POP_JUMP_IF_NONE
      | INSTRUMENTED_POP_JUMP_IF_NOT_NONE | INSTRUMENTED_POP_JUMP_IF_TRUE
      | INSTRUMENTED_RESUME | INSTRUMENTED_RETURN_CONST
      | INSTRUMENTED_RETURN_VALUE | INSTRUMENTED_YIELD_VALUE ) as op ->
        unsupported op
  done;
  flush ();
  let total = Dynarray.length out in
  new_idx.(n) <- total;
  for k = n - 1 downto 0 do
    if new_idx.(k) = -1 then new_idx.(k) <- new_idx.(k + 1)
  done;
  let remap t = new_idx.(t) in
  let instrs =
    Array.map
      (fun (ins, _) ->
        match ins with
        | Jump t -> Jump (remap t)
        | Cond_jump c -> Cond_jump { c with target = remap c.target }
        | For_iter t -> For_iter (remap t)
        | Send s -> Send { s with on_stop = remap s.on_stop }
        | other -> other)
      (Dynarray.to_array out)
  in
  let olds = Array.map snd (Dynarray.to_array out) in
  let lines = Array.map (fun o -> ast.lines.(o)) olds in
  let positions =
    if Array.length ast.positions = 0 then [||]
    else Array.map (fun o -> ast.positions.(o)) olds
  in
  let exn_table =
    Array.map
      (fun (en : Ast.exn_entry) ->
        {
          en with
          start_idx = remap en.start_idx;
          end_idx = remap en.end_idx;
          target_idx = remap en.target_idx;
        })
      ast.exn_table
  in
  Array.iter
    (fun ins ->
      match ins with
      | Jump t | For_iter t -> assert (t >= 0 && t < total)
      | Cond_jump { target; _ } -> assert (target >= 0 && target < total)
      | Send { on_stop; _ } -> assert (on_stop >= 0 && on_stop < total)
      | _ -> ())
    instrs;
  {
    (* Same frame as the input; constants and names are folded into the
       instructions, so their tables become empty. *)
    Ast.filename = ast.filename;
    name = ast.name;
    qualname = ast.qualname;
    docstring = ast.docstring;
    firstlineno = ast.firstlineno;
    argcount = ast.argcount;
    posonlyargcount = ast.posonlyargcount;
    kwonlyargcount = ast.kwonlyargcount;
    nlocals = ast.nlocals;
    stacksize = ast.stacksize;
    flags = ast.flags;
    consts = [||];
    names = [||];
    localsplus = ast.localsplus;
    instrs;
    exn_table;
    lines;
    positions;
  }

(* ------------------------------------------------------------------ *)
(* Pretty-printing                                                     *)
(* ------------------------------------------------------------------ *)

let binop_str op inplace =
  let s =
    match op with
    | Add -> "+"
    | And -> "&"
    | Floor_div -> "//"
    | Lshift -> "<<"
    | Mat_mul -> "@"
    | Mul -> "*"
    | Mod -> "%"
    | Or -> "|"
    | Pow -> "**"
    | Rshift -> ">>"
    | Sub -> "-"
    | Div -> "/"
    | Xor -> "^"
  in
  if inplace then s ^ "=" else s

let cmpop_str = function
  | Lt -> "<"
  | Le -> "<="
  | Eq -> "=="
  | Ne -> "!="
  | Gt -> ">"
  | Ge -> ">="

let unop_str = function
  | Negative -> "neg"
  | Not -> "not"
  | Invert -> "invert"
  | To_bool -> "to_bool"

let conv_str = function
  | Str_conv -> "str"
  | Repr_conv -> "repr"
  | Ascii_conv -> "ascii"

let cond_str = function
  | If_true -> "if_true"
  | If_false -> "if_false"
  | If_none -> "if_none"
  | If_not_none -> "if_not_none"

let func_attr_str = function
  | Defaults -> "defaults"
  | Kw_defaults -> "kwdefaults"
  | Annotations -> "annotations"
  | Closure -> "closure"

let intrinsic_1_str = function
  | Print -> "print"
  | Import_star -> "import_star"
  | Stopiteration_error -> "stopiteration_error"
  | Async_gen_wrap -> "async_gen_wrap"
  | Unary_positive -> "unary_positive"
  | List_to_tuple -> "list_to_tuple"
  | Typevar -> "typevar"
  | Paramspec -> "paramspec"
  | Typevartuple -> "typevartuple"
  | Subscript_generic -> "subscript_generic"
  | Typealias -> "typealias"

let intrinsic_2_str = function
  | Prep_reraise_star -> "prep_reraise_star"
  | Typevar_with_bound -> "typevar_with_bound"
  | Typevar_with_constraints -> "typevar_with_constraints"
  | Set_function_type_params -> "set_function_type_params"
  | Set_typeparam_default -> "set_typeparam_default"

let var_str lp = function
  | Fast i ->
      if i >= 0 && i < Array.length lp then fst lp.(i)
      else "fast#" ^ string_of_int i
  | Deref i ->
      "deref:"
      ^
      if i >= 0 && i < Array.length lp then fst lp.(i)
      else "#" ^ string_of_int i
  | Name s -> "name:" ^ s
  | Global s -> "global:" ^ s

let value_str lp = function
  | Stack -> "stack"
  | Null -> "null"
  | Const c -> Format.asprintf "%a" Ast.pp_const c
  | Code c -> "<code " ^ c.qualname ^ ">"
  | Var v -> var_str lp v

let instr_str lp ins =
  let v = value_str lp in
  let vs values = String.concat ", " (List.map v (Array.to_list values)) in
  let app name args = name ^ "(" ^ String.concat ", " args ^ ")" in
  let int = string_of_int in
  match ins with
  | Assign (x, value) -> app "Assign" [ var_str lp x; v value ]
  | Delete x -> app "Delete" [ var_str lp x ]
  | Push value -> app "Push" [ v value ]
  | Pop_top value -> app "Pop_top" [ v value ]
  | Copy n -> app "Copy" [ int n ]
  | Swap n -> app "Swap" [ int n ]
  | Unary (op, value) -> app "Unary" [ unop_str op; v value ]
  | Binary_op { op; inplace; l; r } ->
      app "Binary_op" [ binop_str op inplace; v l; v r ]
  | Compare { op; coerce_bool; l; r } ->
      app "Compare"
        [ (cmpop_str op ^ if coerce_bool then " as bool" else ""); v l; v r ]
  | Is_op { invert; l; r } ->
      app (if invert then "Is_not" else "Is") [ v l; v r ]
  | Contains_op { invert; item; seq } ->
      app (if invert then "Not_in" else "In") [ v item; v seq ]
  | Subscript (obj, index) -> app "Subscript" [ v obj; v index ]
  | Store_subscr { value; obj; index } ->
      app "Store_subscr" [ v value; v obj; v index ]
  | Delete_subscr { obj; index } -> app "Delete_subscr" [ v obj; v index ]
  | Binary_slice { obj; start; stop } ->
      app "Binary_slice" [ v obj; v start; v stop ]
  | Store_slice { value; obj; start; stop } ->
      app "Store_slice" [ v value; v obj; v start; v stop ]
  | Load_attr { obj; name; meth } ->
      app (if meth then "Load_method" else "Load_attr") [ v obj; name ]
  | Load_super_attr { super; cls; self; name; _ } ->
      app "Load_super_attr" [ v super; v cls; v self; name ]
  | Store_attr { value; obj; name } -> app "Store_attr" [ v value; v obj; name ]
  | Delete_attr { obj; name } -> app "Delete_attr" [ v obj; name ]
  | Call { f; self; args } -> app "Call" [ v f; v self; "[" ^ vs args ^ "]" ]
  | Call_kw { f; self; args; kw_names } ->
      app "Call_kw" [ v f; v self; "[" ^ vs args ^ "]"; v kw_names ]
  | Call_ex { f; null; args; kwargs } ->
      app "Call_ex"
        ([ v f; v null; v args ]
        @ match kwargs with Some k -> [ v k ] | None -> [])
  | Intrinsic_1 (id, value) -> app "Intrinsic_1" [ intrinsic_1_str id; v value ]
  | Intrinsic_2 (id, a, b) -> app "Intrinsic_2" [ intrinsic_2_str id; v a; v b ]
  | Jump t -> app "Jump" [ int t ]
  | Cond_jump { cond; v = value; target } ->
      app "Cond_jump" [ cond_str cond; v value; int target ]
  | Get_iter value -> app "Get_iter" [ v value ]
  | For_iter t -> app "For_iter" [ int t ]
  | End_for -> "End_for"
  | Get_yield_from_iter value -> app "Get_yield_from_iter" [ v value ]
  | Return value -> app "Return" [ v value ]
  | Return_generator -> "Return_generator"
  | Yield { v = value; _ } -> app "Yield" [ v value ]
  | Send { v = value; on_stop } -> app "Send" [ v value; int on_stop ]
  | End_send -> "End_send"
  | Cleanup_throw -> "Cleanup_throw"
  | Raise { exc; cause } ->
      app "Raise" (List.filter_map (Option.map v) [ exc; cause ])
  | Reraise n -> app "Reraise" [ int n ]
  | Push_exc_info -> "Push_exc_info"
  | Pop_except -> "Pop_except"
  | Check_exc_match value -> app "Check_exc_match" [ v value ]
  | Check_eg_match { exc; pattern } -> app "Check_eg_match" [ v exc; v pattern ]
  | With_except_start -> "With_except_start"
  | Build_tuple values -> app "Build_tuple" [ "[" ^ vs values ^ "]" ]
  | Build_list values -> app "Build_list" [ "[" ^ vs values ^ "]" ]
  | Build_set values -> app "Build_set" [ "[" ^ vs values ^ "]" ]
  | Build_map pairs ->
      app "Build_map"
        (List.map
           (fun (k, value) -> v k ^ ": " ^ v value)
           (Array.to_list pairs))
  | Build_string values -> app "Build_string" [ "[" ^ vs values ^ "]" ]
  | Build_slice values -> app "Build_slice" [ "[" ^ vs values ^ "]" ]
  | Build_const_key_map { keys; values } ->
      app "Build_const_key_map" [ v keys; "[" ^ vs values ^ "]" ]
  | List_append (d, value) -> app "List_append" [ int d; v value ]
  | Set_add (d, value) -> app "Set_add" [ int d; v value ]
  | Map_add (d, k, value) -> app "Map_add" [ int d; v k; v value ]
  | List_extend (d, value) -> app "List_extend" [ int d; v value ]
  | Set_update (d, value) -> app "Set_update" [ int d; v value ]
  | Dict_update (d, value) -> app "Dict_update" [ int d; v value ]
  | Dict_merge (d, value) -> app "Dict_merge" [ int d; v value ]
  | Unpack_sequence (count, value) ->
      app "Unpack_sequence" [ int count; v value ]
  | Unpack_ex { before; after; v = value } ->
      app "Unpack_ex" [ int before; int after; v value ]
  | Format_simple value -> app "Format_simple" [ v value ]
  | Format_with_spec (value, spec) -> app "Format_with_spec" [ v value; v spec ]
  | Convert_value (c, value) -> app "Convert_value" [ conv_str c; v value ]
  | Make_function value -> app "Make_function" [ v value ]
  | Set_function_attribute { attr; v = value; f } ->
      app "Set_function_attribute" [ func_attr_str attr; v value; v f ]
  | Make_cell slot -> app "Make_cell" [ var_str lp (Fast slot) ]
  | Copy_free_vars n -> app "Copy_free_vars" [ int n ]
  | Load_build_class -> "Load_build_class"
  | Load_assertion_error -> "Load_assertion_error"
  | Setup_annotations -> "Setup_annotations"
  | Load_locals -> "Load_locals"
  | Load_from_dict_or_globals (value, name) ->
      app "Load_from_dict_or_globals" [ v value; name ]
  | Load_from_dict_or_deref (value, slot) ->
      app "Load_from_dict_or_deref" [ v value; var_str lp (Deref slot) ]
  | Load_fast_and_clear slot ->
      app "Load_fast_and_clear" [ var_str lp (Fast slot) ]
  | Import_name { name; level; from_list } ->
      app "Import_name" [ name; v level; v from_list ]
  | Import_from name -> app "Import_from" [ name ]
  | Get_awaitable (where, value) -> app "Get_awaitable" [ int where; v value ]
  | Get_aiter value -> app "Get_aiter" [ v value ]
  | Get_anext -> "Get_anext"
  | End_async_for -> "End_async_for"
  | Before_with value -> app "Before_with" [ v value ]
  | Before_async_with value -> app "Before_async_with" [ v value ]
  | Match_class { count; subject; cls; names } ->
      app "Match_class" [ int count; v subject; v cls; v names ]
  | Match_mapping -> "Match_mapping"
  | Match_sequence -> "Match_sequence"
  | Match_keys -> "Match_keys"
  | Get_len -> "Get_len"

(* Reuse the shared bytecode renderer: Phir only supplies its instruction column
   and how to find nested code objects (which live inside instruction operands
   now that constants are folded, not in a [consts] table). *)
let pp_code fmt c =
  let render_instr (c : code) ins = instr_str c.localsplus ins in
  let children (c : code) =
    Array.to_list c.instrs
    |> List.concat_map (fun ins ->
        List.filter_map (function Code n -> Some n | _ -> None) (values ins))
  in
  let buf = Buffer.create 4096 in
  Ast.render_generic ~render_instr ~children buf c;
  Format.pp_print_string fmt (Buffer.contents buf)

let instr_to_string = instr_str
