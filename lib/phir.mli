(** Phir — Python High IR.

    A more usable IR obtained by transformation from {!Ast.code}: operands that
    are statically known (constants, variable reads, NULL markers) are folded
    {b into} the instructions that consume them, instead of transiting through
    the operand stack. The operand stack still exists — intermediate results of
    calls/operators flow through it as {!Stack} operands — but straight-line
    code becomes direct:

    {v
      x = 5            Assign(x, 5)
      z = x + y        Binary_op(+, x, y); Assign(z, stack)
      print("hi")      Call(global:print, null, ['hi']); Pop_top(stack)
    v}

    {2 Evaluation contract}

    Operands of an instruction are listed in {b push order} (deepest first, i.e.
    left-to-right in source). Two invariants make interpretation simple and keep
    CPython semantics:

    - {!Stack} operands always form a {b prefix} of an instruction's operand
      list (folding only ever replaces the most recent pushes). Pop them
      right-to-left (the rightmost Stack operand is the top of stack).
    - Folded operands are evaluated {b left-to-right at the instruction}. This
      is sound because the transformation only folds a contiguous run of
      pure-read pushes immediately preceding the consumer, never across a jump
      target or an exception-table boundary, so evaluation order and raise order
      are preserved (a folded read can still raise
      [NameError]/[UnboundLocalError]).

    Jump targets ({!Jump}, {!Cond_jump}, {!For_iter}, {!Send}) and
    exception-table boundaries are absolute indices into {!field:code.instrs},
    exactly as in {!Ast}. [NOP] and [RESUME] are dropped. Superinstructions
    ([STORE_FAST_STORE_FAST], ...) are expanded. Stack-depth semantics of the
    exception table are preserved: folding only removes stack traffic strictly
    inside a protected region, so unwinding to [depth] remains correct. *)

exception Unsupported of string
(** Raised by {!of_code} on opcodes that cannot appear in freshly compiled
    pinned-version code (specialized, instrumented, internal). *)

(** {1 Variables and operator kinds} *)

type var =
  | Fast of int  (** localsplus slot (may raise [UnboundLocalError]) *)
  | Deref of int  (** cell/free localsplus slot: read/write the cell *)
  | Name of string  (** namespace lookup: locals, globals, builtins *)
  | Global of string  (** globals then builtins *)

(** [BINARY_OP], in CPython's [_nb_ops] order. *)
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
type conv = Str_conv | Repr_conv | Ascii_conv  (** [CONVERT_VALUE] *)
type cond = If_true | If_false | If_none | If_not_none

(** [SET_FUNCTION_ATTRIBUTE] flag. *)
type func_attr = Defaults | Kw_defaults | Annotations | Closure

(** [CALL_INTRINSIC_1] ids. *)
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

(** [CALL_INTRINSIC_2] ids. *)
type intrinsic_2 =
  | Prep_reraise_star
  | Typevar_with_bound
  | Typevar_with_constraints
  | Set_function_type_params
  | Set_typeparam_default

(** {1 Code, values, instructions}

    Unless stated otherwise, an instruction pops nothing beyond its [value]
    operands and pushes its result (if it has one) onto the operand stack.
    Operands are in push order. *)

type code = {
  filename : string;
  name : string;
  qualname : string;
  docstring : string option;
      (** [co_consts.(0)] when it is a string — what [MAKE_FUNCTION] exposes as
          [__doc__] (Phir inlines constants, so it must be carried here) *)
  firstlineno : int;
  argcount : int;
  posonlyargcount : int;
  kwonlyargcount : int;
  nlocals : int;
  stacksize : int;  (** upper bound (folding only shrinks the real max) *)
  flags : int;
  localsplus : (string * Ast.local_kind) array;
  instrs : instr array;
  exn_table : Ast.exn_entry array;
  lines : int array;
  positions : Ast.positions array;
}

and value =
  | Stack  (** pop the operand stack *)
  | Null  (** CPython's NULL sentinel ([PUSH_NULL], LOAD_GLOBAL flag) *)
  | Const of Ast.const  (** inlined constant — the [consts] table is gone *)
  | Code of code  (** code constant, recursively transformed *)
  | Var of var  (** read a variable at instruction-evaluation time *)

and instr =
  (* -- data movement -- *)
  | Assign of var * value
  | Delete of var
  | Push of value  (** materialize a value on the operand stack *)
  | Pop_top of value  (** evaluate and discard *)
  | Copy of int  (** push a copy of stack.(-n) *)
  | Swap of int  (** swap top with stack.(-n) *)
  (* -- operators -- *)
  | Unary of unop * value
  | Binary_op of { op : binop; inplace : bool; l : value; r : value }
  | Compare of { op : cmpop; coerce_bool : bool; l : value; r : value }
  | Is_op of { invert : bool; l : value; r : value }
  | Contains_op of { invert : bool; item : value; seq : value }
  | Subscript of value * value  (** object, index *)
  | Store_subscr of { value : value; obj : value; index : value }
  | Delete_subscr of { obj : value; index : value }
  | Binary_slice of { obj : value; start : value; stop : value }
  | Store_slice of { value : value; obj : value; start : value; stop : value }
  (* -- attributes -- *)
  | Load_attr of { obj : value; name : string; meth : bool }
      (** pushes 1 value, or 2 ([method, self_or_null]) when [meth] *)
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
  (* -- calls -- *)
  | Call of { f : value; self : value; args : value array }
      (** [self] is {!Null} for plain calls; a bound receiver when the callee
          came from a method-flavored [LOAD_ATTR] *)
  | Call_kw of { f : value; self : value; args : value array; kw_names : value }
      (** the last [length kw_names] args are keyword values *)
  | Call_ex of { f : value; null : value; args : value; kwargs : value option }
  | Intrinsic_1 of intrinsic_1 * value
  | Intrinsic_2 of intrinsic_2 * value * value
  (* -- control flow -- *)
  | Jump of int
  | Cond_jump of { cond : cond; v : value; target : int }
  | Get_iter of value
  | For_iter of int
      (** peeks the iterator at top of stack; pushes the next item, or on
          exhaustion pushes a dummy and jumps to the target ([End_for] +
          [Pop_top] there clean up) *)
  | End_for  (** pops 1 *)
  | Get_yield_from_iter of value
  | Return of value
  | Return_generator
  | Yield of { v : value; arg : int }
  | Send of { v : value; on_stop : int }  (** peeks receiver below [v] *)
  | End_send  (** pops 2, pushes the result *)
  | Cleanup_throw
  (* -- exceptions (implicit stack contracts, as in CPython) -- *)
  | Raise of { exc : value option; cause : value option }
  | Reraise of int
  | Push_exc_info
  | Pop_except
  | Check_exc_match of value  (** pops the pattern, peeks the exception *)
  | Check_eg_match of { exc : value; pattern : value }
  | With_except_start
  (* -- builds -- *)
  | Build_tuple of value array
  | Build_list of value array
  | Build_set of value array
  | Build_map of (value * value) array
  | Build_string of value array
  | Build_slice of value array  (** 2 or 3 items *)
  | Build_const_key_map of { keys : value; values : value array }
  | List_append of int * value  (** container peeked at given stack depth *)
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
  (* -- functions, classes, scopes -- *)
  | Make_function of value  (** operand is typically {!Code} *)
  | Set_function_attribute of { attr : func_attr; v : value; f : value }
      (** pops both, re-pushes the function *)
  | Make_cell of int
  | Copy_free_vars of int
  | Load_build_class
  | Load_assertion_error
  | Setup_annotations
  | Load_locals
  | Load_from_dict_or_globals of value * string
  | Load_from_dict_or_deref of value * int
  | Load_fast_and_clear of int
      (** pushes the slot content (may be NULL), clears the slot *)
  (* -- imports -- *)
  | Import_name of { name : string; level : value; from_list : value }
  | Import_from of string  (** peeks the module, pushes the attribute *)
  (* -- async -- *)
  | Get_awaitable of int * value
  | Get_aiter of value
  | Get_anext  (** peeks, pushes awaitable *)
  | End_async_for  (** pops 2 *)
  | Before_with of value  (** pushes [__exit__], [__enter__ ()] *)
  | Before_async_with of value
  (* -- pattern matching -- *)
  | Match_class of { count : int; subject : value; cls : value; names : value }
  | Match_mapping  (** peeks, pushes bool *)
  | Match_sequence
  | Match_keys  (** peeks subject and keys, pushes values or None *)
  | Get_len

val of_code : Ast.code -> code
(** Transform bytecode into Phir, recursively (code constants included). Raises
    {!Unsupported} on opcodes that cannot occur in freshly compiled code of the
    pinned CPython. *)

val values : instr -> value list
(** The operand values of an instruction, in push order (analysis aid). *)

(** {2 co_flags accessors} *)

val is_generator : code -> bool
val is_coroutine : code -> bool
val is_async_generator : code -> bool
val has_varargs : code -> bool
val has_varkw : code -> bool

(** {2 Pretty-printing} *)

val pp_code : Format.formatter -> code -> unit

val instr_to_string : (string * Ast.local_kind) array -> instr -> string
(** One instruction, rendered with the given localsplus for slot names — for
    error messages and debugging. *)
