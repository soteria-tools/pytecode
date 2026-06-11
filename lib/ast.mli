(** Pure OCaml representation of CPython bytecode.

    The structure mirrors CPython code objects as closely as possible, with
    exactly one normalization applied at extraction time (see
    [doc/normalization.md]):

    - [CACHE] and [EXTENDED_ARG] entries are stripped, args are
      EXTENDED_ARG-folded;
    - consequently byte offsets are meaningless, so jump args and
      exception-table boundaries are {b absolute instruction indices} into
      {!field:code.instrs}.

    Everything else keeps raw CPython semantics: instruction args index into the
    code object's tables ([consts]/[names]/[localsplus]), including flag-bit
    encodings (e.g. LOAD_GLOBAL's low "push NULL" bit, LOAD_ATTR's low method
    bit, COMPARE_OP's packed operation). *)

type instr = { op : Opcode.t; arg : int }
(** A single instruction. [arg] is CPython's raw (EXTENDED_ARG-folded) oparg, or
    [0] when CPython reports no arg; for jump opcodes ({!Opcode.is_jump}) it is
    the absolute index of the target instruction. *)

type local_kind =
  | Local  (** plain fast local (in [co_varnames]) *)
  | Cell  (** cell for a variable captured by nested code (in [co_cellvars]) *)
  | Local_and_cell
      (** captured parameter: occupies its argument slot, [MAKE_CELL] converts
          it in place (in both [co_varnames] and [co_cellvars]) *)
  | Free  (** free variable of this code, bound in an enclosing scope *)

type exn_entry = {
  start_idx : int;  (** first covered instruction (inclusive) *)
  end_idx : int;  (** first instruction NOT covered (exclusive) *)
  target_idx : int;  (** handler entry point *)
  depth : int;  (** stack depth to restore before jumping to the handler *)
  push_lasti : bool;
      (** push the index of the raising instruction before the exception *)
}
(** One entry of the exception table (CPython 3.11+ zero-cost exception
    handling), with boundaries as instruction indices. *)

type positions = {
  lineno : int;
  end_lineno : int;
  col_offset : int;
  end_col_offset : int;
}
(** Source span of an instruction, from [co_positions]. [-1] marks an absent
    component. *)

type const =
  | None_
  | Bool of bool
  | Int of Z.t
  | Float of float  (** exact, including nan/inf/-0.0 *)
  | Complex of { re : float; im : float }
  | Str of string
      (** UTF-8; Python strings containing lone surrogates are transported as
          their [surrogatepass] (WTF-8) encoding *)
  | Bytes of string
  | Tuple of const array
  | Frozenset of const array
      (** element order: deterministic (sorted by encoded form), {b not}
          CPython's iteration order — semantically a set *)
  | Code of instr code
  | Ellipsis

(* A code object, parameterized over its instruction representation so derived
   IRs (e.g. Phir) reuse the same frame, debug-info and exception-table shape —
   and the shared [render_generic] pretty-printer. Raw bytecode is [instr code];
   IRs that fold constants in (e.g. Phir) leave [consts]/[names] empty. *)
and 'i code = {
  (* identity *)
  filename : string;
  name : string;
  qualname : string;
  docstring : string option;
      (** [co_consts.(0)] when it is a string. Carried explicitly because IRs
          that inline constants (e.g. {!Phir}) drop the [consts] table. *)
  firstlineno : int;
  (* signature / frame shape *)
  argcount : int;
  posonlyargcount : int;
  kwonlyargcount : int;
  nlocals : int;  (** number of [Local]/[Local_and_cell] slots *)
  stacksize : int;
  flags : int;  (** raw [co_flags] *)
  (* tables, indexed by instruction args, CPython-style *)
  consts : const array;
  names : string array;
  localsplus : (string * local_kind) array;
      (** merged fast-locals layout (3.11+): varnames, then cellvars not in
          varnames, then freevars. All [*_FAST]/[*_DEREF]/[MAKE_CELL] args index
          this array directly. *)
  (* code *)
  instrs : 'i array;
  exn_table : exn_entry array;  (** sorted by [start_idx], as CPython emits *)
  (* debug info, struct-of-arrays so the hot loop never touches it *)
  lines : int array;  (** per instruction; [-1] = no line *)
  positions : positions array;
      (** per instruction; [[||]] if dumped without positions *)
}

(** {2 co_flags accessors (CO_* bits, stable across versions)} *)

val is_optimized : 'i code -> bool
(** function-like frame with fast locals (CO_OPTIMIZED) *)

val is_generator : 'i code -> bool
val is_coroutine : 'i code -> bool
val is_async_generator : 'i code -> bool
val has_varargs : 'i code -> bool
val has_varkw : 'i code -> bool
val is_nested : 'i code -> bool

(** {2 Derived legacy views (for tracebacks / inspection)} *)

val varnames : 'i code -> string array
val cellvars : 'i code -> string array
val freevars : 'i code -> string array

(** {2 Pretty-printing (dis-like; for humans and golden tests)} *)

val pp_const : Format.formatter -> const -> unit
val pp_code : Format.formatter -> instr code -> unit

val render_generic :
  render_instr:('i code -> 'i -> string) ->
  children:('i code -> 'i code list) ->
  Buffer.t ->
  'i code ->
  unit
(** Shared dis-like renderer used by both {!pp_code} and derived IRs.
    [render_instr c ins] formats the instruction column (everything after the
    line/index gutter; trailing spaces are trimmed); [children c] lists the
    nested code objects to disassemble depth-first afterwards. *)
