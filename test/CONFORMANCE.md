# Language Reference conformance plan

Goal: every behaviour described in Python Language Reference sections **3, 6, 7,
8, 9** is supported by the pytecode interpreter and covered by a differential
test (`test/programs/<section>/...`) whose stdout matches CPython 3.13
**byte for byte** (`dune build @interp`).

Method: for each subsection/subsubsection, read the reference paragraphs (source
of truth: `/tmp/cpython/Doc/reference/*.rst`, 3.13 branch), write tests for each
observable behaviour, run `@interp`, and implement any interpreter gap until the
new tests pass and nothing else breaks.

Legend: ✅ done & passing · 🔄 in progress · ⬜ todo · ⛔ N/A (not observable
via stdout / internal type / impl-detail / out of interpreter scope).

Notes on scope:
- Identity (`is`) is testable; `id()` values are addresses and impl-specific, so
  identity is tested via `is`, not `id()` output.
- Garbage collection, threading, finalization timing, memory layout, and
  CPython impl-details are ⛔.

**Explicitly OUT OF SCOPE for this autonomous loop (user-directed):**
- **async / await / coroutines** (3.4, 6.4, 8.9) — do NOT implement.
- **import system** (7.11) — deferred; will be done together with the user later.
- **eval / exec** (9.4 expression input) — do NOT implement.
These are excluded from "all behaviour"; everything else in 3/6/7/8/9 is in scope.

---

## Section status

| Sec | Title | Status |
|-----|-------|--------|
| 3 | Data model | ✅ substantially complete (async backlogged) |
| 6 | Expressions | ✅ substantially complete (6.4 await backlogged) |
| 7 | Simple statements | ✅ substantially complete (7.11 import deferred) |
| 8 | Compound statements | ✅ substantially complete (async + generic classes backlogged) |
| 9 | Top-level components | ✅ substantially complete (9.3 interactive N/A, 9.4 eval backlogged) |

---

## 3. Data model  → `test/programs/3_data_model/`

### 3.1 Objects, values and types
- ✅ 3.1 identity/type/value; `is`; `type()`; mutability; `[] is []` False;
  `e=f=[]` aliases; immutable-tuple-holding-mutable → `3.1_.../objects_identity_type.py`

### 3.2 The standard type hierarchy
- ✅ 3.2.1 None — `3.2.1_none/none.py`
- ✅ 3.2.2 NotImplemented — `3.2.2_notimplemented/notimplemented.py`
- ✅ 3.2.3 Ellipsis — `3.2.3_ellipsis/ellipsis.py`
- ✅ 3.2.4 numbers.Number (repr properties) — `3.2.4_numbers/numeric_repr.py`
- ✅ 3.2.4.1 Integral (int, bool) — `3.2.4.1_integral/integral_int_bool.py`
- ✅ 3.2.4.2 Real (float, incl. inf/nan) — `3.2.4.2_real/real_float.py`
- ✅ 3.2.4.3 Complex — `3.2.4.3_complex/complex_numbers.py` (new `Complex` value type)
- ✅ 3.2.5 Sequences (common: len/index/neg/slice/extended) — `3.2.5_sequences/`
- ✅ 3.2.5.1 Immutable sequences (str, tuple, **bytes**) — `3.2.5.1_immutable_sequences/`
- ✅ 3.2.5.2 Mutable sequences (list + slice/extended-slice assign & del,
  **bytearray**) — `3.2.5.2_mutable_sequences/`
- ✅ 3.2.6 Set types — `3.2.6_set_types/` (set: uniqueness, numeric coincidence,
  no subscript, algebra; **frozenset**: distinct type, immutable+hashable, repr,
  set algebra & subset across set/frozenset, ==, dict-key/set-elem, result-type
  follows left operand; named methods union/intersection/difference/
  symmetric_difference/issubset/issuperset/isdisjoint/update/pop/clear over any
  iterable — `3.2.6_set_types/set_methods.py`)
- ✅ 3.2.7 Mappings (get/set/del/len, KeyError) — `3.2.7_mappings/`
- ✅ 3.2.7.1 Dictionaries (numeric-key coincidence, insertion order, unhashable
  keys rejected; methods get/pop/setdefault/update(+kwargs)/copy/clear/popitem,
  PEP 584 `|`/`|=` union) — `3.2.7.1_dictionaries/dict_ops.py`
- ✅ 3.2.8 Callable types (function attrs, bound `__self__`/`__func__`,
  generator-returns-iterator, class-is-callable) — `3.2.8_callable_types/`
- ⛔ 3.2.9 Modules (import-related; deferred)
- ✅ 3.2.10 Custom classes (__name__/__qualname__/__bases__/__mro__/__dict__/__doc__,
  attr assignment, class-call, classmethod/staticmethod) — `3.2.10_custom_classes/`
- ✅ 3.2.11 Class instances (__class__/__dict__, lookup order, del, __getattr__) — `3.2.11_class_instances/`
- ⛔ 3.2.12 I/O objects
- ✅ 3.2.13 Internal types — `slice` objects (`slice()`, start/stop/step, repr,
  ==, extended-slice `__getitem__` key) — `3.2.13_internal_types/`;
  code/frame/traceback/static/class-method objects ⛔

### 3.3 Special method names  → subsubsection folders
- ✅ 3.3.1 Basic customization — `3.3.1_basic_customization/` (new_and_init,
  repr_str_format, rich_comparison, bool_and_len, str_repr, eq_lt_sorting):
  __new__/__init__ (+return-None check), __repr__/__str__/__format__ (+object
  default + error msgs), rich comparisons incl. custom __ne__ & NotImplemented
  fallback, __bool__/__len__. ⛔ __del__ (GC timing) · ⬜ __hash__ (see backlog)
  · ⬜ subclass-reflected-priority rule (see backlog) · ⛔ __bytes__ (needs bytes)
- 🔄 3.3.2 Customizing attribute access:
  - ✅ __getattr__/__getattribute__/__setattr__/__delattr__/__dir__ —
    `3.3.2_customizing_attribute_access/attribute_access.py` (+property,
    getattr_fallback). dir() builtin added.
  - ✅ 3.3.2.3/3.3.2.4 Implementing/Invoking Descriptors (__get__/__set__/
    __delete__, data vs non-data priority, class binding) —
    `3.3.2.3_descriptors/descriptors.py`. ⬜ __set_name__
  - ✅ 3.3.2.5 __slots__ — `3.3.2.5_slots/slots.py`: a fully-slotted instance
    rejects non-slot attribute assignment ("…and no __dict__ for setting new
    attributes") and has no __dict__; single-string slots; a subclass without
    __slots__ regains a __dict__. (Slot values are stored in the instance dict
    internally, so a mixed subclass's __dict__ also lists inherited-slot names —
    untested detail.)
  - ⛔ Customizing module attribute access (deferred w/ imports)
- ✅ 3.3.3 Customizing class creation:
  - ✅ __init_subclass__ (incl. keyword args + object default error) and
    __set_name__ — `3.3.3_customizing_class_creation/init_subclass_set_name.py`
  - ✅ 3.3.3.1 Resolving MRO entries: a non-class base is replaced by its
    __mro_entries__(bases); __orig_bases__ recorded —
    `3.3.3_customizing_class_creation/mro_entries.py`
  - ✅ metaclasses (metaclass= selecting a type subclass; metaclass __new__/
    __init__/__call__; 3.3.3.2 most-derived determination + conflict error;
    3.3.3.3 __prepare__ returning the body namespace) —
    `3.3.3_customizing_class_creation/metaclasses.py`. NOTE: __prepare__
    returning a non-dict mapping is NOT supported.
- ✅ 3.3.4 Customizing instance/subclass checks (metaclass __instancecheck__/
  __subclasscheck__, looked up on the metaclass only; tuple 2nd arg) —
  `3.3.4_instance_subclass_checks/instance_subclass_checks.py`
- ✅ 3.3.5 Emulating generic types (__class_getitem__, implicit classmethod,
  vs instance __getitem__) — `3.3.5_emulating_generic_types/class_getitem.py`;
  built-in generic aliases (`list[int]`, `dict[str,int]`, `tuple[int,...]`,
  nested) producing `types.GenericAlias` with __origin__/__args__ —
  `3.3.5_emulating_generic_types/generic_aliases.py`
- ✅ 3.3.6 Emulating callable objects (__call__) — `3.3.6_emulating_callable_objects/callable.py`
- ✅ 3.3.7 Emulating container types (__len__/__getitem__/__setitem__/__delitem__/
  __iter__/__reversed__/__contains__, reversed() incl. len+getitem fallback,
  in→iteration fallback) — `3.3.7_emulating_container_types/`. ✅ __missing__
  (dict subclass) — `3.3.7_emulating_container_types/dict_subclass_missing.py` ·
  ⬜ __length_hint__ (no operator.length_hint)
- ✅ 3.3.8 Emulating numeric types — `3.3.8_emulating_numeric_types/`
  (binary_and_unary, reflected_and_inplace, conversions, operator_overload,
  divmod_and_index): all binary __op__/__rop__/__iop__, __neg__/__abs__/
  __invert__, __int__/__float__/__index__ (incl. index in subscripting and
  hex/bin/oct), __round__, and __divmod__/__rdivmod__ via divmod().
  ⬜ __pos__ (no UNARY_POSITIVE in Phir) · ⬜ ternary __pow__
- ✅ 3.3.9 With Statement Context Managers (__enter__/__exit__, exc args,
  suppression) — `3.3.9_with_statement_context_managers/`
- ✅ 3.3.10 __match_args__ (positional class patterns + over-count TypeError) —
  `3.3.10_match_args/`
- ⛔ 3.3.11 Emulating buffer types (needs bytes/buffer protocol)
- ✅ 3.3.12 Special method lookup (implicit lookup on the type, not the instance
  dict) — `3.3.12_special_method_lookup/`
- ⛔ 3.4 Coroutines (async)

**Section 3 substantially COMPLETE** (149 tests). Remaining gaps are backlogged
type/feature work: metaclasses (3.3.4, 3.3.3 advanced), __slots__ (3.3.2.5),
bytes/buffer (3.3.11), async (3.4).

---

## 6. Expressions  → `test/programs/6_expressions/`
- ✅ 6.1 Arithmetic conversions (complex>float>int common type) — `6.1_arithmetic_conversions/`
- 🔄 6.2 Atoms:
  - ✅ 6.2.1 Identifiers + private name mangling — `6.2.1_identifiers/`
  - ✅ 6.2.2 Literals (str/fstring tests; full format-spec mini-language in
    `format_spec.py`: fill/align, sign +/-/space, # alternate form, 0-pad,
    width, `,`/`_` grouping, precision, d/x/X/o/b/f/e/E/g/G/%/s; `ascii_builtin.py`)
  - ✅ 6.2.3 Parenthesized forms (comma makes tuples) — `6.2.3_parenthesized_forms/`
  - ✅ 6.2.4 Displays/comprehension scoping (no leak, multi-for) — `6.2.4_displays/`
  - ✅ 6.2.5 List displays (existing) · ✅ 6.2.6 Set displays ({} is dict) · ✅ 6.2.7 Dict displays (** unpack, dup keys, comp)
  - ✅ 6.2.8 Generator expressions (existing) · ✅ 6.2.9 Yield expressions
    (yield/yield from/send + `generator.throw` injecting at the yield, and
    `StopIteration.value` carrying the return value) — `6.2.9_yield_expressions/`
- 🔄 6.3 Primaries: ✅ 6.3.1 Attribute refs (existing) · 🔄 6.3.2 Subscriptions ·
  ✅ 6.3.3 Slicings (existing; range indexing/slicing → new range,
  `6.3.3_slicings/range_slicing.py`; zero-step → ValueError centrally) ·
  ✅ 6.3.4 Calls — `6.3.4_calls/call_binding.py`
  (binding, defaults, */** unpack, * before kwargs, CPython error messages)
- ⛔ 6.4 Await expression
- ✅ 6.5 The power operator (right-assoc, neg-exp→float, 0**neg→ZeroDivisionError) — `6.5_power_operator/`
- ✅ 6.6 Unary arithmetic/bitwise (-, +, ~) — `6.5_power_operator/power_operator.py`
- ✅ 6.7 Binary arithmetic operations *(strong coverage)*
- ✅ 6.8 Shifting operations (x<<n, x>>n floor, negative count ValueError) — `6.8_shifting/`
- ✅ 6.9 Binary bitwise operations (&/^/| + precedence) — `6.9_binary_bitwise_operations/`;
  PEP 604 `X | Y` type unions → `types.UnionType` (None→NoneType, flatten,
  de-dup, single-member collapse, isinstance) — `6.9_binary_bitwise_operations/union_types.py`;
  printf-style `str % args` (positional/single/mapping; d/i/u/o/x/X/e/E/f/g/G/c/r/s/a/%;
  flags -+ #0, width, precision; arg-count errors) — `printf_formatting.py`
- ✅ 6.10 Comparisons — `6.10_comparisons/value_comparisons.py`,
  `6.10.2_.../membership.py`, `6.10.3_.../identity.py`: chaining/short-circuit,
  cross-type numeric eq, NaN ordering (all false incl. <=/>=), default eq=identity
  & order TypeError, sequence lexicographic, `x is e or x==e` membership.
- ✅ 6.11 Boolean operations (and/or return operands, short-circuit, not→bool) — `6.11_boolean_operations/`
- ✅ 6.12 Assignment expressions (walrus, existing) · ✅ 6.13 Conditional (existing) · ✅ 6.14 Lambdas (existing)
- ✅ 6.15 Expression lists (* unpacking in displays) — `6.15_expression_lists/`
- ✅ 6.16 Evaluation order (left-to-right; RHS before LHS) — `6.16_evaluation_order/`
- ✅ 6.17 Operator precedence — `6.17_operator_precedence/`

**Section 6 substantially COMPLETE** (167 tests). 6.4 Await ⛔ (async).

## 7. Simple statements  → `test/programs/7_simple_statements/`
- ✅ 7.1 Expression statements (no auto-print in script mode) — `7.1_expression_statements/`
- ✅ 7.2 Assignment (chained, unpack, overlap order, attr target, unpack errors) — `7.2_assignment_statements/`
- ✅ 7.2.1 Augmented (target once, LHS before RHS, in-place) — `7.2.1_augmented_assignment/`
- ✅ 7.2.2 Annotated (__annotations__ at module/class, bare annotation unbound) — `7.2.2_annotated_assignment/`
- ✅ 7.3 assert (+ __debug__, msg) — `7.3_assert_statement/` · ✅ 7.4 pass — `7.4_pass_statement/`
- ✅ 7.5 del (existing) · ✅ 7.6 return (+finally override) — `7.6_return_statement/` ·
  ✅ 7.7 yield (gen tests) · ✅ 7.8 raise (chaining: __cause__/__context__/
  __suppress_context__, bare reraise) — `7.8_raise_statement/` ·
  ✅ 7.9 break · ✅ 7.10 continue (existing while/for tests)
- ⛔ 7.11 import (deferred)
- ✅ 7.12 global — `7.12_global_statement/` · ✅ 7.13 nonlocal (existing) ·
  ✅ 7.14 type statement (PEP 695): `type X = ...` → `typing.TypeAliasType` via
  the `typealias` intrinsic; __name__/__value__ (lazy)/__type_params__ —
  `7.14_type_statement/type_aliases.py`

**Section 7 substantially COMPLETE**. 7.11 import deferred; 7.14 type DONE.

## 8. Compound statements  → `test/programs/8_compound_statements/`
- ✅ 8.1 if (existing) · ✅ 8.2 while (else/break/continue) — `8.2_while_statement/` ·
  ✅ 8.3 for (target overwrite/persist/empty, starred) — `8.3_for_statement/`
- ✅ 8.4 try (except order/else/finally, return-in-finally discards exc) — `8.4_try_statement/`;
  ✅ except* / exception groups (PEP 654): BaseExceptionGroup/ExceptionGroup,
  split/subgroup/derive, Check_eg_match (wrap bare match, split groups),
  prep_reraise_star (combine leftovers/handler-raises) — `8.4_try_statement/except_star.py`
- ✅ 8.5 with (multiple/parenthesized managers, reverse exit, suppression) — `8.5_with_statement/`
- ✅ 8.6 match (OR/guard/AS/capture/wildcard/value/mapping-**rest/starred) — `8.6_match_statement/`
- ✅ 8.7 Function definitions (decorator order, posonly/kwonly binding + errors,
  `__annotations__` collected in order + empty default, decorators+defaults) — `8.7_function_definitions/`
- ✅ 8.8 Class definitions (class decorators, body exec/order, instance-hides-class) — `8.8_class_definitions/`
- ⛔ 8.9 Coroutines (async)
- ✅ 8.10 Type parameter lists (PEP 695): `def f[T]` and `type X[T] = …`
  introduce TypeVars (incl. `[T: bound]` and `[T: (constraints)]`, lazily
  evaluated); __type_params__ on functions/aliases; TypeVar __name__/__bound__/
  __constraints__ — `8.10_type_parameter_lists/type_params.py`. ⛔ generic
  *classes* `class C[T]` (need typing.Generic via `subscript_generic`; deferred
  with the typing/import work).

**Section 8 substantially COMPLETE**. except* (8.4) + type-params (8.10, sans
generic classes) DONE; async (8.9) backlogged.

## 9. Top-level components  → `test/programs/9_top_level_components/`
- ✅ 9.1 Complete Python programs (__name__=="__main__", top-to-bottom, blank
  lines) — `9.1_complete_programs/` · ✅ 9.2 File input (statement sequence,
  demonstrated) · ⛔ 9.3 Interactive (REPL) · ⬜ 9.4 Expression input (eval — backlog)

**Section 9 substantially COMPLETE** (184 tests). 9.3 interactive N/A; 9.4 eval backlogged.

---

## Progress log
- **int/float methods + numeric-tower attributes** (216/216 @interp; all gates
  green): `int.to_bytes`/`int.from_bytes` (big/little, overflow errors),
  `bit_count`, `conjugate`; `float.conjugate`/`as_integer_ratio` (exact, via
  `float_as_integer_ratio`); and the read-only attributes real/imag/numerator/
  denominator on int/bool/float. Test: `3.2.4.1_integral/int_methods.py`.
  (Explicit dunder-method calls like `(5).__index__()` and `float.fromhex`
  remain niche gaps.)
- **bytes byte-oriented methods** (215/215 @interp; all gates green): added
  bytes `upper`/`lower`/`split`/`replace`/`startswith`/`endswith`/`find`/`rfind`/
  `index`/`count`/`strip`/`lstrip`/`rstrip`/`hex`/`join` (reusing the str byte
  helpers; results stay bytes; bytes-specific "subsection not found"). Test:
  `3.2.5.1_immutable_sequences/bytes_methods.py`. GAP: bytes `%`-formatting
  (`b"%d" % …`) not modelled (the `%b` spec doesn't map onto the str printf).
- **Power-operator edge cases** (214/214 @interp; all gates green): finite
  `float ** float` that overflows now raises `OverflowError(34, 'Result too
  large')`; a negative base to a non-integer power yields a complex result (added
  general complex power `z**w = exp(w·log z)` to `complex_binop`, also filling
  the prior "complex ** non-integer" gap); fixed `int % 0` message to "integer
  modulo by zero" (vs `//`/divmod's "...division or modulo..."). Test:
  `6.5_power_operator/pow_edge_cases.py`.
- **Full exception hierarchy** (213/213 @interp; all gates green): expanded
  `exception_tree` from 19 to ~65 classes — the complete CPython 3.13 builtin
  exception hierarchy (GeneratorExit/KeyboardInterrupt/SystemExit under
  BaseException; OverflowError/FloatingPointError, the OSError family
  (FileNotFoundError/PermissionError/ConnectionError+children/…),
  RecursionError, UnicodeError family, SyntaxError/IndentationError/TabError,
  Warning family, MemoryError/BufferError/EOFError/ReferenceError/SystemError,
  ModuleNotFoundError). Test:
  `3.2.10_custom_classes/exception_hierarchy.py`.
- **dict/set operators + methods** (212/212 @interp; all gates green): PEP 584
  dict union `|`/`|=` (in `binary`, reusing `dict_set`); dict methods
  `clear`/`popitem` and `update(other?, **kwargs)` (kwargs now threaded through
  `dict_method`); set named methods union/intersection/difference/
  symmetric_difference/issubset/issuperset/isdisjoint/update/pop/clear (accept
  any iterable). Tests: `3.2.7.1_dictionaries/dict_ops.py`,
  `3.2.6_set_types/set_methods.py`.
- **Range subscription/slicing + zero-step fix** (210/210 @interp; all gates
  green): `range(10)[3]`/`[-1]` yield the element (start+i*step), `[a:b:c]`
  yields a new range (new `slice_bounds` helper for CPython's slice.indices
  algorithm). Also fixed a latent infinite-loop: `slice_args` now rejects a zero
  step with `ValueError: slice step cannot be zero` (centrally, so list/str/
  tuple/range all covered). Test: `6.3.3_slicings/range_slicing.py`.
- **Type parameter lists (8.10, PEP 695) DONE** (209/209 @interp; all gates
  green): `def f[T]` and `type X[T] = …` now work. Added a `Typevar` heap obj
  (name + lazy bound/constraints functions) and a `TypeVar` boot class; the
  `typevar`/`typevar_with_bound`/`typevar_with_constraints` intrinsics build
  TypeVars; `set_function_type_params` stores `__type_params__` on a function
  (defaulting to `()`); `Typealias` now keeps its type_params. TypeVar exposes
  __name__/__bound__/__constraints__ (bound/constraints evaluated lazily on
  access). Test: `8.10_type_parameter_lists/type_params.py`. Generic *classes*
  (`class C[T]`) still need typing.Generic (`subscript_generic`) — deferred with
  the typing/import work.
- **Built-in-type subclassing DONE** (208/208 @interp; all gates green) — the
  last large data-model gap. `Instance` now carries a `native` payload (None_
  for plain objects). `builtin_base_tag`/`is_native_tag`/`native_of` helpers;
  `instantiate` special-cases native subclasses (build payload via the built-in
  constructor; mutable container + custom __init__ → empty payload then
  super().__init__ fills it via new `list/dict/set.__init__` builtins). Each
  protocol op's Instance branch gained a "no override → delegate to payload"
  fallback: subscript/store/del (dict __missing__ via KeyError interception),
  py_len/py_iter/contains/py_truth/py_repr/py_str/py_hash, instance_binop,
  comparisons (new `cmp_unwrap` respecting __eq__/__lt__/… overrides), int()/
  float() constructors, format_value, and object_getattribute (method access
  binds to the payload). isinstance against a built-in base now checks the MRO.
  Covers dict/list/set/tuple/int/str/float subclasses byte-identically. Tests:
  `3.2.11_class_instances/builtin_subclassing.py`,
  `3.3.7_emulating_container_types/dict_subclass_missing.py`.
- **Additional str methods** (206/206 @interp; all gates green): added
  `split`/whitespace-split with maxsplit, `removeprefix`/`removesuffix`,
  `rfind`/`rindex`, `istitle`/`isspace`/`isalnum`/`isnumeric`/`isdecimal`/
  `isidentifier`, `translate`, `casefold`, `expandtabs`, `encode` (helpers
  `split_on_sep_max`/`split_whitespace_max`/`rfind_substring`/`is_titlecased`/
  `expand_tabs`/`str_translate`). Test: `6.2.2_literals/str_methods_extra.py`.
  (These are library-reference methods; the long tail of remaining str/bytes/
  number methods is out of the strict §3/6/7/8/9 scope.)
- **int(str, base) parsing rewritten** (205/205 @interp; all gates green): the
  old code double-prepended the prefix (so `int("0xff", 16)` failed) and only
  handled bases 2/8/10/16. New `parse_int` is a manual digit scanner: bases
  2..36 or 0 (auto-detect), optional matching 0x/0o/0b prefix, sign, whitespace,
  single underscores between digits, with CPython's exact error messages
  (including "base must be >= 2 and <= 36, or 0"). Test:
  `3.2.4.1_integral/int_parsing.py`.
- **Exception introspection: add_note/__notes__/with_traceback** (204/204
  @interp; all gates green): added `BaseException.add_note` (appends to the
  `__notes__` list, CPython "note must be a str, not '…'" error) and
  `with_traceback` (sets `__traceback__`, returns self). Test:
  `8.4_try_statement/exception_notes.py`. (Suppression via `__exit__`→True, args,
  `from`-chaining `__cause__`, implicit `__context__`/`__suppress_context__`
  already worked.)
- **generator.throw + StopIteration.value** (203/203 @interp; all gates green):
  added `gen_throw` (resumes the suspended frame via a new `resume_with_error`
  that injects the exception at the yield point through the frame's exception
  table) and the `generator.throw` method; `StopIteration.value` now exposes the
  generator return value (instance_getattr fallback = args[0] or None); `next()`
  on a generator routes through `gen_send` so the return value reaches
  StopIteration (py_next discarded it). Test:
  `6.2.9_yield_expressions/gen_throw_value.py`.
- **Function `__annotations__`** (202/202 @interp; all gates green): the
  `Set_function_attribute(Annotations, …)` opcode was a no-op; now the flat
  `[k1;v1;…]` annotation tuple is converted to a dict stored as the function's
  `__annotations__` (in `fdict`), and `getattr` on a function defaults
  `__annotations__` to an empty dict. (Module/class-level annotations via
  `Setup_annotations` already worked.) Test:
  `8.7_function_definitions/annotations.py`.
- **printf-style `str % args`** (201/201 @interp; all gates green): added the
  `%` operator on strings (`binary Mod (Str …)` → `printf_format`). Parses
  `%[(key)][flags][width][.prec][len]conv`, translates each spec to the
  equivalent `format()` mini-language and reuses `format_value`; handles r/a/s/c
  and %d/%i/%u float-truncation specially, tuple vs single vs mapping operands,
  and the tuple-only "not all/enough arguments" errors. Test:
  `6.9_binary_bitwise_operations/printf_formatting.py`.
- **__mro_entries__ (3.3.3.1) + module-qualified class names** (200/200 @interp;
  all gates green): `__build_class__` now resolves non-class bases via
  `__mro_entries__(bases)` and records `__orig_bases__`
  (`3.3.3_customizing_class_creation/mro_entries.py`). Fixed the long-standing
  class-repr gap — `class_qualified_name` reads `__module__` (default "builtins")
  so user classes show `<class '__main__.C'>` (matching CPython) everywhere:
  repr/str of classes, GenericAlias args (`list[__main__.C]`), type_repr. Added
  `__module__`/`__qualname__` to `class_getattr` (builtins → "builtins").
- **Format-spec mini-language completed** (199/199 @interp; all gates green):
  `format_value` now parses the full `[[fill]align][sign][#][0][width][grouping]
  [.prec][type]` grammar — sign (+/-/space), `#` alternate form (0x/0o/0b),
  zero-pad interacting with the sign+prefix via `=` alignment, `,` and `_`
  grouping (every 3 for d/n/floats, every 4 for x/X/o/b), and the e/E/g/G float
  presentations (via `%.*e`/`%.*g`). Verified byte-identical incl. `#010x`,
  `+08`, `_X` hex grouping, `.3g`. Test: `6.2.2_literals/format_spec.py`.
- **Built-in function gaps closed** (199/199 @interp; all gates green): added
  `hex`/`bin`/`oct` (new `to_index` __index__ helper + `radix_repr`), `ascii`
  (`ascii_escape` re-escapes non-ASCII codepoints in repr as \xXX/\uXXXX/\U…),
  three-argument `pow(a,b,m)` (modular exp incl. modular inverse), and
  `round(x, ndigits)` for ints/floats (`round_half_even` banker's rounding;
  decimals via `Printf "%.*f"` round-trip to match CPython, incl. 2.675→2.67;
  `round_int_pow10` for ndigits<0). Also fixed `round(bool)`→int and `round(x,
  None)`. Tests: `3.3.8_emulating_numeric_types/divmod_and_index.py`,
  `6.2.2_literals/ascii_builtin.py`. Found while probing: `__missing__` and other
  builtin-type subclassing (`class D(dict)`) remain unsupported — see backlog.
- **except* / exception groups (8.4) DONE** (197/197 @interp;
  runtest/@stdlib/@gen green). Boot: `BaseExceptionGroup(BaseException)` and
  `ExceptionGroup(BaseExceptionGroup, Exception)` — the latter with an explicit
  MRO (new_class gained `?mro_tail` since it doesn't C3-merge). Construction via
  a shared `BaseExceptionGroup.__init__` builtin: validates message-is-str /
  exceptions-non-empty-sequence / each-item-is-exception, stores args +
  .message + .exceptions. `__str__` = "msg (N sub-exception[s])". Helpers
  `is_exception_instance`/`is_exception_group`/`exc_attr`/`eg_condition_matches`/
  `eg_derive`/`eg_split_pair`/`eg_match`. `Check_eg_match` opcode: full match →
  (group | wrapped-bare, None); group partial → split; non-match → (None, exc).
  `Prep_reraise_star` intrinsic: filter non-None leftovers — []→None, [e]→e,
  many→ExceptionGroup("", excs). ALSO fixed the long-standing general exception
  repr (`ValueError(1)` not `<ValueError object>`), on which group repr relies.
- **Generic aliases + union types + type aliases DONE** (196/196 @interp;
  runtest/@stdlib/@gen green). One cohesive slice spanning 3.3.5, 6.9 (PEP 604),
  7.14 (PEP 695). value.ml: three new heap objs `Generic_alias {ga_origin,
  ga_args}`, `Union_type members`, `Type_alias {ta_name, ta_value}` (type_name
  reports the qualified `types.GenericAlias`/`types.UnionType`/
  `typing.TypeAliasType` for error messages). Boot adds classes `GenericAlias`/
  `UnionType`/`TypeAliasType` (short __name__) — `class_of_value` special-cases
  the three objs to those (the qualified type_name only feeds error messages).
  `subscript` on a builtin container class (list/dict/tuple/set/frozenset/type,
  no user __class_getitem__) builds a `Generic_alias`; `binary Or` over
  type-operands builds a `Union_type` via `build_union` (flatten/de-dup/collapse,
  None→NoneType), with a metatype-named TypeError for non-type operands. New
  `type_repr` renders args (class→cname, builtin; None→"None", "..."; nested).
  `getattr_value` exposes __origin__/__args__ (+ delegate to origin), __args__,
  and __name__/__value__(lazy via call)/__type_params__. `isinstance_value`
  handles `Union_type`; `value_matches_builtin` gained NoneType/ellipsis/
  NotImplementedType. `Typealias` intrinsic builds the alias from the
  (name, type_params, value-fn) tuple. KNOWN GAPS: see backlog (user-class arg
  module prefix, parameterised aliases).
- **Metaclasses (3.3.3 / 3.3.4) DONE** (193/193 @interp; runtest/@stdlib/@gen
  green). Added `meta : int option` to the `cls` record (None = default `type`,
  sidesteps boot bootstrapping). `make_class` now: extracts `metaclass=` from
  kwds, calls `determine_metaclass` (most-derived among explicit hint + bases'
  metaclasses, raising the CPython conflict message), and either fast-paths
  through `type_new` (default `type`) or *calls* the user metaclass via
  `instantiate` (so `Meta.__new__`/`__init__` run). `type_new` is the shared
  class builder (allocates the Class with its `meta`, fills `__classcell__`,
  runs `__set_name__`+`__init_subclass__`); exposed as the `type.__new__`
  builtin (also `type.__init__` no-op, `type.__call__`=instantiate) on the
  `type` boot class so `super().__new__/__init__/__call__` resolve. KEY FIX:
  `super_getattr` now searches the *metaclass* MRO when `self` is a class whose
  metaclass derives from `cls` (metaclass-instance method), vs the class's own
  MRO in a classmethod. `class_of_value`/`isinstance_value` honour `meta` so
  `type(C) is Meta` / `isinstance(C, Meta)`. `metaclass_hook` routes
  isinstance/issubclass to metaclass `__instancecheck__`/`__subclasscheck__`
  (3.3.4); also fixed `issubclass_value` to accept a tuple 2nd arg. `__prepare__`
  (3.3.3.3) determined in `__build_class__` and used as the body namespace.
  `call` dispatch honours a user metaclass `__call__`. OPEN: `__mro_entries__`
  (3.3.3.1), non-dict `__prepare__` mappings.
- Setup: cloned 3.13 reference rst to `/tmp/cpython`; runner walks subfolders;
  tests have no numeric prefixes. Starting section 3.
- 3.1, 3.2.1–3.2.3 done (120/120 @interp green). Interpreter changes:
  - Added `Ellipsis` value + `...` literal; registered singleton types
    `NoneType`/`ellipsis`/`NotImplementedType` so `type(None)` etc. work.
  - Rich comparisons (`__eq__`/`__lt__`/…) now honour a `NotImplemented`
    return → reflected method → identity/TypeError fallback (`try_richcmp`).
  - `py_eq` fallback now identity-based so singletons compare equal to self.
- 3.2.4–3.2.4.2 done (123/123 @interp green). Interpreter change:
  - Numeric `==`/`<` now use IEEE operators (`num_eq`/`num_lt`) instead of
    `compare`, so `nan != nan` and `nan < x` behave per IEEE 754.
  - Still TODO: `<=`/`>=` with NaN (revisit in 6.10.1); exact int-vs-huge-float
    ordering.
- 3.2.4.3 complex done (124/124 @interp green). Added a `Complex` value type:
  `...j` literals, `+ - * / **`(int exp, CPython c_powu), mixed int/float
  promotion, repr (`(1+2j)`/`3j`/`(-0-3j)`), `complex()`, `abs`, `.real`/
  `.imag`/`.conjugate()`, `==`, `isinstance`, and TypeError on ordering.
  Also gave `abs()` an `__abs__` path for instances.
- 3.2.5–3.2.5.2 done (127/127 @interp green). Interpreter changes:
  - Extended-slice assignment `xs[i:j:k] = ...` (length-checked, ValueError on
    mismatch); item-deletion TypeError now names the type and adds instance
    `__delitem__`.

- 3.2.6–3.2.7.1 done (130/130 @interp green). Interpreter change: dict keys and
  set elements are now hashability-checked (`unhashable type: 'list'/'dict'/'set'`).
- 3.2.8 done (131/131 @interp green). Interpreter change: bound methods expose
  `__self__`/`__func__` and delegate other attribute reads to the underlying
  function object.
- 3.2.10–3.2.11 done (133/133 @interp green). Interpreter change: class
  `__doc__` is now class-local (own dict, default None) instead of inherited
  via the MRO. Next: 3.2.13 slice objects, then 3.3 special method names.
- 3.2.13 done (134/134 @interp green). Added `slice` type + `slice()`
  constructor and `.start`/`.stop`/`.step`. **Section 3.2 complete** (modules
  deferred; I/O + non-slice internal types N/A). Next: 3.3 Special method names
  (3.3.1 basic customization onward).
- 3.3.1 done (138/138 @interp green). Interpreter changes: real __new__ path
  (object.__new__, instantiate honours __new__ result & checks __init__ returns
  None); format_value/Format_simple/str.format now call instance __format__
  (object default: empty→str, non-empty→TypeError); repr/str/__bool__ error
  messages now include the offending type; custom __ne__ is honoured.
- 3.3.2 (direct methods) done (139/139 @interp green). Interpreter changes:
  registered object.__getattribute__/__setattr__/__delattr__; instance attribute
  get/set/del now dispatch to user __getattribute__/__setattr__/__delattr__
  (with __getattr__ fallback on AttributeError); added `dir()` builtin honouring
  __dir__ (sort, no dedup) with a default over instance+MRO dicts.
- 3.3.2.3/4 descriptors done (140/140 @interp green). Generalised the attribute
  machinery: `descr_kind`/`descr_get` implement the data/non-data descriptor
  protocol in object_getattribute/setattr/delattr and class binding (Property is
  one data descriptor among many). Next: 3.3.2.5 __slots__ (deferred), 3.3.3+.
- Documentation pass: added `(* ref: ... *)` section citations to all
  conformance-driven code in value.ml/interp.ml (augmented goal).
- 3.3.5 (__class_getitem__, 142/142) and 3.3.7 (container protocol + reversed(),
  143/143) done; 3.3.6 (__call__) covered by existing test; 3.3.4 deferred
  (metaclass-only). Interpreter: Class[key] subscription dispatches to
  __class_getitem__; added `reversed()` builtin (uses __reversed__, else
  __len__+__getitem__ fallback, plus builtin sequences).
  Next: 3.3.8 numeric emulation (binary/reflected/in-place arithmetic & unary
  already work via instance_binop/unary; add __int__/__float__/__index__/__round__
  conversions and a comprehensive test), 3.3.9 with-statement, 3.3.10
  __match_args__, 3.3.12 special-method lookup.
- 3.3.8 done (146/146 @interp green). Binary/reflected/in-place arithmetic and
  __neg__/__abs__/__invert__ already worked via instance_binop/unary; added
  int()/float() dispatch to __int__/__float__ (fallback __index__), round() to
  __round__, and __index__ resolution for str/tuple/list subscripting.
  Next: 3.3.9 with (__enter__/__exit__), 3.3.10 __match_args__, 3.3.12 lookup.
- 3.3.9/3.3.10/3.3.12 done (149/149 @interp green; runtest + @stdlib green).
  3.3.9 and 3.3.12 needed no interpreter change (with-stmt already passes exc
  args/suppression; find_dunder already bypasses the instance dict); 3.3.10 got
  the CPython "N positional sub-pattern(s) (M given)" message. **Section 3 done.**
  Next section: **6 Expressions** — go subsection by subsection (6.1 arithmetic
  conversions, 6.2 atoms, 6.3 primaries, 6.5–6.11 operators, 6.12–6.17), reading
  each paragraph, extending the existing partial coverage, with ref-comments.
- Section 6 started (156/156 @interp green): 6.1 conversions, 6.2.1 names+private
  mangling, 6.2.3 parenthesized, 6.2.4 comprehension scoping, 6.2.6 set/6.2.7 dict
  displays (** unpack, dup keys), 6.3.4 calls. Interpreter: bind_args error
  messages now match CPython ("takes from M to N positional arguments but K
  were given", "missing N required positional argument(s): 'a' and 'b'").
  Next: 6.3.2 subscriptions, 6.5–6.11 operators, 6.12–6.17.
- **Section 6 done** (167/167 @interp; runtest + @stdlib green). Interpreter
  fixes this pass: subscript type-error messages (list/tuple/str indices),
  0**neg→ZeroDivisionError, negative shift→ValueError, NaN <=/>= (num_le/num_ge),
  membership identity short-circuit (`x is e or x==e`), list.clear(). Next
  section: **7 Simple statements** (7.1 expr stmts, 7.2 assignment/aug/annotated,
  7.3 assert, 7.4 pass, 7.5 del, 7.6 return, 7.7 yield, 7.8 raise, 7.9 break,
  7.10 continue, 7.12 global, 7.13 nonlocal, 7.14 type; 7.11 import deferred).
- **Section 7 done** (176/176 @interp; runtest + @stdlib green). Interpreter
  fixes: unpack errors ("too many values to unpack (expected N)" vs
  "not enough… (expected N, got M)"); raise chaining (set_exc_chain:
  __context__/__cause__/__suppress_context__, `from None`, bare-raise message);
  list.clear(). __debug__/assert/__annotations__ already worked. Next section:
  **8 Compound statements** (8.1 if, 8.2 while, 8.3 for, 8.4 try, 8.5 with,
  8.6 match, 8.7 functions, 8.8 classes; 8.9 async deferred).
- **Section 8 done** (183/183 @interp; runtest + @stdlib green). while/for/try/
  with/match/functions/classes all conform. Interpreter fix: bind_args now emits
  "got some positional-only arguments passed as keyword arguments: 'a, b'".
  except* (exception groups) backlogged. Next section: **9 Top-level components**
  (9.1 complete programs — mostly covered; 9.2 file/9.3 interactive ⛔).
- **Section 9 done** (184/184; all gates green incl. @gen). __name__=="__main__"
  works. **ALL FIVE SECTIONS (3,6,7,8,9) now substantially complete.** Remaining
  work = the Backlog below (advanced/deferred features). Priority order for
  closing the backlog: __hash__ (completes 3.3.1) → frozenset (3.2.6) → bytes/
  bytearray (3.2.5) → subclass-reflected-priority (3.3.1/6.10) → __slots__ →
  metaclasses → except* → async/await/eval/import/type-aliases (largest/deferred).
- 3.3.3 hooks done (141/141 @interp green). make_class now runs __set_name__
  (definition order) then __init_subclass__ (MRO-searched, classmethod-bound,
  forwards class-def kwargs minus metaclass); object.__init_subclass__ errors on
  kwargs. Next: 3.3.4 instance/subclass checks, 3.3.5 generics, 3.3.6+.

## Documentation requirement (augmented goal)
Each interpreter behaviour implemented for conformance carries a comment citing
the Language Reference section(s) it implements, e.g.
`(* ref: Language Reference 3.3.2.4 Invoking Descriptors *)`. Apply to all
conformance-driven code (existing pass logged below, new code as written).

## Backlog — IN SCOPE (remaining actionable work)
- ~~`bytearray` (3.2.5.2)~~ DONE (`3.2.5.2_mutable_sequences/bytearray_objects.py`):
  mutable `Bytearray` heap obj, `bytearray()` ctor (shared `build_bytes`), int
  items + slicing, item/slice assignment & del, append/extend, +/* (result type
  = left operand), cross-type ==/< with bytes, iteration, membership, decode,
  unhashable. 3.2.5 sequences now fully covered (str/tuple/bytes/list/bytearray).
- ~~Custom metaclasses (3.3.3 / 3.3.4)~~ DONE (`3.3.3_customizing_class_creation/
  metaclasses.py`, `3.3.4_instance_subclass_checks/instance_subclass_checks.py`):
  `meta` field on `cls`; `determine_metaclass` (most-derived + conflict error);
  `type_new` core builder + `type.__new__`/`__init__`/`__call__` builtins so
  `super().__new__`/`__call__` resolve; `super_getattr` searches the metaclass
  MRO for metaclass-instance methods; `metaclass_hook` routes isinstance/
  issubclass to `__instancecheck__`/`__subclasscheck__`; `__prepare__` produces
  the body namespace. STILL OPEN: `__mro_entries__` (3.3.3.1) and `__prepare__`
  returning a non-dict mapping (STORE_NAME writes a dict directly).
- ~~`__slots__` (3.3.2.5)~~ DONE (`3.3.2.5_slots/slots.py`): enforced in
  object_setattr / object_getattribute via `instance_slots`.
- ~~`type` statement / type aliases (7.14)~~ DONE
  (`7.14_type_statement/type_aliases.py`), together with built-in generic
  aliases (`list[int]`; `3.3.5_emulating_generic_types/generic_aliases.py`) and
  PEP 604 union types (`int | str`; `6.9_binary_bitwise_operations/union_types.py`).
- ~~`except*` exception groups (8.4)~~ DONE (`8.4_try_statement/except_star.py`):
  BaseExceptionGroup/ExceptionGroup boot classes, construction + validation,
  .message/.exceptions/.args, str/repr (general exception repr fixed too:
  `Type(args)` instead of `<Type object>`), split/subgroup/derive, the
  Check_eg_match and prep_reraise_star opcodes. KNOWN GAPS: BaseExceptionGroup
  auto-promotion to ExceptionGroup (all-Exception members), __context__/
  __traceback__ propagation on reraised groups, predicate `split` only loosely
  tested. Non-Exception BaseExceptions (KeyboardInterrupt/SystemExit/
  GeneratorExit) are still absent from the exception tree.
- `dir()` with no args (lists current scope) — needs frame access in builtins.
  Module-level `dir()` is not byte-identical anyway (CPython lists __builtins__,
  __spec__, …); only function-local `dir()` is cleanly testable. Low priority.
- ~~Subclassing built-in types (`class D(dict)`, list/int/str/…)~~ DONE
  (`3.2.11_class_instances/builtin_subclassing.py`,
  `3.3.7_emulating_container_types/dict_subclass_missing.py`): `Instance` gained
  a `native` payload field; `instantiate` builds it from the constructor args
  (mutable containers with a custom __init__ start empty and are filled via
  super().__init__ → new `list/dict/set.__init__` builtins); every protocol
  operation (subscript/store/del, len, iter, contains, repr/str, truth, hash,
  binary ops, comparisons, int()/float(), format, attribute/method access,
  isinstance) falls back to the payload when the subclass doesn't override the
  relevant dunder. Covers dict (incl. __missing__), list, set, tuple, int, str,
  float. REMAINING: subclassing bytes/frozenset less exercised; multiple
  inheritance mixing two built-in bases is unsupported.
- GenericAlias/UnionType edges left open: `X[T]`-parameterised aliases /
  `TypeVar`s, and calling a GenericAlias to instantiate, are not modelled.
  (Class names in displays are now correctly module-qualified via __module__.)

## Backlog — OUT OF SCOPE (user-directed; do NOT implement here)
- async / await / coroutines (3.4, 6.4, 8.9).
- import system (7.11) — to be done with the user later.
- `eval()` / `exec()` (9.4).
- ~~`__hash__` protocol~~ DONE (3.3.1, `3.3.1_basic_customization/hashing.py`):
  `hash()` builtin (int modular hash, hash(-1)==-2, integral floats), custom
  __hash__ (reduced), override-__eq__-without-__hash__ ⇒ unhashable, __hash__=None
  ⇒ unhashable; check_hashable enforces it for dict keys / set elements.
- ~~Subclass-reflected-priority rule~~ DONE (3.3.1/6.10.1/3.3.8,
  `3.3.1_basic_customization/reflected_priority.py`): `reflected_priority` helper
  applied in instance_binop / instance_order / instance_eq / `!=`.
- ~~Non-bool comparison results (6.10.1)~~ DONE
  (`6.10_comparisons/nonbool_results.py`): added `py_compare_value` +
  instance_eq_value/instance_ne_value/instance_order_value; the Compare
  instruction now returns the raw rich-comparison result, coercing via bool()
  only when the bytecode's coerce_bool flag is set.
- ~~`bytes`~~ DONE (3.2.5.1, `3.2.5.1_immutable_sequences/bytes_objects.py`):
  `Bytes` value, `b'…'` literals, `bytes()` ctor (int/iterable/str+encoding),
  int items + slicing→bytes, repr, +/*, lexicographic compare, hashable,
  iteration over ints, membership, `.decode()`. `bytearray` (mutable, 3.2.5.2)
  still TODO.
- ~~`frozenset` distinct type~~ DONE (3.2.6, `3.2.6_set_types/frozenset.py`):
  `Frozenset` obj, `frozenset()`, repr, hashable, set algebra/subset across
  set/frozenset, ==, iteration. (frozenset *methods* like .union()/.copy() not
  yet exposed — only operators; add if needed.)
- ~~NaN with `<=`/`>=`~~ DONE (6.10.1: num_le/num_ge use IEEE operators).
- `nan in [nan]` and `obj in [obj]` short-circuit `x is e`: instance identity now
  works; float/NaN lack object identity in the value model, so `nan in [nan]`
  stays False (interp limitation, not tested).
