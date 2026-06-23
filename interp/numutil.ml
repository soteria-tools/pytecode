(* Pure numeric helpers (rounding, exact float ratio).
   [open]ed by [Interp] and [Py_num]. *)

let round_half_even x =
  let fl = Float.floor x in
  let frac = x -. fl in
  if frac < 0.5 then fl
  else if frac > 0.5 then fl +. 1.
  else if Float.rem fl 2. = 0. then fl
  else fl +. 1.

let round_int_pow10 z k =
  let s = Z.pow (Z.of_int 10) k in
  let q = Z.div z s and r = Z.rem z s in
  let half2 = Z.mul (Z.of_int 2) (Z.abs r) in
  let bump =
    match Z.compare half2 s with
    | c when c > 0 -> true
    | c when c < 0 -> false
    | _ -> not (Z.equal (Z.rem q (Z.of_int 2)) Z.zero)
  in
  let q =
    if not bump then q
    else if Z.sign z >= 0 then Z.add q Z.one
    else Z.sub q Z.one
  in
  Z.mul q s

let float_as_integer_ratio f =
  if not (Float.is_finite f) then (Z.zero, Z.one) (* unreachable in tests *)
  else
    let m, e = Float.frexp f in
    (* m * 2^e, m in [0.5,1); scale m to an integer mantissa *)
    let rec scale m e =
      if Float.is_integer m || e <= -1075 then (m, e)
      else scale (m *. 2.) (e - 1)
    in
    let m, e = scale m e in
    let num = Z.of_float m and den = Z.one in
    if e >= 0 then (Z.mul num (Z.shift_left Z.one e), den)
    else (num, Z.shift_left den (-e))
