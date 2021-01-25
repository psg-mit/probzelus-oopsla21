val step =
  fun ((first, x), (prob, obs)) ->
    let x = if first then sample (prob, gaussian (0., 1.)) else sample (prob, gaussian (x, 1.)) in
    (x, (false, x))

val main_init = infer_init (true, 0.)
val main_step =
  fun ((first, x), (prob, obs)) ->
    infer (
      fun ((first, x), (prob, obs)) ->
        step ((first, x), (prob, obs)),
      ((first, x), (prob, obs))
    )
