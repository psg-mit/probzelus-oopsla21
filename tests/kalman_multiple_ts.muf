
val step =
  fun ((first, x), (prob, obs)) ->
    let x' = if first then 0. else sample (prob, gaussian (x, 1.)) in
    let y = gaussian (x, 1.) in
    let () = observe (prob, (y, obs)) in
    (x', (false, x'))

val main_init = infer_init (true, 0.)
val main_step =
  fun ((first, x), obs) ->
    infer (
      fun ((first, x), (prob, obs)) ->
        step ((first, x), (prob, obs)),
      ((first, x), obs)
    )
