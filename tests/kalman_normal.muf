val step =
  fun ((first, x), obs) ->
    let x = if first then sample (gaussian (0., 1.)) else sample (gaussian (x, 1.)) in
    let y = gaussian (x, 1.) in
    let () = observe (y, obs) in
    (x, (false, x))

val main_init = infer_init (true, 0.)
val main_step =
  fun ((first, x), obs) ->
    infer (
      fun ((first, x), obs) ->
        step ((first, x), obs),
      ((false, x), obs)
    )
