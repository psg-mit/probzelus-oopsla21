val f_init = (0., 0.)
val f_step =
  fun ((x, y), (prob, obs)) ->
    let x = sample (prob, gaussian(0., 1.)) in
    let y = sample (prob, gaussian(0., 1.)) in
    let (a, b) = if (sample (prob, bernoulli(0.5))) then (gaussian (x, 1.), gaussian (y, 1.)) else (gaussian (y, 1.), gaussian (x, 1.)) in
    let () = observe (prob, (a, 1.)) in
    let () = observe (prob, (b, 2.)) in
    (x, (x, y))

val main_init = infer_init (true, 0., 0.)
val main_step =
  fun ((x, y), obs) ->
    infer (
      fun ((x, y), (prob, obs)) ->
        f_step ((x, y), (prob, obs)),
      ((x, y), obs)
    )
