val step =
  fun ((first, x), (prob, obs)) ->
    let p = if first then sample (prob, beta (1., 1.)) else p in
    let outlier = sample (prob, bernoulli (0.5)) in
    let x = if outlier then bernoulli (0.5) else bernoulli (p) in
    let () = observe (prob, (x, obs)) in
    (x, (false, x))

val main_init = infer_init (true, 0.)
val main_step =
  fun (state : (bool * float), args : (_ * float)) ->
    infer (
      fun (state : (bool * float), args : (_ * float)) ->
        step (state, args),
      (state, args)
    )
