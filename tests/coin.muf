val step =
  fun ((first, xt), (prob, yobs)) ->
    let xt = if first then sample (prob, beta (1., 1.)) else xt in
    let () = observe (prob, (bernoulli (xt), yobs)) in
    (xt, (false, xt))

val main_init = infer_init (true, 0.)
val main_step =
  fun (state : (bool * float), args : (_ * float)) ->
    infer (
      fun (state : (bool * float), args : (_ * float)) ->
        step (state, args),
      (state, args)
    )
