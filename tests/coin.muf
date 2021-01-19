val step =
  fun ((first, xt), (prob, yobs)) ->
    let xt = if first then sample (prob, beta (1., 1.)) else xt in
    let () = observe (prob, (bernoulli (xt), yobs)) in
    (xt, (false, xt))

val main_init = infer_init (true, 0.)
val main_step =
  fun ((first, xt), observed) ->
    infer (
      fun ((first, xt), (prob, yobs)) ->
        step ((first, xt), (prob, yobs)),
      ((first, xt), observed)
    )
