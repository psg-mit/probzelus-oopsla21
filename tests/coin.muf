val main_init = infer_init (true, 0.)
val main_step =
  fun ((first, xt), observed) ->
    infer (
      fun ((first, xt), yobs) ->
        let xt = if first then sample (beta (1., 1.)) else xt in
        let () = observe (bernoulli (xt), yobs) in
        (xt, (false, xt)),
      ((first, xt), observed)
    )
