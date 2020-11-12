val f_init = (true, 0.)
val f_step =
  fun ((first, xt), yobs) ->
    let xt = if first then sample (beta (1., 1.)) else xt in
    let () = observe (bernoulli (xt), yobs) in
    (xt, (false, xt))