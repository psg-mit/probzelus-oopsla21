val f_init = sample (beta (1., 1.))
val f_step =
  fun (xt, yobs) ->
    let () = observe (bernoulli (xt), yobs) in
    (xt, xt)