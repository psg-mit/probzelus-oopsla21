val f_init = sample (beta (1., 1.))
val f_step =
  fun (p, obs) ->
    let x = bernoulli(p) in
    let () = observe (x, obs) in
    (p, p)