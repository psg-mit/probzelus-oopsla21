val f_init = (true, 0., 0.)
val f_step =
  fun ((first, mu, sigma), obs) ->
    let mu = if first then sample (gaussian (0., 10.)) else mu in
    let sigma = if first then sample (gaussian (0., 1.)) else sigma in
    let sigma2 = mult (sigma, sigma) in
    let () = observe (gaussian (mu, eval (sigma2)), obs) in
    ((mu, sigma), (false, mu, sigma))
