val f_init =
  fun (prob, ()) ->
    sample (prob, beta (1., 1.))
val f_step =
  fun (p, (prob, obs)) ->
    let x = bernoulli(p) in
    let () = observe (prob, (x, obs)) in
    (p, p)