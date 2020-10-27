val f_init = sample (gaussian (0., 1.))
val f_step =
  fun x ->
    let x = sample (gaussian (x, 1.)) in
    (x, x)