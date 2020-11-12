val f_init = (true, 0.)
val f_step =
  fun ((first, x), ()) ->
    let x = if first then sample (gaussian (0., 1.)) else sample (gaussian (x, 1.)) in
    (x, (false, x))