val f_init = sample (gaussian (0., 1.))
val f_step =
  fun (x, obs) ->
    let x = sample (gaussian (x, 1.)) in
    let y = gaussian (x, 1.) in
    let () = observe (y, obs) in
    (x, x)