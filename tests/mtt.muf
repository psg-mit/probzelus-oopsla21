val f_init = (true, 0., 0.)
val f_step =
  fun ((first, x, map), (obs, cmd)) ->
    let map = if first then sample (bernoulli (0.5)) else map in
    let wheel_slip = sample (bernoulli (0.5)) in
    let x = if first then 0. else (if wheel_slip then x else plus (x, cmd)) in
    let o = get (map, x) in
    let () = observe (bernoulli (ite (o, 0.9, 0.1)), obs) in
    ((x, map), (false, x, map))
