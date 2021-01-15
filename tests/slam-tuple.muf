val step =
  fun ((first, x, (map1, map2)), (obs, cmd)) ->
    let (map1, map2) = if first then (sample (bernoulli (0.5)), sample (bernoulli (0.5))) else (map1, map2) in
    let wheel_slip = sample (bernoulli (0.5)) in
    let x = if first then 0. else (if wheel_slip then x else plus (x, cmd)) in
    let o = if x then map1 else map2 in
    let () = observe (bernoulli (ite (o, 0.9, 0.1)), obs) in
    ((x, (map1, map2)), (false, x, (map1, map2)))

val main_init = infer_init (true, 0., 0.)
val main_step =
  fun ((first, x, (map1, map2)), (obs, cmd)) ->
    infer (
      fun ((first, x, (map1, map2)), (obs, cmd)) ->
        step ((first, x, (map1, map2)), (obs, cmd)),
      ((first, x, (map1, map2)), (obs, cmd))
    )