val init = fun (prob, i) -> sample (prob, bernoulli (0.5))

val step =
  fun ((first, x, map), (prob, (obs, cmd))) ->
    let map = if first then Array.init (prob, 100, init) else map in
    let wheel_slip = sample (prob, bernoulli (0.5)) in
    let x = if first then 0. else (if wheel_slip then x else plus (x, cmd)) in
    let o = Array.get (map, x) in
    let () = observe (prob, (bernoulli (ite (o, 0.9, 0.1)), obs)) in
    ((x, map), (false, x, map))

val main_init = infer_init (true, 0., 0.)
val main_step =
  fun (state : (bool * float * float array), args : (_ * (int * float))) ->
    infer (
      fun (state : (bool * float * float array), args : (_ * (int * float))) ->
        step (state, args),
      (state, args)
    )
