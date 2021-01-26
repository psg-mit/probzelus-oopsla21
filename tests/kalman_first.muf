val step =
  fun ((first, x_0, x), (prob, obs)) ->
    let (x_0, x) =
      if first then (let x_0 = sample (prob, gaussian(0., 1.)) in (x_0, x_0))
      else (x_0, x) in
    let x = sample (prob, gaussian (x, 1.)) in
    let () = observe (prob, (x, obs)) in
    (x_0, (false, x_0, x))

val main_init = infer_init (true, 0., 0.)
val main_step =
  fun (state : (bool * float * float), args : (_ * int)) ->
    infer (
      fun (state : (bool * float * float), args : (_ * int)) ->
        step (state, args),
      (state, args)
    )
