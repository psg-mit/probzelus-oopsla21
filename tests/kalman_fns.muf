val f = fun (prob, x) -> sample (prob, gaussian (x, 1.))
val g = fun x -> gaussian (x, 1.)
val h = fun (prob, y) -> observe (prob, (y, obs))

val step =
  fun ((first, x), (prob, obs)) ->
    let x = if first then sample (prob, gaussian (0., 1.)) else f (prob, x) in
    let y = g (x) in
    let () = h (prob, y) in
    (x, (false, x))

val main_init = infer_init (true, 0.)
val main_step =
  fun (state : (bool * float), args : (_ * float)) ->
    infer (
      fun (state : (bool * float), args : (_ * float)) ->
        step (state, args),
      (state, args)
    )
