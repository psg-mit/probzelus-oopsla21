val step =
  fun ((first, xt, outlier_prob), (prob, yobs)) ->
    let (xt, outlier_prob) =
      if first then
        (sample (prob, gaussian (0., 100.)), sample (prob, beta (100., 1000.)))
      else (sample (prob, gaussian (xt, 1.)), outlier_prob) in
    let is_outlier = sample (prob, bernoulli (outlier_prob)) in
    let () =
      if is_outlier then
        (observe (prob, (gaussian (0., 100.), yobs)))
      else (observe (prob, (gaussian (xt, 1.), yobs))) in
    (xt, (false, xt, outlier_prob))

val main_init = infer_init (true, 0., 0.)
val main_step =
  fun (state : (bool * float * float), args : (_ * float)) ->
    infer (
      fun (state : (bool * float * float), args : (_ * float)) ->
        step (state, args),
      (state, args)
    )
