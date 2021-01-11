val main_init =
  infer_init (true, 0., 0.)
val main_step =
  fun ((first, xt, outlier_prob), observed) ->
    infer (
      fun ((first, xt, outlier_prob), yobs) ->
        let (xt, outlier_prob) =
          if first then
            (sample (gaussian (0., 100.)), sample (beta (100., 1000.)))
          else (sample (gaussian (xt, 1.)), outlier_prob) in
        let is_outlier = sample (bernoulli (outlier_prob)) in
        let () =
          if is_outlier then
            (observe (gaussian (0., 100.), yobs))
          else (observe (gaussian (xt, 1.), yobs)) in
        (xt, (false, xt, outlier_prob)),
      ((first, xt, outlier_prob), observed)
    )
