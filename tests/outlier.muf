val f_init =
  let xt = sample (gaussian (0., 100.)) in
  let outlier_prob = sample (beta (100., 1000.)) in
  (xt, outlier_prob)
val f_step =
  fun ((xt, outlier_prob), yobs) ->
    let xt = sample (gaussian (xt, 1.)) in
    let is_outlier = sample (bernoulli (outlier_prob)) in
    let () = if (is_outlier) then (observe (gaussian (0., 100.), yobs)) else (observe (gaussian (xt, 1.), yobs)) in
    (xt, (xt, outlier_prob))