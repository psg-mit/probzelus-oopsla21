val outlier = stream {
  init = (true, 0., 0.);
  step ((first, xt, outlier_prob), yobs) =
    let (xt, outlier_prob) =
      if first then
        (sample (gaussian (0., 100.)), sample (beta (100., 1000.)))
      else (sample (gaussian (xt, 1.)), outlier_prob) in
    let is_outlier = sample (bernoulli (outlier_prob)) in
    let () =
      if is_outlier then
        (observe (gaussian (0., 100.), yobs))
      else (observe (gaussian (xt, 1.), yobs)) in
    (xt, (false, xt, outlier_prob))
}

val main = stream {
  init = infer (1, outlier);
  step (outlier, obs) = unfold (outlier, obs)
}
