val coin = stream {
  init = (true, 0.);
  step ((first, xt), yobs) =
    let xt = if first then sample (beta (1., 1.)) else xt in
    let () = observe (bernoulli (xt), yobs) in
    (xt, (false, xt))
}

val main = stream {
  init = infer (1, coin);
  step (coin, obs) = unfold (coin, obs)
}
