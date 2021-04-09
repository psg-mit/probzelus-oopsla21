val kalman = stream {
  init = (true, 0.);
  step ((first, x), obs) =
    let x = if first then sample (gaussian (0., 1.)) else sample (gaussian (x, 1.)) in
    (x, (false, x))
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, obs) = unfold (kalman, obs)
}
