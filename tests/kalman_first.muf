val kalman = stream {
  init = (true, 0., 0.);
  step ((first, x_0, x), obs) =
    let (x_0, x) =
      if first then (let x_0 = sample (gaussian(0., 1.)) in (x_0, x_0))
      else (x_0, x) in
    let x = sample (gaussian (x, 1.)) in
    let () = observe (x, obs) in
    (x_0, (false, x_0, x))
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, obs) = unfold (kalman, obs)
}
