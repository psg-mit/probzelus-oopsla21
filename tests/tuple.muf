val tuple = stream {
  init = (0., 0.);
  step ((x, y), obs) =
    let x = sample (gaussian(0., 1.)) in
    let y = sample (gaussian(0., 1.)) in
    let (a, b) = if (sample (bernoulli(0.5))) then (gaussian (x, 1.), gaussian (y, 1.)) else (gaussian (y, 1.), gaussian (x, 1.)) in
    let () = observe (a, 1.) in
    let () = observe (b, 2.) in
    (x, (x, y))
}

val main = stream {
  init = infer (1, tuple);
  step (tuple, obs) = unfold (tuple, obs)
}
