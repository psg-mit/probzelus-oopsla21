val kalman = stream {
  init = (true, 0., 0.);
  step ((first, i, pre_x), obs) =
    let (i, pre_x) =
      if first then (let i = sample (gaussian(0., 1.)) in (i, i))
      else (i, pre_x) in
    let x = sample (gaussian (pre_x, 1.)) in
    let () = observe (gaussian (x, 1.), obs) in
    (x, (false, i, x))
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, obs) = unfold (kalman, obs)
}
