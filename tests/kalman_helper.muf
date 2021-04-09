val kalman_helper = stream {
  init = 0.0;
  step (_, x) =
    let x' = sample (gaussian (x, 1.)) in
    (x', x')
}

val kalman = stream {
  init = init (kalman_helper);
  step (kh, obs) =
    let (x, kh') = unfold (kh, ()) in
    let _ = observe (x, obs) in
    (x, kh')
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, obs) = unfold (kalman, obs)
}
