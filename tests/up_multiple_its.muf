val f = stream {
  init = (0., 0., 0., 0.);
  step ((x_p, x_pp, x_ppp, x_pppp), obs) =
    let x = sample (gaussian (x_p, 1.)) in
    let _ = observe (x, 1.0) in
    (x_pppp, (x, x_p, x_pp, x_ppp))
}

val main = stream {
  init = infer (1, f);
  step (f, obs) = unfold (f, obs)
}
