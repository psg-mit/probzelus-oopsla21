val g_g = stream {
  init = (true, 0., 0.);
  step ((first, mu, sigma), obs) =
    let mu = if first then sample (gaussian (0., 10.)) else mu in
    let sigma = if first then sample (gaussian (0., 1.)) else sigma in
    let sigma2 = mult (sigma, sigma) in
    let () = observe (gaussian (mu, eval (sigma2)), obs) in
    ((mu, sigma), (false, mu, sigma))
}

val main = stream {
  init = infer (1, g_g);
  step (g_g, obs) = unfold (g_g, obs)
}
