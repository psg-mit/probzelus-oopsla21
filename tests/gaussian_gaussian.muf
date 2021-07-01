val g_g = stream {
  init = (true, const (0.), const (0.));
  step ((first, mu, sigma), obs) =
    let mu = if first then sample (gaussian (const (0.), 10.)) else mu in
    let sigma = if first then sample (gaussian (const (0.), 1.)) else sigma in
    let sigma2 = mult (sigma, sigma) in
    let () = observe (gaussian (mu, eval (sigma2)), obs) in
    (pair (mu, sigma), (false, mu, sigma))
}

val main = stream {
  init = infer (1, g_g);
  step (g_g, ()) = 
    let (d, s) = unfold (g_g, 1.) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
