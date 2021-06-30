val coin = stream {
  init = (true, 0.);
  step ((first, x), obs) =
    let p = if first then sample (beta (1., 1.)) else p in
    let outlier = sample (bernoulli (0.5)) in
    let x = if outlier then bernoulli (0.5) else bernoulli (p) in
    let () = observe (x, obs) in
    (x, (false, x))
}

val main = stream {
  init = infer (1, coin);
  step (coin, ()) = 
    let (d, s) = unfold (coin, true) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
