val coin = stream {
  init = (true, const (0.5));
  step ((first, p), obs) =
    let p = if first then sample (beta (1., 1.)) else p in
    let outlier = sample (bernoulli (const (0.5))) in
    let x = if eval (outlier) then bernoulli (const (0.5)) else bernoulli (p) in
    let () = observe (x, obs) in
    (p, (false, p))
}

val main = stream {
  init = infer (1, coin);
  step (coin, ()) = 
    let (d, s) = unfold (coin, true) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
