val coin = stream {
  init = (true, const (0.));
  step ((first, xt), yobs) =
    let xt = if first then sample (beta (1., 1.)) else xt in
    let () = observe (bernoulli (xt), yobs) in
    (xt, (false, xt))
}

val main = stream {
  init = infer (1, coin);
  step (coin, ()) = 
    let (d, s) = unfold (coin, true) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
