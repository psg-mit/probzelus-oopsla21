val tuple = stream {
  init = (const (0.), const (0.));
  step ((x, y), ()) =
    let x = sample (gaussian(const (0.), 1.)) in
    let y = sample (gaussian(const (0.), 1.)) in
    let (a, b) = if eval (sample (bernoulli(const (0.5)))) then (gaussian (x, 1.), gaussian (y, 1.)) else (gaussian (y, 1.), gaussian (x, 1.)) in
    let () = observe (a, 1.) in
    let () = observe (b, 2.) in
    (x, (x, y))
}

val main = stream {
  init = infer (1, tuple);
  step (tuple, ()) = 
    let (d, s) = unfold (tuple, ()) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
