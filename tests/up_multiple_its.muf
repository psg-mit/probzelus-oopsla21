val f = stream {
  init = (const (0.), const (0.), const (0.), const (0.));
  step ((x_p, x_pp, x_ppp, x_pppp), obs) =
    let x = gaussian (x_p, 1.) in
    let _ = observe (x, 1.0) in
    (x_pppp, (sample (x), x_p, x_pp, x_ppp))
}

val main = stream {
  init = infer (1, f);
  step (f, obs) =     
    let (d, s) = unfold (f, 1.) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
