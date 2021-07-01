val kalman_helper = stream {
  init = const (0.);
  step (x, ()) =
    let x' = gaussian (x, 1.) in
    (x', sample (x'))
}

val kalman = stream {
  init = init (kalman_helper);
  step (kh, obs) =
    let (x, kh') = unfold (kh, ()) in
    let _ = observe (x, obs) in
    (sample (x), kh')
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, ()) = 
    let (d, s) = unfold (kalman, 1.) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
