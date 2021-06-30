val kalman = stream {
  init = (true, 0.);
  step ((first, x), obs) =
    let x = if first then sample (gaussian (0., 1.)) else sample (gaussian (x, 1.)) in
    (x, (false, x))
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, ()) = 
    let (d, s) = unfold (kalman, 1.) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
