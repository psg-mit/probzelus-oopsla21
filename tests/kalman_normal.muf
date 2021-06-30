val kalman = stream {
  init = 0.;
  step (pre_x, obs) =
    let x = sample (gaussian (pre_x, 1.0)) in
    let () = observe (gaussian (x, 1.0), obs) in
    (x, x)
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, ()) = 
    let (d, s) = unfold (kalman, 1.) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
