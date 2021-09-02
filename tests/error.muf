val error = stream {
  init = (true, 0.0, 0.0);
  step ((first, x_init, x_prev), obs) =
    if first then (
      let x0 = sample (gaussian (0.0, 1.0)) in
      let x1 = sample (gaussian (x0, 1.0)) in
      let x2 = sample (gaussian (x1, 1.0)) in
      let x3 = sample (gaussian (x2, 1.0)) in
      let () = observe (gaussian (x3, 1.0), obs) in
      (x0, (false, x0, x0))
    ) else (
      let x = sample (gaussian (x_prev, 1.0)) in
      let () = observe (gaussian (x, 1.0), obs) in
      (x0, (false, x_init, x))
    )
}

val main = stream {
  init = infer (1, error);
  step (error, ()) = 
    let (d, s) = unfold (error, ()) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
