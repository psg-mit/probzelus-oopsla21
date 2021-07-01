val kalman = stream {
  init = (true, const (0.), const (0.));
  step ((first, i, pre_x), obs) =
    let (i, pre_x) =
      if first then (let i = sample (gaussian (const (0.), 1.)) in (i, i))
      else (i, pre_x) in
    let x = sample (gaussian (pre_x, 1.)) in
    let () = observe (gaussian (x, 1.), obs) in
    (x, (false, i, x))
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, ()) = 
    let (d, s) = unfold (kalman, 1.) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
