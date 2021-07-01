val f = fun x -> sample (gaussian (x, 1.))
val g = fun x -> gaussian (x, 1.)
val h = fun (y, obs) -> observe (y, obs)

val kalman = stream {
  init = (true, const (0.));
  step ((first, x), obs) =
    let x = if first then sample (gaussian (const (0.), 1.)) else f (x) in
      let y = g (x) in
      let () = h (y, obs) in
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
