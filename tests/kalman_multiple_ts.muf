val f = stream {
  init = (true, const (0.));
  step ((first, x), obs) =
    let x' = if first then const (0.) else sample (gaussian (x, 1.)) in
    let y = gaussian (x, 1.) in
    let () = observe (y, obs) in
    (x', (false, x'))
}

val main = stream {
  init = infer (1, f);
  step (f, ()) = 
    let (d, s) = unfold (f, 1.) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
