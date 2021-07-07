val get = fun (map, _) -> map

val slam = stream {
  init = (true, const (0.), (const (false)));
  step ((first, x, map), (obs, cmd)) =
    let map = if first then sample (bernoulli (const (0.5))) else map in
    let wheel_slip = sample (bernoulli (const (0.5))) in
    let x = if first then const (0.) else (if (eval (wheel_slip)) then x else add (x, const (cmd))) in
    let o = get (map, eval (x)) in
    let () = observe (bernoulli (ite (o, const (0.9), const (0.1))), obs) in
    (pair (x, map), (false, x, map))
}

val main = stream {
  init = infer (1, slam);
  step (slam, ()) = 
    let (d, s) = unfold (slam, (false, 1.)) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
