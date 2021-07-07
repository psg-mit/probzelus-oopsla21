val get = fun (map, _) -> map

val slam = stream {
  init = (true, 0., (const (false)));
  step ((first, x, map), (obs, cmd)) =
    let map = if first then sample (bernoulli (const (0.5))) else map in
    let wheel_slip = sample (bernoulli (const (0.5))) in
    let x = if first then 0. else (if (eval (wheel_slip)) then x else eval (add (const (x), cmd))) in
    let o = get (map, x) in
    let () = observe (bernoulli (ite (o, 0.9, 0.1)), obs) in
    ((x, map), (false, x, map))
}

val main = stream {
  init = infer (1, slam);
  step (slam, ()) = 
    let (d, s) = unfold (slam, (1., 1.)) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
