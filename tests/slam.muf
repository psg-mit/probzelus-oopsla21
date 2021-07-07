val get = fun (map, _) -> map

val slam = stream {
  init = (true, 0, (const (false)));
  step ((first, x, map), (obs, cmd)) =
    let map = if first then sample (bernoulli (const (0.5))) else map in
    let wheel_slip = sample (bernoulli (const (0.5))) in
    let x = if first then 0 else (if eval (wheel_slip) then x else add_int (x, cmd)) in
    let o = get (map, x) in
    let () = observe (bernoulli (ite (o, const (0.9), const (0.1))), obs) in
    (pair (const (x), map), (false, x, map))
}

val main = stream {
  init = infer (1, slam);
  step (slam, ()) = 
    let (d, s) = unfold (slam, (false, 1)) in
    let (d, _) = split (d) in 
    let () = print_float (mean_int (d)) in
    let () = print_newline (()) in
    ((), s)
}

