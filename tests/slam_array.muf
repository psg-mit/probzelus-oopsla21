val init_f = fun i -> sample (bernoulli (const (0.5)))

val slam = stream {
  init = (true, 0, Array.empty);
  step ((first, x, map), (obs, cmd)) =
    let map = if first then Array.init (100, init_f) else map in
    let wheel_slip = sample (bernoulli (const (0.5))) in
    let x = if first then 0 else (if eval (wheel_slip) then x else add_int (x, cmd)) in
    let o = Array.get (map, x) in
    let () = observe (bernoulli (ite (o, const (0.9), const (0.1))), obs) in
    (pair (const (x), array (map)), (false, x, map))
}

val main = stream {
  init = infer (1, slam);
  step (slam, ()) = 
    let (d, s) = unfold (slam, (false, 0)) in
    let (d, _) = split (d) in
    let () = print_float (mean_int (d)) in
    let () = print_newline (()) in
    ((), s)
}
