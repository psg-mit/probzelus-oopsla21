val slam = stream {
  init = (true, false, ((const (false)), (const (false))));
  step ((first, x, (map1, map2)), (obs, cmd)) =
    let (map1, map2) = if first then (sample (bernoulli (const (0.5))), sample (bernoulli (const (0.5)))) else (map1, map2) in
    let wheel_slip = sample (bernoulli (const (0.5))) in
    let x = if first then false else (if (eval (wheel_slip)) then x else cmd) in
    let o = if x then map1 else map2 in
    let () = observe (bernoulli (ite (o, const (0.9), const (0.1))), obs) in
    (pair (const (x), pair (map1, map2)), (false, x, (map1, map2)))
}

val main = stream {
  init = infer (1, slam);
  step (slam, ()) = 
    let (d, s) = unfold (slam, (false, false)) in
    let (d, _) = split (d) in 
    let () = print_float (mean_bool (d)) in
    let () = print_newline (()) in
    ((), s)
}
