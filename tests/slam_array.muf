val init_f = fun i -> sample (bernoulli (const (0.5)))

val slam = stream {
  init = (true, 0., Array.empty);
  step ((first, x, map : float list), (obs, cmd)) =
    let map = if first then Array.init (100, init_f) else map in
    let wheel_slip = sample (bernoulli (const (0.5))) in
    let x = if first then 0. else (if wheel_slip then x else plus (x, cmd)) in
    let o = Array.get (map, x) in
    let () = observe (bernoulli (ite (o, 0.9, 0.1)), obs) in
    ((x, map), (false, x, map))
}

val main = stream {
  init = infer (1, slam);
  step (slam, obs) = unfold (slam, obs)
}
