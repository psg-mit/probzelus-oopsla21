val slam = stream {
  init = (true, 0., (0., 0.));
  step ((first, x, (map1, map2)), (obs, cmd)) =
    let (map1, map2) = if first then (sample (bernoulli (0.5)), sample (bernoulli (0.5))) else (map1, map2) in
    let wheel_slip = sample (bernoulli (0.5)) in
    let x = if first then 0. else (if wheel_slip then x else plus (x, cmd)) in
    let o = if x then map1 else map2 in
    let () = observe (bernoulli (ite (o, 0.9, 0.1)), obs) in
    ((x, (map1, map2)), (false, x, (map1, map2)))
}

val main = stream {
  init = infer (1, slam);
  step (slam, obs) = unfold (slam, obs)
}