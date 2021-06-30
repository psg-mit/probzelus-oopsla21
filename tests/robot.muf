val kalman = stream {
  init = 0.0;
  step (pre_x, obs) =
    let x = sample (gaussian (pre_x, 1.0)) in
    let () = observe (gaussian (x, 1.0), obs) in
    (x, x)
}

val controller = stream {
    init = ();
    step (_, (target, x)) = ((), x)
}

val robot = stream {
  init = (0.0, init (controller), infer (1, kalman));
  step ((c, k), (obs, target)) =
    let (x_dist, k') = unfold (k, obs) in
    let (u, c') = unfold (c, (target, mean (x_dist))) in
    let () = print_any_t (u) in
    let () = print_newline (()) in
    (u, (c', k'))
}
