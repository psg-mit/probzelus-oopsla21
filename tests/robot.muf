val kalman = stream {
  init = const (0.);
  step (pre_x, obs) =
    let x = sample (gaussian (pre_x, 1.0)) in
    let () = observe (gaussian (x, 1.0), obs) in
    (x, x)
}

val lqr = fun (target, x) -> x

val controller = stream {
    init = ();
    step (_, (target, x)) = (lqr (target, x), ())
}

val robot = stream {
  init = (init (controller), infer (1, kalman));
  step ((c, k), (obs, target)) =
    let (x_dist, k') = unfold (k, obs) in
    let (u, c') = unfold (c, (target, mean_float (x_dist))) in
    let () = print_any_t (x_dist) in
    let () = print_newline (()) in
    (u, (c', k'))
}
