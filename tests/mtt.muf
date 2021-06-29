val death_fn = fun (_, _) -> eval (sample (bernoulli (const (0.5))))
val new_track_init_fn = fun _ -> (0, sample (gaussian (const (0.5), 1.)))
val state_update_fn = fun (tr_num, tr) -> (tr_num, sample (gaussian (tr, 1.)))
val observe_fn = fun (_, tr) -> gaussian (tr, 1.)
val clutter_init_fn = fun tr -> gaussian (const (float_of_int (tr)), 1.)
val obsfn = fun (var, value) -> observe (var, value)
val to_expr = fun (tr_num, tr) -> pair (const (tr_num), tr)

val mtt = stream {
  init = (true, List.nil2);
  step ((first, t : (int * float) list), inp : float list) =
    let last_t = t in
    let t_survived = List.filter (death_fn, last_t) in
    let n_new = eval (sample (of_distribution (poisson (1.0)))) in
    let t_new = List.init (n_new, new_track_init_fn) in
    let t_tot = List.append (t_survived, t_new) in
    let t = List.map (state_update_fn, t_tot) in
    let t_expr = List.map (to_expr, t) in
    let obs = List.map (observe_fn, t) in
    let n_clutter = sub_int (List.length (inp), List.length (obs)) in
    let () = observe (of_distribution (poisson (0.5)), n_clutter) in
    let clutter = if (not (lt (n_clutter, 0))) then List.init (n_clutter, clutter_init_fn) else List.nil2 in
    let obs_shuffled = List.append (obs, clutter) in
    let order = eval (sample (random_order (List.length (obs_shuffled)))) in
    let obs_shuffled = List.shuffle (order, obs_shuffled) in
    let () = if (not (lt (n_clutter, 0))) then List.iter2 (obsfn, obs_shuffled, inp) else () in
    (lst (t_expr), (false, t))
}

val main = stream {
  init = infer (1, mtt);
  step (mtt, ()) = 
    let (d, s) = unfold (mtt, List.nil2) in
    let () = print_any_t (d) in
    let () = print_newline (()) in
    ((), s)
}
