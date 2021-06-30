val death_fn = fun (_, _) -> eval (sample (bernoulli (0.5)))
val new_track_init_fn = fun _ -> (0, sample (gaussian (0.5, 1.)))
val state_update_fn = fun (tr_num, tr) -> (tr_num, sample (gaussian (tr, 1.)))
val observe_fn = fun (_, tr) -> gaussian (tr, 1.)
val clutter_init_fn = fun _ -> gaussian (tr, 1.)
val obsfn = fun (var, value) -> observe (var, value)

val mtt = stream {
  init = (true, List.nil2);
  step ((first, t : (int * float) list), (inp : float list, cmd)) =
    let last_t = t in
    let t_survived = List.filter (death_fn, last_t) in
    let n_new = eval (sample (poisson (1.0))) in
    let t_new = List.init (n_new, new_track_init_fn) in
    let t_tot = List.append (t_survived, t_new) in
    let t = List.map (state_update_fn, t_tot) in
    let obs = List.map (observe_fn, t) in
    let n_clutter = sub (List.length (inp), List.length (obs)) in
    let () = observe (poisson (0.5), n_clutter) in
    let clutter = List.init (n_clutter, clutter_init_fn) in
    let obs_shuffled = List.append (obs, clutter) in
    let order = sample (gaussian (1., 1.)) in
    let obs_shuffled = List.shuffle (order, obs_shuffled) in
    let () = if (not (lt (n_clutter, 0))) then List.iter2 (obsfn, obs_shuffled, inp) else () in
    (t, (false, t))
}

val main = stream {
  init = infer (1, mtt);
  step (mtt, obs) = unfold (mtt, obs)
}
