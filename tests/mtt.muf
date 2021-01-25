val death_fn = fun i -> eval (sample (prob, bernoulli (0.5)))
val new_track_init_fn = fun i -> sample (prob, bernoulli (0.5))
val state_update_fn = fun tr -> sample (prob, bernoulli (tr))
val observe_fn = fun tr -> bernoulli (tr)
val clutter_init_fn = fun i -> bernoulli (tr)
val obsfn = fun (var, value) -> observe (prob, (var, value))

val step =
  fun ((first, t), (prob, (inp, cmd))) ->
    let last_t = t in
    let t = if first then List.nil else t in
    let t_survived = List.filter (death_fn, last_t) in
    let n_new = eval (sample (prob, poisson (1.0))) in
    let t_new = List.init (n_new, new_track_init_fn) in
    let t_tot = List.append (t_survived, t_new) in
    let t = List.map (state_update_fn, t_tot) in
    let obs = List.map (observe_fn, t) in
    let n_clutter = sub (List.length (inp), List.length (obs)) in
    let () = observe (prob, (poisson (0.5), n_clutter)) in
    let clutter = List.init (n_clutter, clutter_init_fn) in
    let obs_shuffled = eval (sample (prob, shuffle (List.append (obs, clutter)))) in
    let () = if (not (lt (n_clutter, 0))) then List.iter2 (obsfn, obs_shuffled, inp) else () in
    (t, (false, t))

val main_init = infer_init (true, 0., 0.)
val main_step =
  fun ((first, t), (prob, (inp, cmd))) ->
    infer (
      fun ((first, t), (prob, (inp, cmd))) ->
        step ((first, t), (prob, (inp, cmd))),
      ((first, t), (prob, (inp, cmd)))
    )
