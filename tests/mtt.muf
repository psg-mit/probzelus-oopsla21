val death_fn = fun (prob, i) -> eval (sample (prob, bernoulli (0.5)))
val new_track_init_fn = fun (prob, i) -> sample (prob, bernoulli (0.5))
val state_update_fn = fun (prob, tr) -> sample (prob, bernoulli (tr))
val observe_fn = fun (prob, tr) -> bernoulli (tr)
val clutter_init_fn = fun (prob, i) -> bernoulli (tr)
val obsfn = fun (prob, var, value) -> observe (prob, (var, value))

val step =
  fun ((first, t), (prob, (inp, cmd))) ->
    let last_t = t in
    let t = if first then List.nil else t in
    let t_survived = List.filter (prob, death_fn, last_t) in
    let n_new = eval (sample (prob, poisson (1.0))) in
    let t_new = List.init (prob, n_new, new_track_init_fn) in
    let t_tot = List.append (t_survived, t_new) in
    let t = List.map (prob, state_update_fn, t_tot) in
    let obs = List.map (prob, observe_fn, t) in
    let n_clutter = sub (List.length (inp), List.length (obs)) in
    let () = observe (prob, (poisson (0.5), n_clutter)) in
    let clutter = List.init (prob, n_clutter, clutter_init_fn) in
    let obs_shuffled = eval (sample (prob, shuffle (List.append (obs, clutter)))) in
    let () = if (not (lt (n_clutter, 0))) then List.iter2 (prob, obsfn, obs_shuffled, inp) else () in
    (t, (false, t))

val main_init = infer_init (true, List.nil)
val main_step =
  fun (state : (bool * float list), args : (_ * (float list * float))) ->
    infer (
      fun (state : (bool * float list), args : (_ * (float list * float))) ->
        step (state, args),
      (state, args)
    )
