val main_init = infer_init (0., 0., 0., 0.)
val main_step =
  fun (state : (float * float * float * float), args : (_ * float)) ->
    infer (
      fun ((x_p, x_pp, x_ppp, x_pppp), (prob, obs)) ->
        let x = sample (prob, gaussian (x_p, 1.)) in
        (x_pppp, (x, x_p, x_pp, x_ppp)),
      (state, args)
    )
