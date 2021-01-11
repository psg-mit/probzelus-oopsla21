val main_init = infer_init (true, 0., 0.)
val main_step =
  fun ((first, x_0, x), observed) ->
    infer (
      fun ((first, x_0, x), obs) ->
        let (x_0, x) =
          if first then (let x_0 = sample (gaussian(0., 1.)) in (x_0, x_0))
          else (x_0, x) in
        let x = sample (gaussian (x, 1.)) in
        let y = gaussian (x, 1.) in
        let () = observe (y, obs) in
        (x_0, (false, x_0, x)),
      ((first, x_0, x), observed)
    )
