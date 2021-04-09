val f = fun x -> sample (gaussian (x, 1.))
val g = fun x -> gaussian (x, 1.)
val h = fun y -> observe (y, obs)

val kalman = stream {
  init = (true, 0.);
  step ((first, x), obs) =
    let x = if first then sample (gaussian (0., 1.)) else f (x) in
      let y = g (x) in
      let () = h (y) in
      (x, (false, x))
}

val main = stream {
  init = infer (1, kalman);
  step (kalman, obs) = unfold (kalman, obs)
}
