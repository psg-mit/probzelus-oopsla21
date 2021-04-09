val f = stream {
  init = (true, 0.);
  step ((first, x), obs) =
    let x' = if first then 0. else sample (gaussian (x, 1.)) in
    let y = gaussian (x, 1.) in
    let () = observe (y, obs) in
    (x', (false, x'))
}

val main = stream {
  init = infer (1, f);
  step (f, obs) = unfold (f, obs)
}
