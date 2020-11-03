val f_init = 
  let x = sample (gaussian (0., 1.)) in 
  let () = observe (x, 1.) in 
  (x, x)
val f_step =
  fun (x, ()) ->
    let x = sample (gaussian (x, 1.)) in
    (x, x)