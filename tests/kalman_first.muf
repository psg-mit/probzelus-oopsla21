val f_init = 
  let x_0 = sample (gaussian(0., 1.)) in 
  (x_0, x_0)

val f_step = 
  fun ((x_0, x), obs) -> 
    let x = sample (gaussian (x, 1.)) in 
    let y = gaussian (x, 1.) in 
    let () = observe (y, obs) in 
    (x_0, (x_0, x))