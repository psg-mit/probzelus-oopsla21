val f_init =
  fun (prob, ()) ->
    let x = sample (prob, gaussian (0., 1.)) in 
    let () = observe (prob, (x, 1.)) in 
    x
val f_step =
  fun (x, (prob, ())) ->
    let x = sample (prob, gaussian (x, 1.)) in
    (x, x)