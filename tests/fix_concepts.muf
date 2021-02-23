val step =
  fun ((first, x), (prob, obs)) ->
    let x = if first then sample (prob, gaussian (0., 1.)) else sample (prob, gaussian (x, 1.)) in
    let y = gaussian (x, 1.) in
    let () = observe (prob, (y, obs)) in
    (x, (false, x))

val main_init = infer_init (true, 0.)
val main_step =
  fun (state_dist : (bool * float) dist, args : (_ * float)) ->
    infer (
      fun (state : (bool * float), args : (_ * float)) ->
        step (state, args),
      (state_dist, args)
    )

(* SIMPLE APPROACH *)
(* just encodes what we do already with init and step *)
(* to implement this, we move the semantics of infer partly into this new operator *)
(* how does this work inside an infer statement? *)
val robot = fix main_init main_step

(* COMPLEX APPROACH *)
(* a general "fix" construct that seems to require some dynamic analysis *)

(* "world" interface for observation/command state *)
val init_world = 0.0
val emit (world, output) = output
val get_obs (world) = world
val has_obs (world) = true

val loop =
  fix loop ((state_dist, prob), world) ->
    if not has_obs (world) then () else
    let output_dist, state_dist = main_step (state_dist, (prob, get_obs (world))) in
    loop ((state_dist, prob), emit (world, mean (output_dist)))

val robot = loop (main_init, init_world)

(* how does this work inside an infer statement? *)
