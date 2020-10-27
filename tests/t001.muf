val a = 1

val b = true

(* val c = "Hi!" *)

val tt = true

val ff = false

val d = (1, 2.5, 3)

val (d1, d2, d3) = (1, 2.5, 3)

val id = fun x -> x

val aa = id(a)

val e =
  let x = 41 in
  print_int(x)

val f =
  let x =
    let y = 2 in succ(y)
  in
  print_int(x)

val g =
  let a =
    if true then 1 else -1
  in
  print_int(a)