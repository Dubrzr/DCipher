let iof = int_of_float
let foi = float_of_int

let debug_timer str f =
  let t = Sdltimer.get_ticks () in
  Printf.printf "Starting %s...\n" str;
  let a = f () in
  Printf.printf "%s terminated in %d ms\n" str (Sdltimer.get_ticks () - t);
  a
