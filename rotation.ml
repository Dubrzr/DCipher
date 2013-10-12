(* MATHS FONCTIONS *)
let abs_half x =
if (x -. float_of_int(truncate(x))) < 0.5 then truncate(x)
else truncate(x)+1;;
let radian_to_degree x = x *. 57.2957795;;
let abs_float x = if x > 0. then x else -. x;;
let decimal x = x -. floor x
let foi x = float_of_int x;;
let iof x = int_of_float x;;
(* image to matrix *)
let img_to_matrix img =
  let newMatrix = Array.make_matrix (surface_info img).w (surface_info img).h 0
  in
  for y = 0 to (surface_info img).h -1 do
    for x = 0 to (surface_info img).w -1 do
      let (r, g, b) = get_pixel_color img ~x:x ~y:y in
      if (r == 0) then newMatrix.(x).(y) <- 1;
    done;
  done;
  newMatrix

(* matrix_to_image *)
let matrix_to_img m img =
  for y = 0 to (surface_info img).h  -1 do
    for x = 0 to (surface_info img).w -1 do
      if (m.(x).(y) == 1) then
        put_pixel img ~x:x ~y:y (map_RGB img (0, 0, 0))
    done
  done
(*** TRAVEL MATRIX ***)
(* run through the matrix with angle *)
(* returns true if detects a black pix, else false *)
let travel_matrix m angle px py =
  let coef_X = ref 1. and ratio_X = ref 0. and
      coef_Y = ref 1. and ratio_Y = ref 0. and
      matrix_W = Array.length m and
      matrix_H = Array.length m.(0) and
      result = ref false and
      x = ref px and
      y = ref py in

  (* we determined the ratio_X & ratio_Y to increment posX & posY *)
  if (angle < 270. && angle > 89.) then (* 90 -> 269 *)
    coef_X := -. 1.;
  if (angle < 180. && angle > 0.) then  (*  0 -> 180 *)
    coef_Y := -. 1.;
  if (!coef_X < 0.) then begin
    ratio_X := !coef_X *. (1. -. decimal(angle /. 90.));
    ratio_Y := !coef_Y *. (decimal(abs_float (angle) /. 90.));
  end
  else begin
    ratio_X := !coef_X *. (decimal(abs_float (angle) /. 90.));
    ratio_Y := !coef_Y *. (1. -. decimal(angle /. 90.));
  end;

  while (!x < foi (Array.length m)-.1.
         && !x >= 0.
         && !y < foi (Array.length m.(0))-.1.
         && !y >= 0.
         && !result <> true) do
     if m.(abs_half(!x)).(abs_half(!y)) == 1  then
      result := true;
     x := !x +. !ratio_X;
     y := !y +. !ratio_Y;
   done;
  !result

(*** BEST ANGLE ***)
(* find best angle between x & y *)
let best_angle m x y =
  let correction x = if (x < 0) then (foi (x) /. 5.6 +. 8.)
	                 else (foi (x) /. 5.6 -. 8.) and
      variance = Array.make (Array.length m.(0)) 0 and
      count1 = ref 0 and count2 = ref 0 in

  for i = 0 to Array.length m.(0) - 1 do
    for j = 0 to Array.length m - 1 do
      if (travel_matrix m (foi x) (foi j) (foi i)) then
        variance.(i) <- variance.(i) + 1;
    done;
      if (variance.(i) == 0) then
        count1 := !count1 + 1;
  done;

  for i = 0 to Array.length m.(0) - 1 do
    for j = 0 to Array.length m - 1 do
      if (travel_matrix m (foi y) (foi j) (foi i)) then
        variance.(i) <- variance.(i) + 1;
    done;
      if (variance.(i) == 0) then
        count2 := !count2 + 1;
  done;

  if (x = 179 && y = 90) then -.90.
  else if (y = 179 && x = 90) then 0.
  else if (!count1 > !count2) then ((foi (y-90)) +. (correction (y-90)))
  else ((foi (x-90)) +. (correction (x-90)))

(*** FIND ANGLE ***)
let find_angle m precision num =
  let angle_vect = Array.make (180 * precision) 0 and
      incr = (1. /. float_of_int(precision)) and
      angle = radian_to_degree (
        atan (
          float_of_int (Array.length m) /. float_of_int (Array.length m.(0))
        )
      ) in
  let ratio_X = abs_float(angle) /. 90. and
      ratio_Y = 1. -. (abs_float(angle) /. 90.) and
      length_X = Array.length m and
      length_Y = Array.length m.(0) in

  (* Parcours tous les angles de inf à sup à partir du point (posx, posy) *)
  (* avec un ratio de parcours (+rat_X, +rat_Y) *)
  let travel_angle matrix inf sup posx posy increm rat_X rat_Y =
    let x = ref posx and y = ref posy and current_angle = ref inf and
        arrange = if (inf = 270.) then (-360)
          else if (inf = 180.) then (-180)
          else if (inf = 90.) then (-180)
          else 0 in

    while (abs_half(!x) < length_X &&
		   abs_half(!y) < length_Y &&
	       !x >= 0. &&
		   !y >= 0.) do
      while (!current_angle < sup) do
        let i = int_of_float(
			                 !current_angle *. (1. /. increm) +.
							 90. *. (1. /. increm)
							 ) in
        if (travel_matrix matrix (!current_angle) !x !y) then
          angle_vect.(i+arrange) <- angle_vect.(i+arrange) + 1;
        current_angle := !current_angle +. increm;
      done;
      current_angle := inf;
      x := !x +. rat_X;
      y := !y +. rat_Y;
    done;
  in

  if (num = 1) then begin (* Parcours de haut en bas *)
	(* Gauche à droite *)
    travel_angle m 270. 360. 0. 0. incr
	ratio_X ratio_Y;
	(* Droite à gauche *)
    travel_angle m 180. 270. (foi (length_X-1)) 0. incr
	(-. ratio_X) ratio_Y;
  end
  else begin (* Parcours de bas en haut *)
	(* Gauche à droite *)
    travel_angle m 0. 90. 0. (foi (length_Y-1)) incr
	ratio_X (-. ratio_Y);
	(* Droite à gauche *)
    travel_angle m 90. 180. (foi (length_X-1)) (foi (length_Y-1)) incr
	(-. ratio_X) (-. ratio_Y);
  end;

  let min = ref 0 in
  for k = 0 to (179 * precision) do
    if (angle_vect.(k) < angle_vect.(!min)) then begin
      min := k;
    end;
  done;
  (!min)