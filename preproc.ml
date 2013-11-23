(* =========================== *)
(* ====== PREPROCESSING ====== *)
(* =========================== *)
(*                             *)
(* -> IN : Image               *)
(* -> OUT: Array of Matrix     *)
(*                             *)
(* --------------------------- *)
(*                             *)
(* Dependencies:               *)
(* -> utils.ml                 *)
(* -> convert.ml               *)
(* -> image.ml                 *)
(* -> matrix.ml                *)
(* -> treatment.ml             *)
(* -> rotation.ml              *)
(* -> segm.ml                  *)
(*                             *)
(* --------------------------- *)
(*                             *)
(* -> Last change:  28/11/2013 *)
(*                             *)
(* --------------------------- *)
(*                             *)
(* Steps:                      *)
(* -> Greyscale                *)
(* -> Histeq                   *)
(* -> Median Filter            *)
(* -> Threshold Detection      *)
(* -> Binarization             *)
(*                             *)
(* -> Angle Detection          *)
(* -> Rotation                 *)
(*                             *)
(* -> Paragraph Detection      *)
(* -> Line Detection           *)
(* -> Char Detection           *)
(*                             *)
(* ====== CODE BREAKERS ====== *)
(* =========================== *)

let processAll img screen stepByStep =

	Utils.printTitle "GREYSCALE";
  print_string "------> Start\n";
	Treatment.imageToGrey img;
  let t = Sdltimer.get_ticks () in
	let greyMatrix = Convert.matrixGrey_of_image img in
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);

  img#render screen;
  Sdlvideo.flip screen;
	if (stepByStep) then Utils.pause ();


(*
	Utils.printTitle "CONTRAST HISTOGRAM EQUALIZATION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

	Treatment.histEqualization greyMatrix;
  let img = Convert.image_of_matrixGrey greyMatrix in

  img#render screen;
  Sdlvideo.flip screen;
  if (stepByStep) then Utils.pause ();

  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
*)
(*
  Utils.printTitle "MEDIAN FILTER";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  Treatment.relaxedMedianFilter greyMatrix;
  let img = Convert.image_of_matrixGrey greyMatrix in

  img#render screen;
  Sdlvideo.flip screen;
  if (stepByStep) then Utils.pause ();

  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
*)

  Utils.printTitle "BINARIZATION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let threshold = Treatment.getThreshold greyMatrix in

  Printf.printf "\n Threshold = %f" threshold;

  let booleanMatrix = Convert.matrixBool_of_image img  threshold in
  let img = Convert.image_of_matrixBool booleanMatrix in

  img#render screen;
  Sdlvideo.flip screen;
  if (stepByStep) then Utils.pause ();

  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);


  Utils.printTitle "ROTATION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let angle = 25. in

  Printf.printf "\n Angle = %f" angle;

  let greyMatrix = Rotation.bilinearRotation booleanMatrix angle in
  let img = Convert.image_of_matrixGrey greyMatrix in

  img#render screen;
  Sdlvideo.flip screen;
  if (stepByStep) then Utils.pause ();

  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);


  Utils.printTitle "RLSA";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let booleanMatrix = Segm.getRlsa booleanMatrix in
  let img = Convert.image_of_matrixBool booleanMatrix in

  img#render screen;
  Sdlvideo.flip screen;
  if (stepByStep) then Utils.pause ();

  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
