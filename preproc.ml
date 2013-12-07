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
  let t = Sdltimer.get_ticks () in
  Treatment.imageToGrey img;

	let greyMatrix = Convert.matrixGrey_of_image img in

  img#render screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();
(*
	Utils.printTitle "CONTRAST HISTOGRAM EQUALIZATION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

	Treatment.histEqualization greyMatrix;
  let img = Convert.image_of_matrixGrey greyMatrix in

  img#render screen;
  Sdlvideo.flip screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();
*)


  Utils.printTitle "BINARIZATION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let threshold = Treatment.getThreshold greyMatrix in
  Printf.printf "\n Threshold = %f" threshold;

  let booleanMatrix = Convert.matrixBool_of_image img  threshold in
  let img = Convert.image_of_matrixBool booleanMatrix in

  img#render screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();

  Utils.printTitle "ROTATION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let angle = Rotation.hough booleanMatrix in

  Printf.printf "\nAngle = %f °" angle;
  Printf.printf "\nAngle = %f rad" (Utils.degreeToRadian angle);


  let booleanMatrix = Rotation.bilinearRotation 
        booleanMatrix 
        angle in
  let img = Convert.image_of_matrixBool booleanMatrix in
  img#render screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();
(*
      Utils.printTitle "MEDIAN FILTER";
      print_string "------> Start\n";
      let t = Sdltimer.get_ticks () in

      let greyMatrix = Convert.matrixGrey_of_booleanMatrix booleanMatrix in
      Treatment.relaxedMedianFilter greyMatrix;
      let img = Convert.image_of_matrixGrey greyMatrix in

      img#render screen;

      Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
      print_newline ();
      if (stepByStep) then Utils.pause ();

  *)
(*
  Segm.deleteBlacksAfterBiRotation booleanMatrix angle;
  *)
  let img = Convert.image_of_matrixBool booleanMatrix in
  img#render screen;
  if (stepByStep) then Utils.pause ();

  Utils.printTitle "LINES DETECTION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let linesVect = Segm.travelAllRight img booleanMatrix true in

  img#render screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();


  Utils.printTitle "CHAR DETECTION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let vectOfLinesOfCharMatrix = ref [] in
  for i = 0 to (Array.length linesVect) - 1 do 
    let (a,b,c,d) = linesVect.(i) in (*
    Utils.printArgs "(yMin, yMax, xMin, xMax)" (a::b::c::d::[]); *)
    let lineMatrix = Convert.matrixOfMatrix (a,b,c,d) booleanMatrix in
    Segm.cleanMatrixDots lineMatrix;
    
    let img = Convert.image_of_matrixBool lineMatrix in
    img#render screen;

    let vectOfCharMatrix = Segm.findElmts lineMatrix in(*
    for i = 0 to List.length vectOfCharMatrix - 1 do
      let img = Convert.image_of_matrixBool lineMatrix in
      img#render screen;
    done;*)
    
    vectOfLinesOfCharMatrix := vectOfCharMatrix :: !vectOfLinesOfCharMatrix;
  done;
  
  for i = 0 to (List.length !vectOfLinesOfCharMatrix) - 1 do
    let vectOfCharMatrix = List.nth !vectOfLinesOfCharMatrix i in
    for j = 0 to (List.length vectOfCharMatrix) - 1 do
      let img = Convert.image_of_matrixBool (List.nth vectOfCharMatrix j) in
      img#render screen;
    done    
  done;
  

  img#render screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();





(*
  Utils.printTitle "RLSA";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in
  let booleanMatrix = Segm.getPhrases booleanMatrix in
  let img = Convert.image_of_matrixBool booleanMatrix in

  img#render screen;
  Sdlvideo.flip screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();*)