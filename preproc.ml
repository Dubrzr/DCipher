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


(*************)
(* GREYSCALE *)
(*************) 
let greyscale img screen file tempFile pause display :unit =
  begin
    Utils.printTitle "GREYSCALE"; print_string "------> Start\n";
    let t = Sdltimer.get_ticks () in

    (* processing *)
    Treatment.imageToGrey img;

    (* options *)
    if display then img#render screen;
    if (tempFile <> "null") then img#saveBmp tempFile;

    Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
    print_newline ();
    if pause then Utils.pause ();
  end
(*******)
(* END *)
(*******)

(************)
(* BINARIZE *)
(************)
let binarize img screen tempFile pause display :unit =
  begin
    Utils.printTitle "BINARIZATION"; print_string "------> Start\n";
    let t = Sdltimer.get_ticks () in

    (* processing *)
    img#load tempFile;
    let greyMatrix = Convert.matrixGrey_of_image img in
    let threshold = Treatment.getThreshold greyMatrix in
    Printf.printf "\n Threshold = %f" threshold;
    let booleanMatrix = Convert.matrixBool_of_image img  threshold in
    let img = Convert.image_of_matrixBool booleanMatrix in

    (* options *)
    if display then img#render screen;
    if (tempFile <> "null") then img#saveBmp tempFile;
    
    Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
    print_newline ();
    if pause then Utils.pause ();
  end
  
(*******)
(* END *)
(*******)

(************)
(* ROTATION *)
(************)
let rotate img screen tempFile pause display :unit =
  begin
    Utils.printTitle "ROTATION"; print_string "------> Start\n";
    let t = Sdltimer.get_ticks () in

    (* processing *)
    img#load tempFile;
    let booleanMatrix = Convert.matrixBool_of_image img 0.5 in   
    let angle = Rotation.hough booleanMatrix in
    Printf.printf "\nAngle = %f °" angle;
    Printf.printf "\nAngle = %f rad" (Utils.degreeToRadian angle);
    print_newline (); 
    let booleanMatrix = Rotation.bilinearRotation 
        booleanMatrix 
        angle in  
    Segm.deleteBlacksAfterBiRotation booleanMatrix angle;
    let img = Convert.image_of_matrixBool booleanMatrix in

    (* options *)
    if display then img#render screen;
    if (tempFile <> "null") then img#saveBmp tempFile;
    
    Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
    print_newline ();
    if pause then Utils.pause ();
  end
  
(*******)
(* END *)
(*******)


(******************)
(* LINE DETECTION *)
(******************)
(* Return : linesVect  *)
let linesDetection img screen tempFile pause display =
  begin
    Utils.printTitle "LINES DETECTION"; print_string "------> Start\n";
    let t = Sdltimer.get_ticks () in

    (* processing *)
    img#load tempFile;
    let imgTemp = img in
    let booleanMatrix = Convert.matrixBool_of_image img 0.5 in
    let linesVect = Segm.travelAllRight img booleanMatrix true in

    (* options *)
    if display then img#render screen;
    if (tempFile <> "null") then imgTemp#saveBmp tempFile;

    Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
    print_newline ();
    if pause then Utils.pause ();
    linesVect
  end
(**** ***)
(* END *)
(*******)

(******************)
(* CHAR DETECTION *)
(******************)
(* Return : charArray  *)
let charDetection img screen linesVect tempFile pause display =
  begin
    Utils.printTitle "CHAR DETECTION"; print_string "------> Start\n";
    let t = Sdltimer.get_ticks () in

    (* processing *)
    img#load tempFile;
    let imgTemp = img in
    let booleanMatrix = Convert.matrixBool_of_image img 0.5 in
    let mediumLineHeight = Segm.mediumHeightOfLineVect linesVect in
    let vectOfLinesOfCharMatrix = ref [] in
    for i = 0 to (Array.length linesVect) - 1 do 
      let (a,b,c,d) = linesVect.(i) in
      let lineMatrix = Convert.matrixOfMatrix (a,b,c,d) booleanMatrix in
      if not(Segm.isHereBlacksUseless lineMatrix mediumLineHeight) then
        begin
          Segm.cleanMatrixDots lineMatrix;
          if display then
            begin
              let img = Convert.image_of_matrixBool lineMatrix in
              img#render screen;
            end;
          
          let vectOfCharMatrix = Segm.findElmts lineMatrix in
          vectOfLinesOfCharMatrix := vectOfCharMatrix :: !vectOfLinesOfCharMatrix;
        end
    done;
    let charList = ref [] in
    for i = 0 to (List.length !vectOfLinesOfCharMatrix) - 1 do
      let vectOfCharMatrix = List.nth !vectOfLinesOfCharMatrix i in
      for j = 0 to (List.length vectOfCharMatrix) - 1 do
        let matrix = List.nth vectOfCharMatrix j in
        charList := matrix :: !charList;
        if display then
          begin
            let img = Convert.image_of_matrixBool matrix in
            img#render screen;
          end      
      done
    done;  

    (* options *)
    if display then imgTemp#render screen;
    if (tempFile <> "null") then imgTemp#saveBmp tempFile;

    Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
    print_newline ();
    if pause then Utils.pause ();
    Array.of_list !charList
  end
(*******)
(* END *)
(*******)


let processAll img screen pause display filename =
  (* (*http://tech-algorithm.com/articles/bilinear-image-scaling/*)
  img#load filename;
  img#render screen;
    if pause then Utils.pause (); 
  let booleanMatrix = Convert.matrixBool_of_image img 0.9 in
  let img = Convert.image_of_matrixBool booleanMatrix in
  img#render screen;
    if pause then Utils.pause ();
  let (w, h) = booleanMatrix#getDims in
  let ratio = w / h in
  let booleanMatrix = Matrix.resize booleanMatrix (ratio * 500) 500   in
  let img = Convert.image_of_matrixBool booleanMatrix in
  img#render screen;
    if pause then Utils.pause ();*)

  let tempFile = "thumbs/" ^ filename in
  print_string tempFile;
  greyscale img screen filename tempFile pause display;
  binarize img screen tempFile pause display;
  rotate img screen tempFile pause display;
  let linesVect = linesDetection img screen tempFile pause display in
  let charArray = charDetection img screen linesVect tempFile pause display in
  charArray
(*

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
  
  Segm.deleteBlacksAfterBiRotation booleanMatrix angle;
  let img = Convert.image_of_matrixBool booleanMatrix in
  img#render screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();


  Utils.printTitle "LINES DETECTION";
  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let linesVect = Segm.travelAllRight img booleanMatrix true in

  img#render screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();

  let mediumLineHeight = Segm.mediumHeightOfLineVect linesVect in

  Utils.printTitle "CHAR DETECTION";


  print_string "------> Start\n";
  let t = Sdltimer.get_ticks () in

  let vectOfLinesOfCharMatrix = ref [] in
  for i = 0 to (Array.length linesVect) - 1 do 
    let (a,b,c,d) = linesVect.(i) in
    let lineMatrix = Convert.matrixOfMatrix (a,b,c,d) booleanMatrix in
    if not(Segm.isHereBlacksUseless lineMatrix mediumLineHeight) then
      begin
        Segm.cleanMatrixDots lineMatrix;
        if lineAndCharDisplay then
          begin
            let img = Convert.image_of_matrixBool lineMatrix in
            img#render screen;
          end;
        
        let vectOfCharMatrix = Segm.findElmts lineMatrix in
        vectOfLinesOfCharMatrix := vectOfCharMatrix :: !vectOfLinesOfCharMatrix;
      end
  done;
  let charArray = ref [] in
  for i = 0 to (List.length !vectOfLinesOfCharMatrix) - 1 do
    let vectOfCharMatrix = List.nth !vectOfLinesOfCharMatrix i in
    for j = 0 to (List.length vectOfCharMatrix) - 1 do
      let matrix = List.nth vectOfCharMatrix j in
      charArray := matrix :: !charArray;
      if lineAndCharDisplay then
        begin
          let img = Convert.image_of_matrixBool matrix in
          img#render screen;
        end      
    done
  done;
  

  img#render screen;
  
  Printf.printf "\n<------ Ended in %d ms\n" (Sdltimer.get_ticks () - t);
  print_newline ();
  if (stepByStep) then Utils.pause ();
  Array.of_list !charArray*)