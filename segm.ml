(* ==== *)
(* RLSA *)
(* ==== *)

let getRlsaVect vect length c =
  let newVect = Array.make length false in
  
  let count = ref 0 and
      j     = ref 0 in
    while (!j < length) do
      let aux = ref !j in
      while (!aux < length && not(vect.(!aux))) do
        aux := !aux + 1;
        count := !count + 1;
      done;
      if (!count <= c) then
        begin
          if (!j = !aux) then
            begin
              newVect.(!j) <- true;              
              j := !j + 1;
            end
          else
            begin
              for i = !j to !aux - 1 do
                newVect.(i) <- true;
              done;
              j := !aux;
            end
        end
      else
        j := !aux;
      count := 0;
    done;
    newVect

let vectOfMatrixX matrix lineNumber =
  let vect = Array.make matrix#getWidth false in
  for x = 0 to matrix#getWidth - 1 do
    vect.(x) <- matrix#at x lineNumber
  done;
  vect 

let vectOfMatrixY matrix columnNumber =
  let vect = Array.make matrix#getHeight false in
  for y = 0 to matrix#getHeight - 1 do
    vect.(y) <- matrix#at columnNumber y
  done;
  vect

let vectToMatrixX matrix lineNumber vect = 
  let newMatrix = new Matrix.matrix matrix#getWidth matrix#getHeight false in
  newMatrix#copyFrom matrix;
  for x = 0 to matrix#getWidth - 1 do
    newMatrix#set x lineNumber vect.(x)
  done;
  newMatrix

let vectToMatrixY matrix columnNumber vect = 
  let newMatrix = new Matrix.matrix matrix#getWidth matrix#getHeight false in
  newMatrix#copyFrom matrix;
  for y = 0 to matrix#getHeight - 1 do
    newMatrix#set columnNumber y vect.(y)
  done;
  newMatrix
(*
let horizontalRlsa binarizedMatrix = 
  let (width,height) = binarizedMatrix#getDims in
  let newMatrix = new Matrix.matrix matrix#getWidth matrix#getHeight false in 
  for i = 0 to height - 1 do
    newMatrix := vectToMatrixX binarizedMatrix i (getRlsaVect (vectOfMatrixX binarizedMatrix i) width 5)
  done;
  !newMatrix

let verticalRlsa binarizedMatrix =
  let (width,height) = getDimsM binarizedMatrix in
  let newMatrix = ref (Array.make_matrix width height false) in 
  for i = 0 to width - 1 do
    newMatrix := vectToMatrixY binarizedMatrix i (getRlsaVect (vectOfMatrixY binarizedMatrix i) height 5)
  done;
  !newMatrix

let getRlsa verticalMatrix horizontalMatrix =
  let (width,height) = getDimsM verticalMatrix in
  let newMatrix = Array.make_matrix width height false in 
  for y = 0 to height - 1 do
    for x = 0 to width - 1  do
      if horizontalMatrix.(x).(y) && verticalMatrix.(x).(y) then
        newMatrix.(x).(y) <- true
    done;
  done;
  newMatrix

let segmMoiCaPlz binarizedMatrix =
  *)












