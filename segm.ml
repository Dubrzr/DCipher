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
  for x = 0 to matrix#getWidth - 1 do
    matrix#set x lineNumber vect.(x)
  done

let vectToMatrixY matrix columnNumber vect = 
  for y = 0 to matrix#getHeight - 1 do
    matrix#set columnNumber y vect.(y)
  done

let horizontalRlsa binarizedMatrix = 
  let newMatrix = new Matrix.matrix binarizedMatrix#getWidth binarizedMatrix#getHeight false in 
  newMatrix#copyFrom binarizedMatrix;
  for i = 0 to newMatrix#getHeight - 1 do
    vectToMatrixX newMatrix i (getRlsaVect (vectOfMatrixX newMatrix i) newMatrix#getWidth 5)
  done;
  newMatrix

let verticalRlsa binarizedMatrix =
  let newMatrix = new Matrix.matrix binarizedMatrix#getWidth binarizedMatrix#getHeight false in 
  newMatrix#copyFrom binarizedMatrix;
  for i = 0 to newMatrix#getWidth - 1 do
    vectToMatrixY newMatrix i (getRlsaVect (vectOfMatrixY newMatrix i) newMatrix#getHeight 5)
  done;
  newMatrix

let getRlsa matrix =
  let (width,height) = matrix#getDims in
  let newMatrix = new Matrix.matrix width height false in
  let horizontalMatrix = horizontalRlsa matrix and
      verticalMatrix   = verticalRlsa   matrix in 
  for y = 0 to height - 1 do
    for x = 0 to width - 1  do
      if horizontalMatrix#at x y && verticalMatrix#at x y then
        newMatrix#set x y true
    done;
  done;
  newMatrix
(*
let segmMoiCaPlz binarizedMatrix =
  *)












