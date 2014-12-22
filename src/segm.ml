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

let horizontalRlsa binarizedMatrix c =
  let newMatrix = new Matrix.matrix binarizedMatrix#getWidth binarizedMatrix#getHeight false in
  newMatrix#copyFrom binarizedMatrix;
  for i = 0 to newMatrix#getHeight - 1 do
    vectToMatrixX newMatrix i (getRlsaVect (vectOfMatrixX newMatrix i) newMatrix#getWidth c)
  done;
  newMatrix

let verticalRlsa binarizedMatrix c =
  let newMatrix = new Matrix.matrix binarizedMatrix#getWidth binarizedMatrix#getHeight false in
  newMatrix#copyFrom binarizedMatrix;
  for i = 0 to newMatrix#getWidth - 1 do
    vectToMatrixY newMatrix i (getRlsaVect (vectOfMatrixY newMatrix i) newMatrix#getHeight c)
  done;
  newMatrix

let getRlsa matrix =
  let (width,height) = matrix#getDims in
  let newMatrix = new Matrix.matrix width height false in
  let horizontalMatrix = horizontalRlsa matrix 5 and
      verticalMatrix   = verticalRlsa   matrix 5 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1  do
      if horizontalMatrix#at x y && verticalMatrix#at x y then
        newMatrix#set x y true;
    done;
  done;
  newMatrix

(* Travels the matrix from left to right on the y line, returns : *)
(* (nbBlackPixels,) posFirstPixel, posLastPixel) *)
let travelMatrixRight binarizedMatrix y =
  let (width,height) = binarizedMatrix#getDims in
  let r = ref 0 and min = ref 0 and max = ref 0 in
  for x = 0 to width - 1 do
    if binarizedMatrix#at x y then
      begin
        if (!r = 0) then
          begin
            min := x;
          end;
        r := !r + 1;
        max := x;
      end
  done;
  (!r, !min, !max)

(* Find all lines and returns (posY, posYMax, posX, posXMax) *)
let findLinesComponants vect =
  let height = Array.length vect in
  let blackLine = ref false in
  let startPos = ref 0 in
  let endPos = ref 0 in
  let newMatrixlines = Array.make height (0, 0, 0, 0) in
  for y = 0 to height - 1 do
    let (r, min, max) = vect.(y) in
    if r <> 0 then (* Black Line *)
      begin
        if (!blackLine <> true) then (* If last line wasn't black *)
          begin
            startPos := y;
            blackLine := true;
          end
      end
    else (* White Line *)
      begin
        if (!blackLine) then (* If last line was black *)
          begin
            endPos := y - 1;
            if (!startPos <> !endPos) then
              begin
                let minValueVect = Array.make (!endPos - !startPos) 0 in
                let maxValueVect = Array.make (!endPos - !startPos) 0 in
                for i = !startPos to !endPos - 1 do
                  let (rb, minb, maxb) = vect.(i) in
                  minValueVect.(i - !startPos) <- minb;
                  maxValueVect.(i - !startPos) <- maxb;
                done;

                let minValue = Utils.minVect minValueVect in
                let maxValue = Utils.maxVect maxValueVect in
                let (a,b,c,d) = (!startPos,
                                 !endPos,
                                 minValueVect.(minValue),
                                 maxValueVect.(maxValue)) in
                newMatrixlines.(y) <- (a,b,c,d);
              end;
            blackLine := false;
          end
      end
  done;
  let count = ref 0 in
  for i = 0 to height - 1 do
    if (0, 0, 0, 0) <> newMatrixlines.(i) then
      count := !count + 1;
  done;
  let finalVect = Array.make !count (0, 0, 0, 0) in
  let count = ref 0 in
  for i = 0 to height - 1 do
    if (0, 0, 0, 0) <> newMatrixlines.(i) then
      begin
        finalVect.(!count) <- newMatrixlines.(i);
        count := !count + 1;
      end
  done;
  finalVect

(* color lines in img : borders in red, white in yellow. *)
let colorImgLines img binarizedMatrix vectMatrixLines =
  for i = 0 to (Array.length vectMatrixLines) - 1 do
    let (yMin, yMax, xMin, xMax) = vectMatrixLines.(i) in
    for y = yMin to yMax do
      for x = xMin to xMax do
        if (x = xMin || x = xMax || y = yMin || y = yMax) then
          begin
            let c = (255, 0, 0) in
            img#putPixel x y c;
          end
        else
          begin
            if not(binarizedMatrix#at x y) then
              begin
                let c = (255, 255, 0) in
                img#putPixel x y c;
              end
          end
      done;
    done;
  done

let travelAllRight img binarizedMatrix colorize =
  let (width,height) = binarizedMatrix#getDims in
  let vectLines = Array.make height (0, 0, 0) in
  for y = 0 to height - 1 do
    let q = travelMatrixRight binarizedMatrix y in
    vectLines.(y) <- q;
  done;
  let vectLinesComponants = findLinesComponants vectLines in
  colorImgLines img binarizedMatrix vectLinesComponants;
  vectLinesComponants

let cleanMatrixDots mat =
  let (w, h) = mat#getDims in
  let white x y = if mat#isInBounds x y then
    not (mat#at x y) else true in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      if
        not (white i j)       &&
        white (i - 1) (j - 1) &&
        white (i - 1) j       &&
        white (i - 1) (j + 1) &&
        white i       (j - 1) &&
        white i       (j + 1) &&
        white (i + 1) (j - 1) &&
        white (i + 1) j       &&
        white (i + 1) (j + 1)
      then mat#set i j false
    done
  done

let deleteBlackStartFrom mat x y i j =
  let rec aux x y =
    if mat#isInBounds x y && mat#at x y then
      begin
        mat#set x y false;
        if (i, j) = (0, 0) then
          begin
            aux (x-1) (y-1);
            aux (x-1) y;
            aux (x-1) (y+1);
            aux x (y-1);
            aux x (y+1);
            aux (x+1) (y-1);
            aux (x+1) y;
            aux (x+1) (y+1);
          end
        else
          begin
            aux x (y+j);
            aux (x+i) y;
            aux (x+i) (y+j);
          end
      end
  in aux x y

let mediumHeightOfLineVect linesVect =
  let count = ref 0 in
  for i = 0 to (Array.length linesVect) - 1 do
    let (yMin, yMax, xMin, xMax) = linesVect.(i) in
      count := !count + (yMax - yMin);
  done;
  (!count / ((Array.length linesVect)))

let isHereBlacksUseless mat maxHeight =
  let (w, h) = mat#getDims in
  let aux x =
    let count = ref 0 in
    for j = 0 to h - 1 do
      if (!count <= maxHeight) &&
         mat#at x j then
        begin
          count := !count + 1;
        end
      else
        begin
          if (maxHeight >= !count) then
            count := 0;
        end;
    done;
    (maxHeight < !count)
  in
  let i = ref 0 in
  let res = ref false in
  while not(!res) && !i < w do
    res := aux !i;
    i := !i + 1;
  done;
  !res


let deleteBlacksAfterBiRotation mat angle =
  if angle <> 0. then
    begin
      let (w, h) = mat#getDims in
      deleteBlackStartFrom mat 0 0             ( 1) ( 1);
      deleteBlackStartFrom mat 0 (h - 1)       ( 1) (-1);
      deleteBlackStartFrom mat (w - 1) 0       (-1) ( 1);
      deleteBlackStartFrom mat (w - 1) (h - 1) (-1) (-1);
    end

(*
let cleanMatrixBlacks mat =
  let (w, h) = mat#getDims in
  let black x y = if mat#isInBounds x y then
    mat#at x y else true in
  let count = ref 0 in
  let pts = ref [] in
  let rec aux x y =
    if
      !count < 100         &&
      not (List.exists (fun t -> t = (x, y)) !pts) &&
      mat#isInBounds x y    &&
      black x y             &&
      black (x - 1) (y - 1) &&
      black (x - 1) y       &&
      black (x - 1) (y + 1) &&
      black x       (y - 1) &&
      black x       (y + 1) &&
      black (x + 1) (y - 1) &&
      black (x + 1) y       &&
      black (x + 1) (y + 1)
    then
      begin
        Utils.printArgs "cleanMatrixBlacks(x, y)" (x::y::[]);
        count := !count + 1;
        pts := (x, y) :: !pts;

        aux (x-1) (y-1);
        aux (x-1) y;
        aux (x-1) (y+1);
        aux x (y-1);
        aux x (y+1);
        aux (x+1) (y-1);
        aux (x+1) y;
        aux (x+1) (y+1);
      end
  in

  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      count := 0;
      aux i j;
      if !count > 5 then deleteBlackStartFrom mat i j
    done
  done
*)

let findElmt elmt x y =
  let (width,height) = elmt#getDims in
  let pts = ref [] in
  let xmax = ref 0 and
      ymax = ref 0 and
      xmin = ref (width - 1) and
      ymin = ref (height - 1) in
  let rec aux x y =
    if x < 0 || y < 0 ||
      x >= width ||
      y >= height ||
      not(elmt#at x y) ||
      List.exists (fun t -> t = (x, y)) !pts then ()
    else
      begin
        pts := (x, y) :: !pts;

        if x > !xmax then xmax := x;
        if x < !xmin then xmin := x;
        if y > !ymax then ymax := y;
        if y < !ymin then ymin := y;

        aux (x-1) (y-1);
        aux (x-1) y;
        aux (x-1) (y+1);
        aux x (y-1);
        aux x (y+1);
        aux (x+1) (y-1);
        aux (x+1) y;
        aux (x+1) (y+1);
      end
  in
  aux x y;

  let newLine = elmt in
  let resultingChar = new Matrix.matrix (!xmax - !xmin  + 1) (height + 1) false in
  while !pts <> [] do
    let (x, y) = List.hd !pts in
    newLine#set x y false;
    resultingChar#set (x - !xmin) y true;
    pts := List.tl !pts;
  done;(*
  Utils.printArgs "(posX, posY, W, H)"
                   ((!xmin)::(!ymin)::(!xmax - !xmin)::(!ymax - !ymin)::[]);*)
  (resultingChar, newLine)

let findElmts matrix =
  let (width,height) = matrix#getDims in
  let charList = ref [] in
  let actualMatrix = ref matrix in
  let i = ref 0 and j = ref 0 in
  while !i < width do
    while !j < height do
      if (matrix#at !i !j) then
        begin
          let (resultingChar, newMatrix) = (findElmt !actualMatrix !i !j) in
          (*let (charW, charH) = resultingChar#getDims in
          if ()*)
            charList := resultingChar :: !charList;
          actualMatrix := newMatrix;
        end;
      incr j
    done;
    j := 0;
    incr i;
  done;
  !charList


(*
let mAndM firstMatrix secondMatrix =
  let (width,height) = firstMatrix#getDims in
  let newMatrix = new Matrix.matrix width height false in
  for y = 0 to height - 1 do
    for x = 0 to width - 1  do
      if firstMatrix#at x y && secondMatrix#at x y then
        newMatrix#set x y true
    done;
  done;
  newMatrix

let cleanMatrix matrix =
  let (width,height) = matrix#getDims in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if matrix#at x y then
        begin
          if (matrix#isInBounds x (y - 1) && not(matrix#at x (y + 1))) &&
             (matrix#isInBounds x (y + 1) && not(matrix#at x (y + 1))) then
            matrix#set x y false
        end
    done
  done


let segmMoiCaPlz binarizedMatrix =
  let hMatrix = horizontalRlsa binarizedMatrix 20 and
      vMatrix = verticalRlsa   binarizedMatrix 160 in
  let andMatrix = mAndM hMatrix vMatrix in
  let hAndMatrix = horizontalRlsa andMatrix 15 in
  let vhAndMatrix = verticalRlsa hAndMatrix 5 in
  cleanMatrix vhAndMatrix;
  vhAndMatrix

let getPhrases matrix =
  let otherMatrix = matrix in
  let hMatrix = verticalRlsa matrix 10 and
      vMatrix = horizontalRlsa otherMatrix 6 in
  let andMatrix = mAndM hMatrix vMatrix in
  let hAndMatrix = horizontalRlsa andMatrix 4 in
  cleanMatrix hAndMatrix;
  hAndMatrix *)
