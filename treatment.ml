(* ========= *)
(* GREYSCALE *)
(* ========= *)

let level (r,g,b) =
  (0.3 *. (float r) +. 0.59 *. (float g) +. 0.11 *. (float b)) /. 255.

let colorToGrey (r,g,b) =
  let grey = int_of_float (level (r,g,b) *. 255.) in (grey, grey, grey)

let imageToGrey img =
  img#iter (fun i j (r,g,b) -> img#putPixel i j (colorToGrey (r,g,b)))

(* =============================== *)
(* CONTRAST HISTOGRAM EQUALIZATION *)
(* =============================== *)

let createGreyHistogram greyMatrix  =
  let vect = Array.make 256 0. in
  greyMatrix#iter (fun i j value -> vect.(value) <- vect.(value) +. 1.);
  vect

(* vect (0 to 255) -> normalizedVect (0. to 1.) *)
let normalizeVect vect n =
  for k = 0 to (Array.length vect) - 1 do
    vect.(k) <- vect.(k) /. (Utils.foi n);
  done

let histEqualization matrix =
  let (width,height) = matrix#getDims in
  let greyVect = createGreyHistogram matrix in
  let nbPixels = width * height in
    normalizeVect greyVect nbPixels;
    let vectTransform = Array.make 256 0. in
      for i = 0 to 255 do
        let total = ref 0. in
          for j = 0 to i do
            total := !total +. greyVect.(j);
          done;
        vectTransform.(i) <- 255. *. !total;
      done;
    matrix#iter (fun i j value ->matrix#set i j (Utils.iof vectTransform.(value)))

(* =================== *)
(* THRESHOLD DETECTION *)
(* =================== *)

let createGreyHistogram greyMatrix  =
  let vect = Array.make 256 0. in
  greyMatrix#iter (fun i j value -> vect.(value) <- vect.(value) +. 1.);
  vect

let normalizeVect vect n =
  for k = 0 to (Array.length vect) - 1 do
    vect.(k) <- vect.(k) /. (Utils.foi n);
  done

let averageVect normalizedVect j =
  let ave = ref 0. in
  for k = 0 to j do
    ave := !ave +. normalizedVect.(k) *. (Utils.foi k);
  done;
  !ave

let varianceVect normalizedVect j =
  let var = ref 0. in
  for k = 0 to j do
    var := !var +. normalizedVect.(k);
  done;
  !var

let otsu normalizedVect k =
  let varK = varianceVect normalizedVect k in
      varK *. (1. -. varK) *. ((averageVect normalizedVect 255) *. varK -.
        (averageVect normalizedVect (Array.length normalizedVect - 1))) ** 2.

let getThreshold matrix =
  let (width,height) = matrix#getDims in
  let greyVect = createGreyHistogram matrix in
    let nbPixels = width * height in
    normalizeVect greyVect nbPixels;

  let vect = Array.make 256 0. in
    for i = 0 to 255 do
      vect.(i) <- otsu greyVect i;
    done;
  Utils.foi (Utils.maxVect vect) /. 256. 

(* ================== *)
(* CONVOLUTION MATRIX *)
(* ================== *)

(* Various filters *)
let random1Kernel =
[| [| 1.;  2.;  1.|];
   [| 2.;  4.;  2.|];
   [| 1.;  2.;  1.|]
|]
let random2Kernel =
[| [| 2.;  5.;  2.|];
   [| 5.; 20.;  5.|];
   [| 2.;  5.;  2.|]
|]
let random3Kernel =
[| [| 0.; -1.;  0.|];
   [|-1.;  5.; -1.|];
   [| 0.; -1.;  0.|]
|]
let random4Kernel =
[| [|-1.;  0.; -1.|];
   [| 0.;  3.;  0.|];
   [|-1.;  0.; -1.|]
|]
let random5Kernel =
[| [| 1.;  2.;  1.|];
   [| 2.;  5.;  2.|];
   [| 1.;  2.;  1.|]
|]
let random6Kernel =
[| [| 1. /. 9.;  1. /. 9.;  1. /. 9.|];
   [| 1. /. 9.;  1. /. 9.;  1. /. 9.|];
   [| 1. /. 9.;  1. /. 9.;  1. /. 9.|]
|]
let antialiasingLowKernel =
[| [| 0.25; 0.25|];
   [| 0.25; 0.25|]
|]
let antialiasingMediumKernel =
[| [| 0.;  2.;  0.|];
   [| 2.;  2.;  2.|];
   [| 0.;  2.;  0.|]
|]
let antialiasingMaxKernel =
[| [| 0.  ; 0.  ; 0.75; 0.  ; 0.  |];
   [| 0.  ; 0.75; 0.75; 0.75; 0.  |];
   [| 0.75; 0.75; 1.  ; 0.75; 0.75|];
   [| 0.  ; 0.75; 0.75; 0.75; 0.  |];
   [| 0.  ; 0.  ; 0.75; 0.  ; 0.  |]
|]
let laplacienKernel =
[| [|-1.; -1.; -1.|];
   [|-1.;  8.; -1.|];
   [|-1.; -1.; -1.|]
|]
let embossKernel =
[| [|-1.; -1.;  0.|];
   [|-1.;  0.;  1.|];
   [| 0.;  1.;  1.|]
|]
let motionBlurKernel =
[| [| 1.;  0.;  0.|];
   [| 0.;  1.;  0.|];
   [| 0.;  0.;  1.|]
|]
let motionBlurNegatifKernel =
[| [|-1.;  0.;  0.|];
   [| 0.; -1.;  0.|];
   [| 0.;  0.;  -1.|]
|]
let motionBlurInverseKernel =
[| [| 0.;  1.;  1.|];
   [| 1.;  0.;  1.|];
   [| 1.;  1.;  0.|]
|]
let gaussianKernel =
[| [| 1. /. 16.;  2. /. 16.;  1. /. 16.|];
   [| 2. /. 16.;  4. /. 16.;  2. /. 16.|];
   [| 1. /. 16.;  2. /. 16.;  1. /. 16.|]
|]


(* greyMatrix * x * y * kernel -> 0 to 255 *)
let applyMatrix x y matrix kernel bias =
  let (width,height) = Utils.getDimsM kernel in
  let r = ref 0. and 
			f = ref 0. and
  		size = width / 2 in
  for my = 0 to height - 1 do
      for mx = 0 to width - 1 do
        f := !f +. kernel.(mx).(my);
        let nx = x + mx - size in
        let ny = y + my - size in
        if (matrix#isInBounds nx ny) then
        begin
          let r1 = Utils.foi (matrix#at nx ny) in
          r := !r +. r1 *. kernel.(mx).(my);
        end
    done;
  done;
  if !f > 0. then (!r /. !f +. bias) else (!r +. bias)

(* GreyMatrix * Kernel -> GreyMatrix *)
let convolution greyMatrix kernel =
  let (width,height) = greyMatrix#getDims in
  let (widthK,heightK) = Utils.getDimsM kernel in
  let (widthK,heightK) = (widthK / 2,heightK / 2) in
  let newMatrix = new Matrix.matrix width height 0 in
  for y = heightK to (height - 1 - heightK) do
    for x = widthK to (width - 1 - widthK) do
      newMatrix#set x y (Utils.iof (applyMatrix x y greyMatrix kernel 0.));
    done;
  done;
  newMatrix

(* ===================== *)
(* RELAXED MEDIAN FILTER *)
(* ===================== *)

(* Checks if x exists in l between inf and sup (inf start at 0)*)
let existVectInterval vect e inf sup =
  let k = ref inf and r = ref false in
  while (!k < sup && !r <> true) do
    if vect.(!k) = e then r := true
    else k := !k + 1
  done;
  !r

let medianValue vect e =
  let k = ref 0 in
    while vect.(!k) = -1 do
      k := !k + 1
    done;
    let median = vect.((!k + 8) / 2) in
    let medianPos = ((!k + 8) / 2) in
    if existVectInterval vect e medianPos (medianPos + 1) then
      e
    else
      median

let isInBoundAddToVect matrix x y vect w =
  for i = x - (w / 2) to x + (w / 2) do
    for j = y - (w / 2) to y + (w / 2) do
      if matrix#isInBounds i j then
        vect.((i mod w) * w + j mod w) <- matrix#at i j;
    done;
  done

let relaxedMedianFilter matrix =
  matrix#iter (fun i j value -> matrix#set i j 
    (
      let vect = Array.make 9 (-1) in
      isInBoundAddToVect matrix i j vect 3;
      Array.sort (fun i j -> i - j) vect;
      medianValue vect (matrix#at i j)
    )
  )