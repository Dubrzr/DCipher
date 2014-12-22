let matrixColor_of_image img =
  let m = new Matrix.matrix img#getWidth img#getHeight (0, 0, 0) in
  img#iter (fun i j value -> m#set i j value);
  m

let matrixGrey_of_image img =
  let m = new Matrix.matrix img#getWidth img#getHeight 0 in
  img#iter (fun i j (r,g,b) -> m#set i j r);
  m

let matrixGrey_of_booleanMatrix mat =
  let m = new Matrix.matrix mat#getWidth mat#getHeight 255 in
  mat#iter (fun i j r -> m#set i j (if r then 0 else 255));
  m


let image_of_matrixGrey mat =
  let im = new Image.image mat#getWidth mat#getHeight in
  mat#iter (fun i j r -> im#putPixel i j (r,r,r));
  im

let matrixBool_of_image img threshold =
  let th = Utils.iof(threshold *. 255.) and
      m = new Matrix.matrix img#getWidth img#getHeight false in
  img#iter (fun i j (r,g,b) -> m#set i j (r < th));
  m

let image_of_matrixBool mat =
  let (blk, wht) = ((0, 0, 0), (255, 255, 255)) and
      im = new Image.image mat#getWidth mat#getHeight in
  mat#iter (fun i j value -> im#putPixel i j (if value then blk else wht));
  im

let float_of_input (a, b) i : float =
  if i = 0 then float a else float b

let float_of_target = float


let matrixOfMatrix (yMin, yMax, xMin, xMax) booleanMatrix =
  let newMatrix = new Matrix.matrix (xMax - xMin + 1) (yMax - yMin + 1) false in
    for y = yMin to yMax do
      for x = xMin to xMax do
        newMatrix#set (x - xMin) (y - yMin) (booleanMatrix#at x y)
      done
    done;
  newMatrix

let float_of_bool x = if x then 1. else 0.
