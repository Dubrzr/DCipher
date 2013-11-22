let matrixColor_of_image img =
  let m = new Matrix.matrix img#getWidth img#getHeight (0, 0, 0) in
  img#iter (fun i j value -> m#set i j value);
  m

let image_of_matrixColor mat =
  let im = new Image.image mat#getWith mat#getHeight in
  mat#iter (fun i j value -> im#putPixel i j value);
  im

let matrixBool_of_image img threshold =
  let th = Utils.iof(threshold *. 255.) and
      m = new Matrix.matrix img#getWith img#getHeight false in
  img#iter (fun i j value -> m#set i j (value >= th));
  m

let image_of_matrixBool mat =
  let (blk, wht) = ((0, 0, 0), (255, 255, 255)) and
      im = new Image.image mat#getWith mat#getHeight in
  mat#iter (fun i j value -> im#putPixel i j (if value then blk else wht));
  im
