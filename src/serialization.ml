let serialize mat n m =
  let ratio =
    if mat#getWidth > mat#getHeight then
      float m /. float mat#getWidth
    else
      float n /. float mat#getHeight in
  let im = Convert.image_of_matrixBool mat in
  im#resize ratio;
  im#crop n m;
  im#saveBmp "test.bmp";
  Convert.matrixBool_of_image im 0.5
