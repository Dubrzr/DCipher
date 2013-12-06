let bilinearRotation matrix angle =
	let angle = Utils.degreeToRadian angle in
	let (cosA,sinA) = (cos angle),(sin angle) in
	let (width, height) = matrix#getDims in
	let (centerX,centerY) = (Utils.foi (width / 2), Utils.foi (height / 2)) in
	let (matrixW, matrixH) = (Utils.foi height, Utils.foi width) in
	let (w, h) = (abs_float(sinA *. matrixW +. cosA *. matrixH),
				  abs_float(sinA *. matrixH +. cosA *. matrixW)) in
	Utils.printArgs "(width, height)" (width::height::[]);
	Utils.printArgs "(w, h)" ((Utils.iof w)::(Utils.iof h)::[]);
	let newMatrix = new Matrix.matrix (Utils.iof w)
									  (Utils.iof h)
									  false in

	for i = 0 to (Utils.iof w) - 1 do
		for j = 0 to (Utils.iof h) - 1 do
			let (fi, fj) = ((Utils.foi i), (Utils.foi j)) in
			let  (x,y)   = ((centerX -. 
							(cosA *. (centerX -. fi)) -. 
							(sinA *. (centerY -. fj))),
							(centerY +. (sinA *. (fi -. centerX)) +.
							(cosA *. (centerY -. fj)))) in
			let (ax,ay) = Utils.iof (x),Utils.iof (y) and
				(bx,by) = Utils.iof (x) + 1,Utils.iof (y) and
				(cx,cy) = Utils.iof (x),Utils.iof (y) + 1 and
				(dx,dy) = Utils.iof (x) + 1,Utils.iof (y) + 1 and
				ca = ref 0 and 
				cb = ref 0 and 
				cc = ref 0 and 
				cd = ref 0 in
 			
 			let value x y = if (matrix#at x y) then 0 else 255 in
           if (matrix#isInBounds ax ay) then ca := value ax ay;
           if (matrix#isInBounds bx by) then cb := value bx by;
           if (matrix#isInBounds cx cy) then cc := value cx cy;
           if (matrix#isInBounds dx dy) then cd := value dx dy;
   
           let (xf,yf) = (x -. (float ax), y -. (float ay)) in
  
           let grey = int_of_float (
                      ((1. -. xf) *. (1. -. yf) *. (float !ca)) +.
                      ( xf *. (1. -. yf) *. (float !cb)) +.
                      ((1. -. xf) *. yf *. (float !cc)) +.
                      (xf *. yf *. (float !cd)))
                      in
            let pixel = (if grey < 245	 then true else false) in
      		newMatrix#set i ((Utils.iof h) - 1 - j) pixel;
    	done;
    done;
    newMatrix


(* OLD ROTATION *)
let rotate matrix angle =
  let (width,height) = matrix#getDims in
  let (centerX, centerY) = (Utils.foi (width / 2), Utils.foi (height / 2)) in
  let resultMatrix = new Matrix.matrix width height false in
  let cosa = cos (-. angle) and
      sina = sin (-. angle) in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        begin
          let posX =
            Utils.iof (centerX +. (float x -. centerX) *.
                           cosa -. (float y -. centerY) *. sina) in
          let posY =
            Utils.iof (centerY +. (float x -. centerX) *.
                           sina +. (float y -. centerY) *. cosa) in
          if resultMatrix#isInBounds posX posY then
            resultMatrix#set posX posY (matrix#at x y)
        end;
      done; 
    done;
  resultMatrix