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
            let pixel = (if grey < 245 then true else false) in
      		newMatrix#set i ((Utils.iof h) - 1 - j) pixel;
    	done;
    done;
    newMatrix


(* OLD ROTATION *)
let rotate matrix angle =
	if angle <> 0. then
	let angle = Utils.degreeToRadian angle in
		begin
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
		end
	else
		matrix
  

let max_matrix matrix w h =
  let maxpos = ref(0,0) in
  let max = ref 0 in
  for j=0 to (h-1) do
    for i =0 to (w-1) do
      if (!max < matrix.(i).(j)) then
	begin
	  maxpos := (i,j);
	  max := matrix.(i).(j)
	end
    done;
  done;
  !maxpos

  (* hough *)
let hough matrix =
  let (w, h) = matrix#getDims in
  let pi = 2.*.acos(0.) in
  let rad = pi /. 180. in 
  let pmax = int_of_float( sqrt(float(w*w) +. float(h*h)) +. 1. ) in 
  let co theta = cos(float(theta) *.rad)  in
  let si theta = sin(float(theta) *.rad)  in  
  let hough_matrix = Array.make_matrix pmax 181 0 in
  for i=0 to w-1 do 
    for j=0 to h-1 do 
      if matrix#at i j then
	for theta = 0 to 180 do
	  let p =int_of_float(float(i)*.co (theta) +. float(j)*.si (theta)) in
	  if (p >= 0) then 
	    hough_matrix.(p).(theta) <- hough_matrix.(p).(theta) + 1
	  else
	    hough_matrix.(-p).(theta) <- hough_matrix.(-p).(theta) +1;
	done
    done
  done;
  let (a,b) = max_matrix hough_matrix pmax 181 in 
  Utils.foi (- (90-b)) 
    