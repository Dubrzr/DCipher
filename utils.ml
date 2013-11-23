let iof x = int_of_float x
let foi x = float_of_int x

let debugTimer str f =
  let t = Sdltimer.get_ticks () in
  Printf.printf "Starting %s...\n" str;
  let a = f () in
  Printf.printf "%s terminated in %d ms\n" str (Sdltimer.get_ticks () - t);
  a

let rec pause () =
  match Sdlevent.wait_event () with
      Sdlevent.QUIT | Sdlevent.KEYDOWN _ -> ()
    | _ -> pause()

let printTitle title =
	begin
		let length = String.length title - 1 in

		print_string "\n(* ";
		for i = 0 to length do
			print_string "=";
		done;
		print_string " *)\n(* ";
		print_string (String.uppercase title);
		print_string " *)\n(* ";
		for i = 0 to length do
			print_string "=";
		done;
		print_string " *)\n";
	end
	
let printArgs str args =
	print_string ("\n" ^ str ^ " = (");
	let rec printBidule lt = match lt with
		| [] -> print_string ")";
		| e::l -> print_int e; if l <> [] then print_string ", "; printBidule l;
	in
	printBidule args
		
			


let absHalf x =
if (x -. float_of_int(truncate(x))) < 0.5 then
  truncate(x)
else
  truncate(x)+1;;

let absFloat x = if x > 0. then x else -. x
let pi = 3.14159265359
let radianToDegree x = x *. 57.2957795
let degreeToRadian x = pi *. x /. 180.
let decimal x = x -. floor x

let getDimsM matrix =
  (Array.length matrix,
   Array.length matrix.(0))

 let maxVect vect =
  let max = ref 0 in
  for i = 0 to (Array.length vect) - 1 do
    if vect.(i) > vect.(!max) then max := i;
  done;
  !max