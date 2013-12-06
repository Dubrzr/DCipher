let main () =
  begin
    let train = ref 0 in
    let percent = ref 1 in

<<<<<<< HEAD
    let screen = Sdlvideo.set_video_mode ~w:640 ~h:800 ~bpp:16 [`HWSURFACE] and
        img = new Image.image 0 0 in
=======
    let specs = [
      "--train", Arg.Set_int train, "train the neural network";
      "--percent", Arg.Set_int percent, "coucou :)";
    ] in Arg.parse specs (fun _ -> ()) "usage: call 911";
>>>>>>> be87bd39dc1bf67e4738516c5b73e641ad02d713

    (* let screen = Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:16 [`HWSURFACE] in *)
    (* ignore (Sys.command "i3-msg floating enable"); (\* forget this... *\) *)

    if !train <> 0 then
        Utils.debugTimer "training" (fun () -> Ocr.train !train !percent);

<<<<<<< HEAD
    Preproc.processAll img screen true;
=======
    for i = 0 to 1 do
      for j = 0 to 1 do
        let a = Ocr.run (i, j) in
        Printf.printf "%d xor %d = %d (%f)\n" i j (int_of_float (a +. 0.5)) a;
      done;
    done;
>>>>>>> be87bd39dc1bf67e4738516c5b73e641ad02d713

    Printf.printf "\n";
  end

let _ =
  Printexc.record_backtrace true;
  try
    Sdl.init [`VIDEO];
    Sdlttf.init ();
    Utils.debugTimer "program" main;
    Sdlttf.quit ();
    Sdl.quit ();
  with e ->
   begin
     Printf.printf "Error: %s\n" (Printexc.to_string e);
     Printexc.print_backtrace stdout;
   end;
   exit 0;
