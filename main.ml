(*let load imagePath =
  Sdlvideo.load_Bmp imagepath

let saveBmp image imagePath =
  Sdlvideo.save_BMP image imagePath

let binarize imagepath =
    processing (Main.binarize) ("thumbs/temp.bmp")

let binar imgpath () =
  let imgload = Sdlloader.load_image imgpath in
  let img2 = Treatment.image2grey imgload in
  let img3 = Treatment.image2bin img2 in
  let img4 = Bruit.filtre_med img3 in
  let angle = Rotation.det_angle img4 in
  let img5 = Rotation.rotation img4 angle in
    begin
      Printf.printf "Binarisation";
      print_newline ();
      Sdlvideo.save_BMP img3 "binarsource.bmp";
      Printf.printf "Binarisation terminé";
      print_newline ();
      Printf.printf "Application du filtre";
      print_newline ();
      Sdlvideo.save_BMP img4 "filtersource.bmp";
      Printf.printf "Application du filtre terminé";
      print_newline ();
      Printf.printf "Rotation automatique de l'image";
      print_newline ();
      Sdlvideo.save_BMP img5 "treatedsource.bmp";
      Printf.printf "Rotation terminé";
      print_newline ();
    end
*)

let main () =
  begin
    let train = ref 0 in
    let percent = ref 1 in
    let filename = ref "" in
    let specs = [
      "--train", Arg.Set_int train, "Train The Neural Network";
      "--percent", Arg.Set_int percent, "Neural Network Progress";
      "-f", Arg.Set_string filename, "Specify Your Filename";
    ] in Arg.parse specs (fun _ -> ()) "Usage: Call 911";


    (* let screen = Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:16 [`HWSURFACE] in *)
    (* ignore (Sys.command "i3-msg floating enable"); (\* forget this... *\) *)


    let screen = Sdlvideo.set_video_mode ~w:600 ~h:600 ~bpp:16 [`HWSURFACE] and
      img = new Image.image 0 0 in

    img#load !filename;
    

    if !train <> 0 then
        Utils.debugTimer "training" (fun () -> Ocr.train !train !percent);

    Preproc.processAll img screen true true !filename;

    let screen = Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:32 [`HWSURFACE] in
    let img = new Image.image 0 0 in
    img#load !filename;
    Preproc.processAll img screen true;
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
