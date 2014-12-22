let scrn = ref (Obj.magic None)

let charset = "abcdefghijklmnopqrstufwxyz" ^
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ^
  "0123456789"

let o = new Ocr.ocr charset

let startGraphial imgfile =
  let g = new Gui.gui imgfile in
  g#start

let startNotGraphial imgfile show =
  let img = new Image.image 0 0 in
  img#load imgfile;
  let chars = Array.map (fun x ->
    Serialization.serialize x 16 16) (Preproc.processAll img) in
  if show then
    begin
      Array.iter (fun x ->
        Sdlvideo.fill_rect !scrn (Sdlvideo.map_RGB !scrn Sdlvideo.black);
        (Convert.image_of_matrixBool x)#render !scrn;
        Sdltimer.delay 100) chars;
    end;
  let str = o#run chars in
  Printf.printf "%s\n%!" str

let main () =
  begin
    let graphical = ref false and
        imgfile = ref "" and
        train = ref 0 and
        fontname = ref "" and
        str = ref "" and
        show = ref false in

    let specs = [
      "--train", Arg.Set_int train, "Train the neural network";
      "--imgfile", Arg.Set_string imgfile, "Set the image input";
      "--fontname", Arg.Set_string fontname, "Set the font for training";
      "--graphical", Arg.Set graphical, "Kikou mode";
      "--demo", Arg.Set_string str, "ANN demo";
      "--show", Arg.Set show, "Show chars";
    ] in Arg.parse specs (fun _ -> ()) "Usage: call 911";

    if !show then
      scrn := Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:32 [`HWSURFACE];

    if Sys.file_exists "ann" then
      o#load "ann";

    if !train <> 0 then
      begin
        if !fontname = "" then raise (Failure "missing fontname");
        o#train !train !fontname;
        o#save "ann"
      end;

    if !imgfile <> "" then
      begin
        if !graphical then
          startGraphial !imgfile
        else
          startNotGraphial !imgfile !show
      end;

    if !str <> "" then
      begin
        if !fontname = "" then raise (Failure "missing fontname");
        let scrn =
           if !show then Sdlvideo.set_video_mode
             ~w:640 ~h:480 ~bpp:32 [`HWSURFACE]
           else Obj.magic None in
        let font = Sdlttf.open_font !fontname 12 in
        let img = new Image.image 0 0 in
        let arr = Array.init (String.length !str) (fun i ->
          img#setSrc (Sdlttf.render_glyph_solid font
                        !str.[i] Sdlvideo.black);
          img#crop 16 16;
          if !show then
            begin
              img#render scrn;
              Utils.pause ()
            end;
          Convert.matrixBool_of_image img 0.5) in
        Printf.printf "%s\n" (o#run arr);
      end;
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
