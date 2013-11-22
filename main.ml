let rec pause () =
  match Sdlevent.wait_event () with
      Sdlevent.QUIT | Sdlevent.KEYDOWN _ -> ()
    | _ -> pause()

let main () =
  begin
    Sdl.init [`VIDEO];

    if Array.length Sys.argv <= 1 then
      exit 1;

    let screen = Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:16 [`HWSURFACE] and
        img = new Image.image 0 0 in

    img#load Sys.argv.(1);
    img#render screen;

    Sdlvideo.flip screen;
    pause ();

    Sdl.quit ();
    exit 0
  end

let _ = main ()
