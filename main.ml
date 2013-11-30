let main () =
  begin
    if Array.length Sys.argv < 2 then
      exit 1;

    Sdl.init [`VIDEO];

    let screen = Sdlvideo.set_video_mode ~w:640 ~h:480 ~bpp:16 [`HWSURFACE] in
    ignore (Sys.command "i3-msg floating enable"); (* forget this... *)

    let img = new Image.image 0 0 in
    img#load Sys.argv.(1);

    img#render screen;
    Sdlvideo.flip screen;

    Utils.pause ();

    Sdl.quit ();
  end

let _ =
  Printexc.record_backtrace true;
  try Utils.debugTimer "program" (Printexc.print main) with _ ->
   begin
     Printf.printf "Error: ";
     Printexc.print_backtrace stdout;
   end;
   exit 0;
