(* Project: DCipher
 * File: ocr.ml
 * Description: optical character reading
 * Date: 09-10-2013
 * Author: CodeBreaker
 *)

let rec pause () =
  match Sdlevent.wait_event () with
    Sdlevent.QUIT
  | Sdlevent.KEYDOWN _ -> ()
  | _ -> pause ()

let clean () =
  begin
    Sdlttf.quit ();
    Sdl.quit ();
  end

let init () =
  begin
    Sdl.init [`VIDEO];
    Sdlttf.init ();
    at_exit clean;
  end

let read_text imgpath train () =
  init ();

  let image = Image.img_load imgpath () in

  if Utils.debug then print_endline "Starting image preprocessing...";
  let t = ref (Sdltimer.get_ticks ()) in
  Preprocessing.process image ();
  if Utils.debug then print_endline ("Terminated in " ^
    Utils.soi (Sdltimer.get_ticks () - !t) ^ " ms");

  if Utils.debug then print_endline "Starting character finding...";
  t := Sdltimer.get_ticks ();
  let chars_bb = Charsfinding.find_chars image in
  if Utils.debug then print_endline ("Terminated in " ^
    Utils.soi (Sdltimer.get_ticks () - !t) ^ " ms");

  let text = ref "" in

  if not train then
    begin
      if Utils.debug then print_endline "Character recognition...";
      t := Sdltimer.get_ticks ();
      text := Charsrecognition.read_text_from_image image chars_bb;
      if Utils.debug then print_endline ("Terminated in " ^
        Utils.soi (Sdltimer.get_ticks () - !t) ^ " ms");

      if Utils.debug then
        begin
          let (w, h, bpp) = (image#get_w, image#get_h, 16) in
          let screen = new Image.image w h in
          screen#set_src (Sdlvideo.set_video_mode ~w:w ~h:h ~bpp:bpp [`HWSURFACE]);
          ignore (Sys.command "i3-msg floating enable");

          image#blit_on screen;
          Sdlvideo.flip screen#get_src;

          if Utils.debug then
            pause ();

          Printf.printf "Text: %s\n" !text
        end;
    end
  else
    begin
      if Utils.debug then print_endline "Character recognition [train mode]";
      let (w, h, bpp) = (100, 100, 16) in
      let screen = new Image.image w h in
      screen#set_src (Sdlvideo.set_video_mode ~w:w ~h:h ~bpp:bpp [`HWSURFACE]);
      ignore (Sys.command "i3-msg floating enable");
      Charsrecognition.train screen image chars_bb;
    end;
