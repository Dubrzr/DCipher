(* Project: DCipher
 * File: utils.ml
 * Description: utilities functions
 * Date: 09-10-2013
 * Author: CodeBreaker
 *)

let debug = false

let foi = float_of_int
let iof = int_of_float
let soi = string_of_int

let get_img_w img = (Sdlvideo.surface_info img).Sdlvideo.w
let get_img_h img = (Sdlvideo.surface_info img).Sdlvideo.h

let rect_contains r x y =
  x >= r.Sdlvideo.r_x && x <= r.Sdlvideo.r_x + r.Sdlvideo.r_w &&
  y >= r.Sdlvideo.r_y && y <= r.Sdlvideo.r_y + r.Sdlvideo.r_h

let draw_rect dst r c =
  let x = r.Sdlvideo.r_x and
      y = r.Sdlvideo.r_y and
      w = r.Sdlvideo.r_w and
      h = r.Sdlvideo.r_h in
  for i = x to x + w do
    dst#put_pixel i y c;
    dst#put_pixel i (y + h) c;
  done;
  for i = y to y + h do
    dst#put_pixel x i c;
    dst#put_pixel (x + w) i c;
  done

let draw_rects img l c =
  List.iter (fun x -> draw_rect img x c) l

let rect_zero =
{
  Sdlvideo.r_x = 0;
  Sdlvideo.r_y = 0;
  Sdlvideo.r_w = 0;
  Sdlvideo.r_h = 0;
}

let read_char () =
  let c = ref (read_line ()) in
  while String.length !c = 0 do
    c := read_line ();
  done;
    !c.[0]

let print_color (r, g, b) =
  Printf.printf "color [ r=%d ; g=%d ; b=%d ]" r g b
