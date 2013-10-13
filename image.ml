(* Project: DCipher
 * File: image.ml
 * Description: image object
 * Date: 09-10-2013
 * Author: CodeBreaker
 *)

class image w h =
object (s)

  val mutable src = Sdlvideo.create_RGB_surface [`HWSURFACE] w h 16
    Int32.zero Int32.zero Int32.zero Int32.zero

  method load_from_file filename =
    src <- Sdlloader.load_image filename

  method set_src s =
    src <- s

  method get_src =
    src

  method get_w =
    (Sdlvideo.surface_info src).Sdlvideo.w

  method get_h =
    (Sdlvideo.surface_info src).Sdlvideo.h

  method is_in_bounds x y =
    x >= 0 && y >= 0 && x < s#get_w && y < s#get_h

  method put_pixel x y c =
    if s#is_in_bounds x y then Sdlvideo.put_pixel_color src x y c
    else invalid_arg ("image#put_pixel: out of bounds")

  method get_pixel x y =
    if s#is_in_bounds x y then Sdlvideo.get_pixel_color src x y
    else invalid_arg ("image#get_pixel: out of bounds")

  method iter_pixels fn =
    for i = 0 to s#get_w -1 do
      for j = 0 to s#get_h -1 do
        s#put_pixel i j (fn (s#get_pixel i j))
      done
    done

  method zoom x y =
    src <- Sdlgfx.rotozoomSurfaceXY src 0. x y false

  method blit_on ?(pos=Utils.rect_zero) dst =
    Sdlvideo.blit_surface ~src:src ~dst:(dst#get_src) ~dst_rect:pos ()

  method blit_center_on dst =
    let pos =
      {
        Sdlvideo.r_x = dst#get_w / 2 - s#get_w / 2;
        Sdlvideo.r_y = dst#get_h / 2 - s#get_h / 2;
        Sdlvideo.r_w = 0; Sdlvideo.r_h = 0;
      } in
    s#blit_on ~pos:pos dst;

  method clip rect =
    let w = rect.Sdlvideo.r_w and h = rect.Sdlvideo.r_h in
    let nsrc = Sdlvideo.create_RGB_surface [`HWSURFACE] w h 16
      Int32.zero Int32.zero Int32.zero Int32.zero in
    Sdlvideo.blit_surface ~src_rect:rect ~src:src ~dst:nsrc ();
    src <- nsrc

  method fill c =
    Sdlvideo.fill_rect src (Sdlvideo.map_RGB src c)

  method duplicate =
    let im = new image s#get_w s#get_h in
    s#blit_on im;
    im

end

let img_load filename () =
  let img = new image 1 1 in
  img#load_from_file filename;
  img
