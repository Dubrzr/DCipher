exception ImageOutOfBounds of int * int

class image w h =
object (self)

  val mutable src:Sdlvideo.surface =
    Sdlvideo.create_RGB_surface [`HWSURFACE] ~w:w ~h:h ~bpp:16
      ~rmask:Int32.zero ~gmask:Int32.zero ~bmask:Int32.zero ~amask:Int32.zero

  val mutable w:int = w
  val mutable h:int = h

  method updateDims =
    let i = Sdlvideo.surface_info src in
    w <- i.Sdlvideo.w;
    h <- i.Sdlvideo.h

  method getSrc = src
  method setSrc (s:Sdlvideo.surface) =
    src <- s;
    self#updateDims

  method isInBounds x y = x >= 0 && y >= 0 && x < w && y < h

  method getPixel x y =
    if not (self#isInBounds x y) then raise (ImageOutOfBounds (x, y))
    else Sdlvideo.get_pixel_color src x y

  method putPixel x y p =
    if not (self#isInBounds x y) then raise (ImageOutOfBounds (x, y))
    else Sdlvideo.put_pixel_color src x y p

  method getWidth = w
  method getHeight = h

  method load filename =
    src <- Sdlloader.load_image filename;
    self#updateDims

  method display (dst:image) =
    self#render dst#getSrc

  method render (dst:Sdlvideo.surface) =
    Sdlvideo.blit_surface src dst ()

  method iter (f:int -> int -> (int * int * int) -> unit) =
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
        f i j (self#getPixel i j)
      done;
    done;

end
