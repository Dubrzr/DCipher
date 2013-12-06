exception ImageOutOfBounds of int * int

class image w h =
object (self)

  val mutable src:Sdlvideo.surface =
    Sdlvideo.create_RGB_surface [`HWSURFACE] ~w:w ~h:h ~bpp:16
      ~rmask:Int32.zero ~gmask:Int32.zero ~bmask:Int32.zero ~amask:Int32.zero

  val mutable w:int = w
  val mutable h:int = h

  initializer
    Sdlvideo.fill_rect src (Sdlvideo.map_RGB src Sdlvideo.white)

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
  method getDims = (w, h)

  method crop nw nh =
    let nsrc = Sdlvideo.create_RGB_surface [`HWSURFACE] ~w:nw ~h:nh ~bpp:16
      ~rmask:Int32.zero ~gmask:Int32.zero ~bmask:Int32.zero ~amask:Int32.zero in
    Sdlvideo.fill_rect nsrc (Sdlvideo.map_RGB nsrc Sdlvideo.white);
    let center = Sdlvideo.({
      r_x = (nw-w)/2;
      r_y = (nh-h)/2;
      r_w = 0;
      r_h = 0;
    }) in
    Sdlvideo.blit_surface ~src:src ~dst_rect:center ~dst:nsrc ();
    Sdlvideo.flip src;
    self#setSrc nsrc

  method load filename =
    src <- Sdlloader.load_image filename;
    self#updateDims

  method display (dst:image) =
    self#render dst#getSrc

  method render (dst:Sdlvideo.surface) =
    Sdlvideo.blit_surface src dst ();
    Sdlvideo.flip dst;

  method resize height =
    let ratio = (Utils.foi w) /. (Utils.foi h) in
    let (newW, newH) = (Utils.iof (height *. ratio), Utils.iof (height)) in
    let (ratioW, ratioH) = ((Utils.foi w) /. (Utils.foi newW), (Utils.foi h) /. (Utils.foi newH)) in
    let newSrc = Sdlvideo.create_RGB_surface [`HWSURFACE] ~w:newW ~h:newH ~bpp:16
      ~rmask:Int32.zero ~gmask:Int32.zero ~bmask:Int32.zero ~amask:Int32.zero in

    for y = 0 to newH do
      for x = 0 to newW do
        try
          let pixel = self#getPixel (Utils.iof ((Utils.foi x) *. ratioW)) (Utils.iof ((Utils.foi y) *. ratioH)) in
          Sdlvideo.put_pixel_color newSrc x y pixel
        with
        | _ -> ()
      done;
    done;
    self#setSrc newSrc

  method iter (f:int -> int -> (int * int * int) -> unit) =
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
        f i j (self#getPixel i j)
      done;
    done;
end
