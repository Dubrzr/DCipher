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

  method getWidth = w
  method getHeight = h

  method load filename =
    src <- Sdlloader.load_image filename;
    self#updateDims

  method display (dst:image) =
    self#render dst#getSrc

  method render (dst:Sdlvideo.surface) =
    Sdlvideo.blit_surface src dst ()

end
