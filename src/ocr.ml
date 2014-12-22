class ocr charset =
object (self: 'self)

  val n = 16
  val m = 16

  val font = ref (Obj.magic None)
  val img = new Image.image 0 0

  val mutable ann = Obj.magic None

  initializer
    ann <- new Neuralnetwork.neuralNetwork (n*m) (String.length charset);

  method run input =
    let str = ref "" in
    Array.iter (fun x -> str := !str ^ (self#runOneChar x)) input;
    !str

  method load = ann#load
  method save = ann#save

  method runOneChar (mat:bool Matrix.matrix) =
    let mat = Serialization.serialize mat n m in
    ann#feedInput (self#createInput mat);
    Char.escaped (charset.[ann#getMaxOutput])

  method createInput mat =
    let mat = Serialization.serialize mat n m in
    let input = Array.make (mat#getWidth * mat#getHeight) 0. in
    for i = 0 to Array.length input - 1 do
      input.(i) <- Convert.float_of_bool
        (mat#at (i / mat#getHeight) (i mod mat#getHeight))
    done; input

  method createTrainingSet i =
    img#setSrc
      (Sdlttf.render_glyph_solid !font charset.[i] ~fg:Sdlvideo.black);
    img#crop n m;
    let mat = Convert.matrixBool_of_image img 0.5 in
    let mat = Serialization.serialize mat n m in
    let target = Array.make (String.length charset) 0. in
    target.(i) <- 1.;
    Neuralnetwork.({
      input = self#createInput mat;
      target = target;
    })

  method train times fontname =
    font := Sdlttf.open_font fontname 12;
    let trainingSet = Array.init (String.length charset)
      self#createTrainingSet in
    for i = 0 to times do
      ann#train trainingSet;
    done;
end
