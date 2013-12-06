class ocr =
object (self: 'self)

  val n = 16
  val m = 16

  val charset_from = 33
  val charset_to = 126

  val ann = new Neuralnetwork.neuralNetwork 2 3 1
  val converter = Neuralnetwork.({
    convertInput = (fun (a, b) i -> if i = 0 then float a else float b);
    convertTarget = float;
  })

  initializer
    ann#useStochasticLearning;
    ann#setLearningParam 0.1 0.9;

  method close = ()

  method run input =
    let input = Neuralnetwork.({pattern = input; target = Array.make 1 0;}) in
    (ann#feedInput input converter).(0)

  method train times percent =
    let trainingSet = Array.of_list Neuralnetwork.([
      {pattern = (0, 0); target = Array.make 1 0;};
      {pattern = (0, 1); target = Array.make 1 1;};
      {pattern = (1, 0); target = Array.make 1 1;};
      {pattern = (1, 1); target = Array.make 1 0;};
    ]) in
    for i = 0 to times do
      if i mod (times / percent) = 0 then
        begin
          Printf.printf "\n\r====== %d%% ======\n\n" (100*i/times);
          ann#print;
          Printf.printf "\n\n\n\n\n\n%!";
        end;
      ignore (ann#train trainingSet converter);
    done;
    Printf.printf "\n\n";

  (*
    method train (fontname: string) =
    let font = Sdlttf.open_font fontname 12 in
    let img = new Image.image 0 0 in
    for i = 0 to charset_to - charset_from do
      let c = Char.chr (i + charset_from) in
      img#setSrc
        (Sdlttf.render_glyph_solid font c Sdlvideo.black);
      img#crop n m;
      let trainingSet = Neuralnetwork.({
        pattern = Convert.matrixBool_of_image img 0.5;
        target = Char.chr (i + charset_from);
      }) in
      ignore (Array.iter ann#feedInput trainingSet);
    done;
  *)

end

let ocr = new ocr
let train = ocr#train
let run = ocr#run
