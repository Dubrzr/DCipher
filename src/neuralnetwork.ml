type trainingSet = {
  input: float array;
  target: float array;
}

let activation x = if x > 0. then 1. else 0.

class neuralNetwork nInput nOutput =
object (self)

  val _learningRate = 0.1

  val _inputs = Array.make nInput 0.
  val _outputs = Array.make nOutput 0.
  val _weights = new Matrix.matrix nInput nOutput 0.

  initializer
    Random.self_init ();
    _weights#map (fun _ _ _ -> Random.float 1. -. 0.5)

  method save = _weights#saveToFile
  method load = _weights#loadFromFile

  method feedInput inputs =
    for i = 0 to nInput - 1 do
      _inputs.(i) <- inputs.(i)
    done; self#feedFroward;

  method feedFroward =
    for j = 0 to nOutput - 1 do
      let sum = ref 0. in
      for i = 0 to nInput - 1 do
        sum := !sum +. _weights#at i j *. _inputs.(i)
      done;
      _outputs.(j) <- activation !sum
    done;

  method getOutput i =
    _outputs.(i)

  method getMaxOutput =
    let max = ref neg_infinity in
    let max_i = ref 0 in

    for i = 0 to nOutput - 1 do
      if _outputs.(i) > !max then
        begin
          max := _outputs.(i);
          max_i := i;
        end;
    done; !max_i

  method train =
    Array.iter self#learn

  method learn ts =
    self#feedInput ts.input;
    self#backpropagate ts.target

  method backpropagate target =
    _weights#map (fun i j v -> v +.
      _learningRate *. (target.(j) -. _outputs.(j)) *. _inputs.(i))

end
