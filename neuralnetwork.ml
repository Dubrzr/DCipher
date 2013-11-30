type dataEntry = {
  pattern: float array;
  target: float array
}

let activationFunction x = 1. /. (1. +. exp (-. x))
let getRoundedOutputValue x = if x < 0.1 then 0 else if x > 0.9 then 1 else -1
let squaref x = x *. x

class neuralNetword nin nhid nout =
object (self:'self)

  val inputNeurons = Array.make (nin + 1) 0.
  val hiddenNeurons = Array.make (nhid + 1) 0.
  val outputNeurons = Array.make nout 0.

  val wInputHidden = new Matrix.matrix (nin + 1) nhid 0.
  val wHiddenOutput = new Matrix.matrix (nhid + 1) nout 0.

  val deltaInputHidden = new Matrix.matrix (nin + 1) nhid 0.
  val deltaHiddenOutput = new Matrix.matrix (nhid + 1) nout 0.

  val hiddenErrorGradient = Array.make (nhid + 1) 0.
  val outputErrorGradient = Array.make (nout + 1) 0.

  val learningRate = 0.001
  val momentum = 0.9
  val desiredAccuracy = 90

  val mutable trainingSetAccuracy = 0.
  val validationSetAccuracy = 0.
  val generalizationSetAccuracy = 0.

  val mutable trainingSetMSE = 0.
  val validationSetMSE = 0.
  val generalizationSetMSE = 0.

  val useBatch = false

  initializer
    inputNeurons.(nin) <- -1.;
    hiddenNeurons.(nhid) <- -1.;

    wInputHidden#map (fun _ _ -> Random.float 1. -. 0.5);
    wHiddenOutput#map (fun _ _ -> Random.float 1. -. 0.5);

  method feedInput inputs =
    self#feedForward inputs;
    outputNeurons

  method feedForward inputs =
    for i = 0 to nin do
      inputNeurons.(i) <- inputs.(i);
    done;

    for j = 0 to nhid do
      hiddenNeurons.(j) <- 0.;
      for i = 0 to nin do
        hiddenNeurons.(j) <-
          hiddenNeurons.(j) +. inputNeurons.(i) *. wInputHidden#at i j
      done;
      hiddenNeurons.(j) <- activationFunction hiddenNeurons.(j)
    done;

    for k = 0 to nout do
      outputNeurons.(k) <- 0.;
      for j = 0 to nhid do
        outputNeurons.(k) <-
          outputNeurons.(k) +. hiddenNeurons.(j) *. wHiddenOutput#at j k
      done;
      outputNeurons.(k) <- activationFunction(outputNeurons.(k))
    done;

  method train trainingSet =
    let incorrectPatterns = ref 0 and
        mse = ref 0 in

    for i = 0 to Array.length trainingSet do
      self#feedForward trainingSet.(i).pattern;
      self#backpropagate trainingSet.(i).target;

      let patternCorrect = ref true in

      for k = 0 to nout do
        if getRoundedOutputValue outputNeurons.(k) !=
          int_of_float trainingSet.(i).target.(k) then
          begin
            patternCorrect := false;
            mse := !mse + int_of_float
              (squaref (outputNeurons.(k) -. trainingSet.(i).target.(k)))
          end
      done;

      if not !patternCorrect then
        incr incorrectPatterns;
    done;

    if useBatch then
      self#updateWeights;

    trainingSetAccuracy <-
      100. -. (float !incorrectPatterns /.
                 (float (Array.length trainingSet) *. 100.));
    trainingSetMSE <- (float !mse) /. float (nout * Array.length trainingSet);

  method backpropagate targets =
    for k = 0 to nout do
      outputErrorGradient.(k) <-
        self#getOutputErrorGradient targets.(k) outputNeurons.(k);

        for j = 0 to nhid do
          if useBatch then
            deltaHiddenOutput#set j k
              (learningRate *. hiddenNeurons.(j) *. outputErrorGradient.(k) +.
                 momentum *. deltaInputHidden#at j k)
          else
            deltaHiddenOutput#set j k
              (deltaHiddenOutput#at j k +.
                 learningRate *. hiddenNeurons.(j) *. outputErrorGradient.(k))
        done;
    done;

    for j = 0 to nhid do
      hiddenErrorGradient.(j) <- self#getHiddenErrorGradient j;

        for i = 0 to nin do
          if useBatch then
            deltaInputHidden#set i j
              (learningRate *. inputNeurons.(i) *. hiddenErrorGradient.(j) +.
              momentum *. deltaHiddenOutput#at i j)
          else
            deltaInputHidden#set i j
              (learningRate *. inputNeurons.(i) *. hiddenErrorGradient.(j))
        done;
    done;

    if useBatch then
      self#updateWeights;

  method updateWeights =
    wInputHidden#iter (fun i j v ->
      wInputHidden#set i j (v +. (deltaInputHidden#at i j));
      if useBatch then deltaInputHidden#set i j 0.);

    wHiddenOutput#iter (fun k j v ->
      wHiddenOutput#set j k (v +. (deltaHiddenOutput#at j k));
      if useBatch then deltaHiddenOutput#set j k 0.);

  method getOutputErrorGradient target outputValue =
    outputValue *. (1. -. outputValue) *. (target -. outputValue)

  method getHiddenErrorGradient j =
    let wsum = ref 0. in
    for k = 0 to nout do
      wsum := !wsum +. wHiddenOutput#at j k *. outputErrorGradient.(k)
    done;
    hiddenNeurons.(j) *. (1. -. hiddenNeurons.(j) *. !wsum)

end
