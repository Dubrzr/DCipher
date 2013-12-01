let squaref x = x *. x
let activationFunction x = 1. /. (1. +. exp (-. x))

type dataEntry = {
  pattern: float array;
  target: float array;
}

class neuralNetwork nInput nHidden nOutput =
object (self)

  val mutable learningRate: float = 0.001
  val mutable momentum: float = 0.9

  val inputNeurons: float array = Array.make (nInput + 1) 0.
  val hiddenNeurons: float array = Array.make (nHidden + 1) 0.
  val outputNeurons: float array = Array.make (nOutput + 1) 0.

  val wInputHidden = new Matrix.matrix (nInput + 1) nHidden 0.
  val wHiddenOutput = new Matrix.matrix (nHidden + 1) nOutput 0.

  val mutable epoch: int = 0
  val mutable maxEpochs: int = 1500

  val mutable desiredAccuracy: float = 90.

  val deltaInputHidden = new Matrix.matrix (nInput + 1) nHidden 0.
  val deltaHiddenOutput = new Matrix.matrix (nHidden + 1) nOutput 0.

  val hiddenErrorGradients: float array = Array.make (nHidden + 1) 0.
  val outputErrorGradients: float array = Array.make (nOutput + 1) 0.

  val mutable trainingSetAccuracy: float = 0.
  val mutable validationSetAccuracy: float = 0.
  val mutable generalizationSetAccuracy: float = 0.
  val mutable trainingSetMSE: float = 0.
  val mutable validationSetMSE: float = 0.
  val mutable generalizationSetMSE: float = 0.

  val mutable useBatch: bool = false

  val mutable logResults: bool = false
  val mutable logFile: out_channel = stdout
  val mutable logResolution: int = 10
  val mutable lastEpochLogged: int = -10

  initializer
    inputNeurons.(nInput) <- -1.;
    hiddenNeurons.(nHidden) <- -1.;
    self#initalizeWeights

  method setLearningParameters (lr: float) (m: float) =
    learningRate <- lr;
    momentum <- m

  method setMaxEpochs (max: int) =
    maxEpochs <- max

  method setDesiredAccuracy (d: float) =
    desiredAccuracy <- d

  method useBatchLearning =
    useBatch <- true

  method useStochasticLearning =
    useBatch <- false

  (* TODO: default resolution: 1 *)
  (* TODO: implement enableLogging *)
  method enableLogging (filename: string) (resolution: int) =
    logResults <- true;
    logResolution <- resolution;
    lastEpochLogged <- -resolution;
    if filename <> "stdout" then
      logFile <- open_out filename
    else
      logFile <- stdout


  method resetWeights =
    self#initalizeWeights

  method feedInput (input: float array) =
    self#feedForward input;
    outputNeurons

  method trainNetwork
    (trainingSet: dataEntry array)
    (generalizationSet: dataEntry array)
    (validationSet: dataEntry array) =
    Printf.printf "\n Neural network Training Starting:\n";
    Printf.printf "=========================================================\n";
    Printf.printf " LR: %1.2f, Momentum: %1.2f," learningRate momentum;
    Printf.printf " Max Epochs: %d\n" maxEpochs;
    Printf.printf " %d Input Neurons, %d Hidden Neurons," nInput nHidden;
    Printf.printf " %d Output Neurons\n" nOutput;
    Printf.printf "=========================================================\n";

    epoch <- 0;
    lastEpochLogged <- -logResolution;

    while (trainingSetAccuracy < desiredAccuracy) ||
      (generalizationSetAccuracy < desiredAccuracy) &&
      epoch < maxEpochs do

      let previousTAccuracy = trainingSetAccuracy in
      let previousGAccuracy = generalizationSetAccuracy in

      self#runTrainingEpoch trainingSet;

      generalizationSetAccuracy <- self#getSetAccuracy generalizationSet;
      generalizationSetMSE <- self#getSetMSE generalizationSet;

      if logResults && epoch - lastEpochLogged == logResolution then
        begin
          let str = Printf.sprintf ("%d, %1.2f, %1.2f, %1.2f, %1.2f\n")
            epoch trainingSetAccuracy generalizationSetAccuracy
            trainingSetMSE generalizationSetMSE in
          output_string logFile str;
          lastEpochLogged <- epoch;
        end;

      if (ceil previousTAccuracy != ceil trainingSetAccuracy) ||
        (ceil previousGAccuracy != ceil generalizationSetAccuracy) then
        begin
          Printf.printf "Epoch: %d\n" epoch;
          Printf.printf " TSet Acc: %1.2f%%," trainingSetAccuracy;
          Printf.printf " MSE: %1.2f\n" trainingSetMSE;
          Printf.printf " GSet Acc: %1.2f%%," generalizationSetAccuracy;
          Printf.printf " MSE: %1.2f\n" generalizationSetMSE;
        end;

      epoch <- epoch + 1;
    done;

    validationSetAccuracy <- self#getSetAccuracy validationSet;
    validationSetMSE <- self#getSetMSE validationSet;

    Printf.printf "\nTraining Complete!!! - > Elapsed Epochs: %d\n" epoch;
    Printf.printf " Validation Set Accuracy: %1.2f\n" validationSetAccuracy;
    Printf.printf " Validation Set MSE: %1.2f\n\n" validationSetMSE;

  (* PRIVATE *)

  method initalizeWeights =
    Random.self_init ();

    for i = 0 to nInput do
      for j = 0 to nHidden do
        wInputHidden#set i j (Random.float 1. -. 0.5);
        deltaInputHidden#set i j 0.;
      done;
    done;

    for j = 0 to nHidden do
      for k = 0 to nHidden do
        wHiddenOutput#set j k (Random.float 1. -. 0.5);
        deltaHiddenOutput#set j k 0.;
      done;
    done;

  method runTrainingEpoch (trainingSet: dataEntry array) =
    let incorrectPatterns = ref 0. and
        mse = ref 0. in

    for tp = 0 to Array.length trainingSet do
      self#feedForward trainingSet.(tp).pattern;
      self#backpropagate trainingSet.(tp).target;

      let patternCorrect = ref true in

      for k = 0 to nOutput do
        if self#getRoundedOutputValue outputNeurons.(k) !=
          int_of_float trainingSet.(tp).target.(k) then
          patternCorrect := false;
        mse := !mse +.
          squaref (outputNeurons.(k) -. trainingSet.(tp).target.(k))
      done;

      if not !patternCorrect then
        incorrectPatterns := !incorrectPatterns +. 1.;
    done;

    if useBatch then
      self#updateWeights;

    trainingSetAccuracy <-
      100. -. (!incorrectPatterns /. float (Array.length trainingSet) *. 100.);
    trainingSetMSE <- !mse /. (float (nOutput * Array.length trainingSet));

  method feedForward (input: float array) =
    for i = 0 to nInput do
      inputNeurons.(i) <- input.(i);
    done;

    for j = 0 to nHidden do
      hiddenNeurons.(j) <- 0.;

      for i = 0 to nInput do
        hiddenNeurons.(j) <- hiddenNeurons.(j) +.
          inputNeurons.(i) *. wInputHidden#at i j
      done;

      hiddenNeurons.(j) <- activationFunction hiddenNeurons.(j)
    done;

    for k = 0 to nOutput do
      outputNeurons.(k) <- 0.;

        for j = 0 to nHidden do
          outputNeurons.(k) <- outputNeurons.(k) +.
            hiddenNeurons.(j) *. wHiddenOutput#at j k
        done;

      outputNeurons.(k) <- activationFunction outputNeurons.(k);

    done;

  method backpropagate (desiredValues: float array) =
    for k = 0 to nOutput do
      outputErrorGradients.(k) <-
        self#getOutputErrorGradient desiredValues.(k) outputNeurons.(k);

      for j = 0 to nHidden do
        if not useBatch then
          deltaHiddenOutput#set j k (learningRate *. hiddenNeurons.(j) *.
            outputErrorGradients.(k) +. momentum *. deltaHiddenOutput#at j k)
        else
          deltaHiddenOutput#set j k (deltaHiddenOutput#at j k +.
            learningRate *. hiddenNeurons.(j) *. outputErrorGradients.(k))
      done;
    done;

    for j = 0 to nHidden do
      hiddenErrorGradients.(j) <- self#getHiddenErrorGradient j;

      for i = 0 to nInput do
        if not useBatch then
          deltaInputHidden#set i j (learningRate *. inputNeurons.(i) *.
            hiddenErrorGradients.(j) +. momentum *. deltaInputHidden#at i j)
        else
          deltaInputHidden#set i j (deltaInputHidden#at i j +.
            learningRate *. inputNeurons.(i) *. hiddenErrorGradients.(j));
      done;
    done;

    if useBatch then
      self#updateWeights;

  method updateWeights =
    for i = 0 to nInput do
      for j = 0 to nHidden do
        wInputHidden#set i j (wInputHidden#at i j +. deltaInputHidden#at i j);
        if useBatch then
          deltaInputHidden#set i j 0.;
      done;
    done;

    for j = 0 to nHidden do
      for k = 0 to nOutput do
        wHiddenOutput#set j k (wHiddenOutput#at j k +.
          deltaHiddenOutput#at j k);
        if useBatch then
          deltaHiddenOutput#set j k 0.;
      done;
    done;

  method getOutputErrorGradient (desiredValue: float) (outputValue: float) =
    outputValue *. (1. -. outputValue) *. (desiredValue -. outputValue)

  method getHiddenErrorGradient (j: int) =
    let weightedSum = ref 0. in

    for k = 0 to nOutput do
      weightedSum := !weightedSum +.
        wHiddenOutput#at j k *. outputErrorGradients.(k);
    done; hiddenNeurons.(j) *. (1. -. hiddenNeurons.(j)) *. !weightedSum

  method getRoundedOutputValue (x: float) : int =
    if x < 0.1 then 0
    else if x > 0.9 then 1
    else -1

  method getSetAccuracy (set: dataEntry array) : float =
    let incorrectResults = ref 0. in

    for tp = 0 to Array.length set - 1 do
      self#feedForward set.(tp).pattern;

      let correctResult = ref true in
      for k = 0 to nOutput do
        if self#getRoundedOutputValue outputNeurons.(k) !=
          int_of_float set.(tp).target.(k) then correctResult := false;
      done;

      if not !correctResult then
        incorrectResults := !incorrectResults +. 1.;
    done;

    100. -. (!incorrectResults /. float (Array.length set) *. 100.)

  method getSetMSE (set: dataEntry array) : float =
    let mse = ref 0. in

    for tp = 0 to Array.length set - 1 do
        self#feedForward set.(tp).pattern;

      for k = 0 to nOutput do
        mse := !mse +. (squaref outputNeurons.(k) -. set.(tp).target.(k))
      done;
    done; !mse /. (float (nOutput * Array.length set))

end
