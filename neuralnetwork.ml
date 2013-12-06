let activationFunction x = 1. /. (1. +. exp (-. x))

type ('a, 'b) dataConverter = {
  convertInput: 'a -> int -> float;
  convertTarget: 'b -> float;
}

type ('a, 'b) dataEntry = {
  pattern: 'a;
  target: 'b array;
}

class ['a, 'b] neuralNetwork nInput nHidden nOutput =
object (self)

  constraint 'a = int * int

  val mutable learningRate = 0.001
  val mutable momentum = 0.9

  val inputNeurons = Array.make (nInput + 1) 0.
  val hiddenNeurons = Array.make (nHidden + 1) 0.
  val outputNeurons = Array.make (nOutput + 1) 0.

  val wInputHidden = new Matrix.floatMatrix (nInput + 1) nHidden 0.
  val wHiddenOutput = new Matrix.floatMatrix (nHidden + 1) nOutput 0.

  val deltaInputHidden = new Matrix.floatMatrix (nInput + 1) nHidden 0.
  val deltaHiddenOutput = new Matrix.floatMatrix (nHidden + 1) nOutput 0.

  val mutable useBatch = false

  val outErrGrad = Array.make nOutput 0.
  val hidErrGrad = Array.make nHidden 0.


  initializer
    inputNeurons.(nInput) <- -1.;
    hiddenNeurons.(nHidden) <- -1.;
    self#initializeWeights

  method useBatchLearning =
    useBatch <- true;

  method useStochasticLearning =
    useBatch <- false;

  method setLearningParam lr m =
    learningRate <- lr;
    momentum <- m;

  method feedInput
    (input: ('a, 'b) dataEntry)
    (converter: ('a, 'b) dataConverter)
    : float array =
    self#feedForward input.pattern converter;
    outputNeurons

  method initializeWeights : unit =
    Random.self_init ();

    wInputHidden#map (fun i j _ -> Random.float 2. -. 1.);
    deltaInputHidden#map (fun i j _ -> 0.);

    wHiddenOutput#map (fun i j _ -> Random.float 2. -. 1.);
    deltaHiddenOutput#map (fun i j _ -> 0.);

  method updateWeights =
    wInputHidden#map (fun i j v ->
      if useBatch then deltaInputHidden#set i j 0.;
      v -. deltaInputHidden#at i j);
    wHiddenOutput#map (fun j k v ->
      if useBatch then deltaHiddenOutput#set j k 0.;
      v -. deltaHiddenOutput#at j k);

  method train trainingSet converter =
    for i = 0 to Array.length trainingSet - 1 do
      self#feedForward trainingSet.(i).pattern converter;
      self#backpropagate trainingSet.(i).target converter;
    done;
    if useBatch then self#updateWeights;


  method feedForward input converter =
    Array.iteri
      (fun i _ -> inputNeurons.(i) <- converter.convertInput input i)
      inputNeurons;

    for j = 0 to nHidden - 1 do
      hiddenNeurons.(j) <- 0.;
      for i = 0 to nInput do
        hiddenNeurons.(j) <- hiddenNeurons.(j) +.
          inputNeurons.(i) *. wInputHidden#at i j;
      done;
      hiddenNeurons.(j) <- activationFunction hiddenNeurons.(j);
    done;

    for k = 0 to nOutput - 1 do
      outputNeurons.(k) <- 0.;
      for j = 0 to nHidden do
        outputNeurons.(k) <- outputNeurons.(k) +.
          hiddenNeurons.(j) *. wHiddenOutput#at j k;
      done;
      outputNeurons.(k) <- activationFunction outputNeurons.(k);
    done;

  method backpropagate desiredValue converter =

    if Array.length desiredValue != nOutput then
      raise (Invalid_argument "desiredValues");

    for k = 0 to nOutput - 1 do
      outErrGrad.(k) <- self#computeOutErrorGradient
        (converter.convertTarget desiredValue.(k)) outputNeurons.(k);

      for j = 0 to nHidden do
        deltaHiddenOutput#set j k
          (learningRate *. hiddenNeurons.(j) *. outErrGrad.(k) +.
             (if useBatch then 1. else momentum) *. deltaHiddenOutput#at j k);
      done;
    done;

    for j = 0 to nHidden - 1 do
      hidErrGrad.(j) <- self#computeHidErrorGradient j;

      for i = 0 to nInput do
        deltaInputHidden#set i j
          (learningRate *. inputNeurons.(i) *. hidErrGrad.(j) +.
             (if useBatch then 1. else momentum) *. deltaInputHidden#at i j);
      done;
    done;

    if not useBatch then self#updateWeights;

  method computeOutErrorGradient desiredValue outputValue =
    outputValue *. (1. -. outputValue) *. (desiredValue -. outputValue)

  method computeHidErrorGradient j =
    let a = ref 0. in
    for k = 0 to nOutput - 1 do
      a := !a +. (wHiddenOutput#at j k *. outErrGrad.(k));
    done;
    hiddenNeurons.(j) -. (1. -. hiddenNeurons.(j)) *. !a;

  method print =
    Printf.printf "inputNeurons: ";
    Array.iter (Printf.printf "%1.3f ") inputNeurons;
    Printf.printf "\nhiddenNeurons: ";
    Array.iter (Printf.printf "%1.3f ") hiddenNeurons;
    Printf.printf "\noutputNeurons: ";
    Array.iter (Printf.printf "%1.3f ") outputNeurons;
    Printf.printf "\n\nwInputHidden: \n";
    wInputHidden#print (Printf.printf "%1.3f ");
    Printf.printf "\nwHiddenOutput: \n";
    wHiddenOutput#print (Printf.printf "%1.3f ");
    Printf.printf "\ndeltaInputHidden: \n";
    deltaInputHidden#print (Printf.printf "%1.3f ");
    Printf.printf "\ndeltaHiddenOutput: \n";
    deltaHiddenOutput#print (Printf.printf "%1.3f ");

    let a = ref 0. in
    for i = 0 to nHidden - 1 do
      a := !a +. hidErrGrad.(i);
    done;
    Printf.printf "\nhidErrGrad average: %f\n" (!a /. float nHidden);
    a := 0.;
    for i = 0 to nOutput - 1 do
      a := !a +. outErrGrad.(i);
    done;
    Printf.printf "outErrGrad average: %f\n" (!a /. float nOutput);

end
