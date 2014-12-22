exception MatrixOutOfBounds of int * int

class ['a] matrix n m (initValue:'a) =
object (self:'self)

  val mutable mat = Array.init n (fun _ -> Array.make m initValue)

  method getWidth = n
  method getHeight = m
  method getDims = (n, m)

  method isInBounds x y = x >= 0 && y >= 0 && x < n && y < m
  method at x y =
    if not (self#isInBounds x y) then raise (MatrixOutOfBounds (x, y))
    else mat.(x).(y)

  method set x y value =
    if not (self#isInBounds x y) then raise (MatrixOutOfBounds (x, y))
    else mat.(x).(y) <- value

  method iter (f:int -> int -> 'a -> unit) =
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        f i j mat.(i).(j)
      done;
    done;

  method map (f:int -> int -> 'a -> 'a) =
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        mat.(i).(j) <- f i j mat.(i).(j)
      done;
    done;

  method saveToFile fname =
    let f = open_out_bin fname in
    output_value f mat;
    close_out f

  method loadFromFile fname =
    let f = open_in_bin fname in
    mat <- input_value f;
    close_in f;

  method copyTo (mat:'self) =
    self#iter mat#set

  method copyFrom (mat:'self) =
    mat#iter self#set

  method print (f: 'a -> unit) =
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        f mat.(j).(i)
      done;
      Printf.printf "\n";
    done;
end

class ['a] floatMatrix n m (initValue:'a) =
object (self:'self)
  inherit ['a] matrix n m initValue
  constraint 'a = float
end

class ['a] intMatrix n m (initValue:'a) =
object (self:'self)
  inherit ['a] matrix n m initValue
  constraint 'a = int
end

class ['a] int3Matrix n m (initValue:'a) =
object (self:'self)
  inherit ['a] matrix n m initValue
  constraint 'a = int * int * int
end

class ['a] boolMatrix n m (initValue:'a) =
object (self:'self)
  inherit ['a] matrix n m initValue
  constraint 'a = bool
end



let resize mat newW newH =
  let (n, m) = mat#getDims in
  let temp = new matrix newW newH false in
  let arrayPixels = Array.make (n * m) false in
  for j = 0 to m - 1 do
    for i = 0 to n - 1 do
      let pixelPos = j * n + i in
      arrayPixels.(pixelPos) <- mat#at i j;
    done
  done;

  let a = ref 0. and
      b = ref 0. and
      c = ref 0. and
      d = ref 0. and
      x = ref 0 and
      y = ref 0 and
      index = ref 0 and
      grey = ref 0 and
      x_ratio = (Utils.foi (n - 1)) /. (Utils.foi newW) and
      y_ratio = (Utils.foi (m - 1)) /. (Utils.foi newH) and
      x_diff = ref 0. and
      y_diff = ref 0. in
  for i = 0 to newH - 1 do
    for j = 0 to newW - 1 do
      x := Utils.iof (x_ratio *. (Utils.foi j));
      y := Utils.iof (y_ratio *. (Utils.foi i));
      x_diff := (x_ratio *. (Utils.foi j)) -. (Utils.foi !x);
      y_diff := (y_ratio *. (Utils.foi i)) -. (Utils.foi !y);
      index := !y * n + !x;

      a := if arrayPixels.(!index) then 0. else 255.;
      b := if arrayPixels.(!index+1) then 0. else 255.;
      c := if arrayPixels.(!index+n) then 0. else 255.;
      d := if arrayPixels.(!index+n+1) then 0. else 255.;

      grey := Utils.iof (
              !a *. (1. -. !x_diff) *. (1. -. !y_diff) +.
              !b *. !x_diff *. (1. -. !y_diff) +.
              !c *. !y_diff *. (1. -. !x_diff) +.
              !d *. !x_diff *. !y_diff
            );
      let color =
        if !grey < 254 then true else false in
        temp#set !x !y color;
    done
  done;
  temp
