exception MatrixOutOfBounds of int * int

class ['a] matrix n m (initValue:'a) =
object (self:'self)

  val mat = Array.init n (fun _ -> Array.make m initValue)

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

  method map f =
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        self#set i j (f i j mat.(i).(j))
      done;
    done;

  method saveToFile fname =
    let f = open_out_bin fname in
    self#iter (fun _ _ v -> output_value f v);
    close_out f

  method loadFromFile fname =
    let f = open_in_bin fname in
    self#map (fun _ _ v -> input_value f);
    close_in f

  method copyTo (mat:'self) =
    self#iter mat#set

  method copyFrom (mat:'self) =
    mat#iter self#set
end
