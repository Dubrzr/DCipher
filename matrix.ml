(* Project: DCipher
 * File: matrix.ml
 * Description: matrix operations
 * Date: 13-10-2013
 * Author: CodeBreaker
 *)

class matrix _n _m =
object (s)

  val n = _n
  val m = _m
  val arr = Array.init _n (fun _ -> Array.make _m 0)

  method in_bounds i j =
    i >= 0 && j >= 0 && i < n && j < m

  method at i j =
    if (not (s#in_bounds i j)) then invalid_arg "Matrix#at"
    else arr.(i).(j)

  method set i j value =
    if (not (s#in_bounds i j)) then invalid_arg "Matrix#set"
    else arr.(i).(j) <- value

  method print =
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        Printf.printf "%d " arr.(i).(j)
      done;
      Printf.printf "\n";
    done;

  method map f =
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        arr.(i).(j) <- f i j arr.(i).(j)
      done;
    done;

  method save filename =
    let oc = open_out filename in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        Printf.fprintf oc "%d " arr.(i).(j)
      done;
      Printf.fprintf oc "\n"
    done;
    close_out oc;

end
