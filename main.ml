(* Project: DCipher
 * File: main.ml
 * Description: program entry point
 * Date: 09-10-2013
 * Author: CodeBreaker
 *)

let usage = "Usage: dcipher [--train] img_file"

let parse_args argv =
  if Array.length argv <= 1 || Array.length argv = 2 && argv.(1) = "--train" then
    begin
      Printf.printf "%s\n" usage;
      exit 1;
    end;
  let train = (argv.(1) = "--train") in
  let file = if (train) then argv.(2) else argv.(1) in
  (train, file)

let main () =
  begin
    let (train, file) = parse_args Sys.argv in
    Ocr.read_text file train ();
    exit 0;
  end

let _ = main ()
