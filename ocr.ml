class ocr =
object (self: 'self)

  val n = 10
  val m = 10

  method run (input:bool Matrix.boolMatrix) =
    let mat = new Matrix.boolMatrix n m false in

    if Sys.file_exists "data" then
      mat#loadFromFile "data"
    else
      mat#saveToFile "data"

end

let ocr = new ocr
