class gui imgname =
object (self)

  val img_w = 480
  val img_h = 750
  val mutable img = new Image.image 0 0

  val charset = "abcdefghijklmnopqrstufwxyz" ^
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ^
    "0123456789"

  val mutable lines = Obj.magic None
  val mutable chars = Obj.magic None

  val mutable win = Obj.magic None
  val mutable mainBox = Obj.magic None

  val mutable menuBar = Obj.magic None
  val mutable menu1 = Obj.magic None
  val mutable menu1item = Obj.magic None
  val mutable menu1item1 = Obj.magic None
  val mutable menu1item2 = Obj.magic None
  val mutable menu2 = Obj.magic None
  val mutable menu2item = Obj.magic None
  val mutable menu2item1 = Obj.magic None
  val mutable menu2item2 = Obj.magic None

  val mutable content = Obj.magic None
  val mutable mainContent = Obj.magic None
  val mutable imagePixbuf = Obj.magic None
  val mutable image = Obj.magic None
  val mutable text = Obj.magic None

  val mutable buttonBar = Obj.magic None
  val mutable btn1 = Obj.magic None
  val mutable btn2 = Obj.magic None
  val mutable btn3 = Obj.magic None
  val mutable btn4 = Obj.magic None
  val mutable btn5 = Obj.magic None
  val mutable btn6 = Obj.magic None
  val mutable btn7 = Obj.magic None

  method greyscale () =
    img <- Preproc.greyscale img;
    self#reloadImg ();

  method binarize () =
    img <- Preproc.binarize img;
    self#reloadImg ();

  method rotate () =
    img <- Preproc.rotate img;
    self#reloadImg ();

  method loadImg () =
    img#load imgname;
    self#reloadImg ();

  method linesDetection () =
    lines <- Preproc.linesDetection img;

  method charDetection () =
    chars <- Preproc.segm img;

  method runFukinOcr () =
    let ocr = new Ocr.ocr charset in
    ocr#load "ann";
    text#set_buffer (GText.buffer ~text:(ocr#run chars) ());

  method reloadImg () =
    img#saveBmp "temp.bmp";
    imagePixbuf <- GdkPixbuf.from_file_at_size "temp.bmp"
      ~width:img_w ~height:img_h;
    image#set_pixbuf imagePixbuf;

  method start =
    win <- GWindow.window ~width:1000 ~height:600 ~title:"dcipher" ();
    self#createGUI;
    ignore (win#connect#destroy ~callback:(fun () -> GMain.Main.quit ()));
    ignore (win#event#connect#delete ~callback:(fun e -> false));
    win#show ();
    ignore (Sys.command "i3-msg floating enable");
    GMain.Main.main ();

  method createGUI =
    mainBox <- GPack.vbox ~packing:win#add ();
    self#createMenus;
    self#createContent;

  method createMenus =
    menuBar <- GMenu.menu_bar ~packing:mainBox#pack ();

    menu1 <- GMenu.menu ();
    menu1item1 <- GMenu.menu_item ~label:"abc" ~packing:menu1#append ();
    ignore (menu1item1#connect#activate ~callback:(fun () -> ()));
    menu1item2 <- GMenu.menu_item ~label:"def" ~packing:menu1#append ();
    ignore (menu1item2#connect#activate ~callback:(fun () -> ()));
    menu1item <- GMenu.menu_item ~label:"File" ();
    menu1item#set_submenu menu1;
    menuBar#append menu1item;

    menu2 <- GMenu.menu ();
    menu2item1 <- GMenu.menu_item ~label:"ghi" ~packing:menu2#append ();
    ignore (menu2item1#connect#activate ~callback:(fun () -> ()));
    menu2item2 <- GMenu.menu_item ~label:"jkl" ~packing:menu2#append ();
    ignore (menu2item2#connect#activate ~callback:(fun () -> ()));
    menu2item <- GMenu.menu_item ~label:"Edit" ();
    menu2item#set_submenu menu2;
    menuBar#append menu2item;

  method createContent =
    mainContent <- GPack.hbox ~packing:(mainBox#pack ~expand:true) ();
    imagePixbuf <- GdkPixbuf.create img_w img_h ();
    image <- GMisc.image ~pixbuf:imagePixbuf ~packing:mainContent#pack ();
    content <- GPack.vbox ~packing:(mainContent#pack ~expand:true) ();
    self#createButtonBar;
    text <- GText.view ~packing:(content#pack ~expand:true) ();

  method createButtonBar =
    buttonBar <- GPack.hbox ~packing:content#pack ();
    btn1 <- GButton.button ~label:"Load image" ~packing:buttonBar#pack ();
    ignore (btn1#connect#clicked ~callback:self#loadImg);
    btn2 <- GButton.button ~label:"Grey scale" ~packing:buttonBar#pack ();
    ignore (btn2#connect#clicked ~callback:(self#greyscale));
    btn3 <- GButton.button ~label:"Binarize" ~packing:buttonBar#pack ();
    ignore (btn3#connect#clicked ~callback:(self#binarize));
    btn4 <- GButton.button ~label:"Rotate" ~packing:buttonBar#pack ();
    ignore (btn4#connect#clicked ~callback:(self#rotate));
    btn5 <- GButton.button ~label:"Detect lines" ~packing:buttonBar#pack ();
    ignore (btn5#connect#clicked ~callback:(self#linesDetection));
    btn6 <- GButton.button ~label:"Detect chars" ~packing:buttonBar#pack ();
    ignore (btn6#connect#clicked ~callback:(self#charDetection));
    btn7 <- GButton.button ~label:"Run ocr" ~packing:buttonBar#pack ();
    ignore (btn7#connect#clicked ~callback:(self#runFukinOcr));

end
