let bsnumb = ref 0

(* Module for file opening and saving text*)
module Aux =
    struct
        let load (text : GText.view) file =
        let ich = open_in file in
        let len = in_channel_length ich in
        let buf = Buffer.create len in
            Buffer.add_channel buf ich len;
            close_in ich;
            text#buffer#set_text (Buffer.contents buf)
		let save (text : GText.view) file =
			let och = open_out file in
			output_string och (text#buffer#get_text ());
			close_out och
		

            let loadimage (name:bool) (image : GMisc.image) file =
            Sdlvideo.save_BMP (Sdlloader.load_image file) "thumbs/temp.bmp";
            let buf = GdkPixbuf.from_file_at_size file ~width:580 ~height:700 in
                image#set_pixbuf buf;
            print_endline file;
            if name then
            let aux file =
            match file with
                |f when f.[(String.length f)-5]='1'||f.[(String.length f)-9]='1'->begin bsnumb := 1 end
                |f when f.[(String.length f)-5]='2'||f.[(String.length f)-9]='2'->begin bsnumb := 2 end
                |f when f.[(String.length f)-5]='3'||f.[(String.length f)-9]='3'->begin bsnumb := 3 end 
                |f                                                      ->begin bsnumb := 0 end
            in
            aux file;
            print_endline (string_of_int(!bsnumb))

    end

(* Init of GTK *)
let _ = GMain.init ()

(* Main window *)
let window =
    GWindow.window
        ~width:1400
        ~height:700
        ~resizable:false
        ~title:"DCipher" ()

(* Main box for the menu and the main box *)
let boxMenu =
    GPack.vbox
     	~packing:window#add ()

(* Menu bar *)
let barMenu =
        let menubar =
                GMenu.menu_bar
                    ~packing:boxMenu#pack ()
        in
           menubar

(* Main box *)
let boxMain =
        GPack.hbox
                ~packing:boxMenu#add ()




(* Selection File dialog *)
let action stock event action =
        let dialog =
                GWindow.file_chooser_dialog
                        ~action:`OPEN
                        ~parent:window
                        ~destroy_with_parent:true ()
        in
                dialog#add_button_stock `CANCEL `CANCEL;
                dialog#add_select_button_stock stock event;
        let fun_action () =
                if dialog#run () = `OPEN then
                        Gaux.may action dialog#filename;
                        dialog#misc#hide ()
        in
                fun_action ()


(* Image box and image displayer *)
let boxImage =
        GPack.hbox
                ~spacing:5
        		~width:1
                ~border_width:5
                ~homogeneous:true
                ~packing:boxMain#add ()

let image =
    let buf = GdkPixbuf.create ~width:520 ~height:768 () in
        GMisc.image
        ~pixbuf:buf
                ~packing:boxImage#add ()

(* Text Box and text buffer*)
let boxText =
        GPack.hbox
                ~spacing:5
                ~border_width:5
                ~packing:boxMain#add()

let tagTab = GText.tag_table ()
let buffer = GText.buffer
    ~tag_table:tagTab
    ~text:"Le texte sera extrait ici" ()

let text =
        let scroll =
                GBin.scrolled_window
                        ~hpolicy:`ALWAYS
                        ~vpolicy:`ALWAYS
                        ~shadow_type:`ETCHED_IN
                        ~packing:boxText#add ()
        in
    let basetext = "" in
    buffer#set_text(basetext);
        let textaux =
                GText.view
            ~buffer:buffer
                        ~packing:scroll#add ()
        in
        GtkSpell.attach ~lang:"fr_gut" textaux;
                textaux#misc#modify_font_by_name "Arial 10";
                textaux

let bbox = GPack.button_box `HORIZONTAL
	
	~border_width:5
	~packing:(boxMenu#pack ~expand:false) ()


(* Function to open an image and save a text *)
let actionOpen () =
        action `OPEN `OPEN (Aux.loadimage true image)

let actionSave () =
        action `SAVE `SAVE (Aux.save text)

(* Copyright dialog *)
let about () =
    let dialog =
                GWindow.about_dialog
                ~authors:["Code Breaker"]
                ~copyright:"Copyright © 2013 Code Breaker"
                ~license:"GNU General Public License v3.0"
                ~version:"1.0"
                ~website:"dcipher.42portal.com"
                ~website_label:"DCipher"
                ~position:`CENTER_ON_PARENT
                ~parent:window
                ~destroy_with_parent:true ()
    in
         let fun_about () =
                ignore (dialog#run ());
                dialog#misc#hide ();
        in
                fun_about ()


(*===========Image Processing===========*)
let processing funct imagepath =
    funct imagepath;
    Aux.loadimage false image ("thumbs/temp.bmp")

let processonly funct imagepath =
    funct imagepath


(*let greyscale () =
    processing (Main.greyscale) ("thumbs/temp.bmp")

let binarize () =
    processing (Main.binarize) ("thumbs/temp.bmp")

let rotate () =
    processing (Main.rotate) ("thumbs/temp.bmp")

let median () =
    processing (Main.median) ("thumbs/temp.bmp")

let convolution () =
    processing (Main.convolution) ("thumbs/temp.bmp")

let segmentation () =
    processing (Main.segmentation) ("thumbs/temp.bmp")

let skeleton () =
    processing (Main.hilditch) ("thumbs/temp.bmp")

let detection () =
    processing (Main.detection) ("thumbs/temp.bmp")

let fulldisplay () =
    processing (Main.fulldisplay) ("thumbs/temp.bmp")

let applyall () =
    processonly (Main.convolution) ("thumbs/temp.bmp");
    processonly (Main.binarize) ("thumbs/temp.bmp");
    processing (Main.rotate) ("thumbs/temp.bmp");
    processonly (Main.segmentation) ("thumbs/temp.bmp");
    processonly (Main.hilditch) ("thumbs/temp.bmp");
    processing (Main.detection) ("thumbs/temp.bmp")

let extraction () =
    let s = Main.extraction !bsnumb in
        Aux.load text s*)

let checkspell () =
    GtkSpell.recheck_all text

(*let xor_approx () =
    Nn.xor_sigmoid_test ();
    print_newline ()*)

(*=============Buttons===========*)
let greyButton = 
  let button = GButton.button
  	~label:"Niveau de gris"
    ~packing:bbox#add () in
  button#connect#clicked ~callback:(fun()->());
  button

let binButton = 
  let button = GButton.button 
    ~label:"Binarisation" 
    ~packing:bbox#add () in
  button#connect#clicked ~callback:(fun()->());
  button

let rotButton = 
  let button = GButton.button 
    ~label:"Rotation" 
    ~packing:bbox#add () in
  button#connect#clicked ~callback:(fun()->());
  button

let detectlineButton = 
  let button = GButton.button 
    ~label:"Détection de ligne" 
    ~packing:bbox#add () in
  button#connect#clicked ~callback:(fun()->());
  button

let detectcharButton = 
  let button = GButton.button 
    ~label:"Détection de char" 
    ~packing:bbox#add () in
  button#connect#clicked ~callback:(fun()->());
  button

let applyallButton = 
  let button = GButton.button 
    ~label:"Tout appliquer"
    ~packing:bbox#add () in
  button#connect#clicked ~callback:(fun()->());
  button

let spellButton = 
  let button = GButton.button 
    ~label:"Correcteur orthographique"
    ~packing:bbox#add () in
  button#connect#clicked ~callback:checkspell;
  button
  

(*============Submenus===========*)
let tools =
	let toolbar = new GMenu.factory barMenu in
		let file_menu = toolbar#add_submenu "Fichier" in
               let file = new GMenu.factory file_menu in
                   begin
                       ignore(file#add_item "Ouvrir" ~callback:actionOpen);
                       ignore(file#add_item "Exporter texte" ~callback:actionSave);
                   end;
		let helpMenu = toolbar#add_submenu "Aide" in
                let help = new GMenu.factory helpMenu in
                        begin
                            ignore(help#add_item "A propos" ~callback:about);
                            ignore(help#add_item "Aide" ~callback:(fun () ->
                    print_endline "Cette fonction n'est pas encore implémentée."));
                        end;
                toolbar


let _ =
	ignore(window#connect#destroy ~callback:GMain.quit);
	window#show ();        
	GMain.main ()