(* Project: DCipher
 * File: test.ml
 * Description: test de code OCaml avec SDL
 * Date: 05-10-2013
 * Author: Nils Layet
 *)

(* Pour simplifier la lecture du code, j'utilise open... *)
(* Dans la release, il faudra utilise la syntaxe pointee (module.fonction) *)

open Sdl;;
open Sdlvideo;;
open Sdlloader;;
open Sdlevent;;

(* binarise l'image selon un seuil *)
let img_binarize img thredhold =
  let thredhold = int_of_float (thredhold *. 255.) in
  for i = 0 to (surface_info img).w -1 do
    for j = 0 to (surface_info img).h -1 do
      let (r, g, b) = get_pixel_color img ~x:i ~y:j in
      let c = if (r + g + b) / 3 > thredhold then 255 else 0 in
      put_pixel img ~x:i ~y:j (map_RGB img (c, c, c));
    done
  done

(* met en pause en attendant un keydown *)
let rec wait_key() =
  let e = wait_event() in
  match e with
    KEYDOWN _ -> ()
  | _ -> wait_key()

(* point d'entree *)
let main() =
  begin
    (* chargement de SDL *)
    init [`VIDEO];
    enable_events all_events_mask;

    (* chargement de l'image *)
    let img = load_image "test.png" in

    (* bpp: bits par pixels, w et h: largeur et hauteur de la fenetre *)
    let (bpp, w, h) = (16, (surface_info img).w, (surface_info img).h) in

    (* ouverture de la fenetre (HWSURFACE: chargement en memoire video) *)
    let screen = set_video_mode ~w ~h ~bpp [`HWSURFACE] in

    (* Pour ceux qui sont sous i3 *)
    ignore (Sys.command "i3-msg floating enable");

    (* binarisation de l'image *)
    img_binarize img 0.45;

    (* 'collage' de l'image sur l'ecran *)
    blit_surface ~src:img ~dst:screen ();

    (* mise a jour de l'ecran *)
    flip screen;

    (* mise en pause du prog *)
    wait_key();

    (* liberation de la memoire allouee par SDL et quit clean *)
    quit();
    exit 0;
  end

let _ =
  try
    main()
  with
    Failure _ -> print_endline "Exception caught"
