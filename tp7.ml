open Graphics

(* Interfaces d'iterateurs / de flux *)
module type SimpleIter =
sig
  type 'a t
  val vide : 'a t
  val cons : 'a -> 'a t -> 'a t
  val unfold : ('s -> ('a * 's) option) -> 's -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t                        
  val constant : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Iter =
sig
  include SimpleIter
  val uncons : 'a t -> ('a * 'a t) option
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

(* Le module Flux implantant l'interface de flux Iter *)
type 'a flux = Tick of ('a * 'a flux) option Lazy.t;;
module Flux : Iter with type 'a t = 'a flux =
  struct
    type 'a t = 'a flux = Tick of ('a * 'a t) option Lazy.t;;

    let vide = Tick (lazy None);;

    let cons t q = Tick (lazy (Some (t, q)));;

    let uncons (Tick flux) = Lazy.force flux;;
 
    let rec apply f x =
      Tick (lazy (
      match uncons f, uncons x with
      | None         , _             -> None
      | _            , None          -> None
      | Some (tf, qf), Some (tx, qx) -> Some (tf tx, apply qf qx)));;

    let rec unfold f e =
      Tick (lazy (
      match f e with
      | None         -> None
      | Some (t, e') -> Some (t, unfold f e')));;

    let rec filter p flux =
      Tick (lazy (
      match uncons flux with
      | None        -> None
      | Some (t, q) -> if p t then Some (t, filter p q)
                       else uncons (filter p q)));;
    
    let rec append flux1 flux2 =
      Tick (lazy (
      match uncons flux1 with
      | None          -> uncons flux2
      | Some (t1, q1) -> Some (t1, append q1 flux2)));;
    
    let constant c = unfold (fun () -> Some (c, ())) ();;
    (* implantation rapide mais inefficace de map *)
    let map f i = apply (constant f) i;;

    let map2 f i1 i2 = apply (apply (constant f) i1) i2;;
end

(* Parametres globaux de la simulation     *)
(* dt : pas de temps                       *)
(* box_x : paire d'abscisses (xmin, xmax)  *)
(* box_y : paire d'ordonnees (ymin, ymax)  *)
module type Frame =
  sig
    val dt : float
    val box_x : float * float
    val box_y : float * float
  end
  

module Parametres =
struct

  (* Paramètres de l'écran *)
  let long_ecran = 800.
  let haut_ecran = 600.



  (* Blocs *)
  let largeur_bloc = 100
  let hauteur_bloc = 20
  
end

module Balle =
struct

  (* Balle *)
  let rayon_ball = 5
  let color_ball = black


  let coord_rel x sens = x +. float_of_int(rayon_ball * sens)
  let xd x = coord_rel x 1
  let xg x = coord_rel x (-1)
  let yh y = coord_rel y 1
  let yb y = coord_rel y (-1)


end

module Raquette =
struct

  include Parametres

  let long_raq = 100.
  let hraq = 20.
  let yraq = 50
  let color_raq = red

  let get_xraq = 
    (fun xsouris ->
      if xsouris > int_of_float (long_ecran -. long_raq/.2.) then int_of_float (long_ecran -. long_raq/.2.)
      else if xsouris < int_of_float (long_raq/.2.) then int_of_float (long_raq/.2.)
      else xsouris)

end

module All =
struct

  include Parametres
  include Raquette
  include Balle

end

module Drawing (F : Frame) =
  struct

  include All

  let draw r =

    let ref_r = ref r in
    let ref_handler_alrm = ref Sys.(Signal_handle (fun _ -> ())) in
    let ref_handler_int  = ref Sys.(Signal_handle (fun _ -> ())) in
    let handler_alrm i =
      begin
        match Flux.uncons !ref_r with
        | None                          ->
            begin
              Sys.(set_signal sigalrm !ref_handler_alrm);
              Sys.(set_signal sigint  !ref_handler_int)
            end
        | Some (((x, y), (dx, dy), blocs), r') ->
            begin
            (* Format.printf "r=(%f, %f); dr = (%f, %f)@." x y dx dy;*)
            Graphics.clear_graph ();


            (* Définition des fonctions graphiques *)
            
            let draw_raquette =
              set_color color_raq;
              let x = get_xraq (fst (mouse_pos ())) in
                fill_rect (int_of_float (float_of_int x -. (long_raq)/.2.)) yraq (int_of_float long_raq) (int_of_float hraq);
            in

            let draw_balle =
              set_color color_ball;
              draw_circle (int_of_float x) (int_of_float y) rayon_ball;
            in

            let draw_bloc (x, y, color) =
              set_color color; 
              fill_rect x y largeur_bloc hauteur_bloc
            in


            (* Application des fonctions graphiques *)
            set_color black;
            draw_balle;
            draw_raquette;
            List.iter draw_bloc blocs;
            Graphics.synchronize ();
            (*ignore (read_line ());*)
            ref_r := r'
            end
      end in
    let handler_int i =
      begin
        ref_r := Flux.vide
      end in
    begin
      let (inf_x, sup_x) = F.box_x in
      let (inf_y, sup_y) = F.box_y in
      let size_x = int_of_float (sup_x -. inf_x) in
      let size_y = int_of_float (sup_y -. inf_y) in
      Graphics.open_graph (Format.sprintf " %dx%d" size_x size_y);
      Graphics.auto_synchronize false;
      Sys.(ref_handler_alrm := signal sigalrm (Signal_handle handler_alrm));
      Sys.(ref_handler_int  := signal sigint  (Signal_handle handler_int));
      Unix.(setitimer ITIMER_REAL { it_interval = F.dt; it_value = F.dt })
    end    
  end

(* Caracteristiques du système:  *)
(* pas de temps + etat du mobile *)
module type Params =
  sig
    val dt : float
    val masse0 : float
    val position0 : float * float
    val vitesse0  : float * float
  end

module FreeFall (F : Frame) =
  struct
    let (|+|) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
    let (|*|) k (x, y) = (k *. x, k *. y)

    let integre dt flux =
      let init = (0., 0.) in
      let rec acc =
        Tick (lazy (Some (init, Flux.map2 (fun a f -> a |+| (dt |*| f)) acc flux)))
      in acc

    let run (position0, vitesse0, blocs) =
      let acceleration = Flux.constant (0., 0.) in
      let vitesse      = Flux.(map2 ( |+| ) (constant vitesse0) (integre F.dt acceleration)) in
      let position     = Flux.(map2 ( |+| ) (constant position0) (integre F.dt vitesse)) in
      Flux.map2 (fun a b -> (a, b, blocs)) position vitesse
  end

module Bouncing (F : Frame) =
  struct

    include All

    let acceleration = 1.01

    (* version avec unfold sans récursivité directe *)
    let unless flux cond f_cond =
      Flux.unfold (fun (init, f) ->
          match Flux.uncons f with
          | None         -> None
          | Some (v, f') -> Some (v, (false, f'))
        ) (true, flux)

    (* version avec récursivité, donc paresse explicite *)
    let rec unless flux cond f_cond =
      Tick (lazy (
                match Flux.uncons flux with
                | None        -> None
                | Some (t, q) -> if cond t then Flux.uncons (f_cond t) else Some (t, unless q cond f_cond)
        ))
    
    let bords_solo (x, y, _) = 
      ((float_of_int x, (float_of_int x) +. (float_of_int largeur_bloc)), 
      ((float_of_int y, (float_of_int y) +. (float_of_int hauteur_bloc) )))

    let bords blocs = List.map bords_solo blocs
        

    let contact_bloc_x ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y =
      yb y < yh_bloc && yh y > yb_bloc && ((xd x >= xg_bloc && xd x <= xd_bloc) || (xg x <= xd_bloc && xg x >= xg_bloc))
      
    let contact_bloc_y ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y = 
      xg x < xd_bloc && xd x > xg_bloc && ((yh y >= yb_bloc && yh y <= yh_bloc) || (yb y <= yh_bloc && yb y >= yb_bloc))

    let contact_blocs_x blocs x y = 
      List.fold_right (fun bloc reste -> (contact_bloc_x bloc x y) || reste) (bords blocs) false

    let contact_blocs_y blocs x y = 
      List.fold_right (fun bloc reste -> (contact_bloc_y bloc x y) || reste) (bords blocs) false


    let contact_x (infx, supx) blocs x y dx = 
      ((x -. (float_of_int rayon_ball)) <= infx && dx < 0.) || 
      ((x +. (float_of_int rayon_ball)) >= supx && dx > 0.) ||
      contact_blocs_x blocs x y

    let contact_y (infy, supy) blocs x y dy = 
      ((y +. (float_of_int rayon_ball) >= supy && dy > 0.)) ||
      contact_blocs_y blocs x y

    let contact_raq xraq x y dy = 
      dy < 0. && 
      float_of_int yraq +. hraq > yb y && 
      float_of_int yraq < yb y && 
      float_of_int xraq -. long_raq /. 2. < x && 
      float_of_int xraq +. long_raq /. 2. > x

    let contact_bloc ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y = 
      contact_bloc_x ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y  ||
      contact_bloc_y ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y
              
    (*let contact_bloc blocs x dx = 
      List.*)

    let rebond ((x, y), (dx, dy), blocs) =
      
      let pivote (vx, vy) theta =
        (vx *. cos theta +. vy *. sin theta,
        -.vx *. sin theta +. vy *. cos theta)
      in

      let xraq = get_xraq (fst (Graphics.mouse_pos ())) in
        
      let new_dx = (if contact_x F.box_x blocs x y dx then -. dx *. acceleration else dx) in
      let new_dy = (if (contact_y F.box_y blocs x y dy) || (contact_raq xraq x (yb y) dy) then -. dy *. acceleration else dy) in
        let dist_centre = x -. (float_of_int xraq) in
        let ratio_centre = (dist_centre /. (long_raq /. 2.)) in
        let thetaAjoute = (if (contact_raq xraq x (yb y) dy) then ratio_centre *. 3.1415 /. 4. else 0.) in
        let new_blocs = List.fold_right (fun t q -> if (contact_bloc (bords_solo t) x y) then q else t::q ) blocs [] in
          ((x, y), pivote (new_dx, new_dy) thetaAjoute, new_blocs)

    let contact ((x, y), (dx, dy), blocs) = 
      let xraq = get_xraq (fst (Graphics.mouse_pos ())) in
      (contact_x F.box_x blocs x y dx) || (contact_y F.box_y blocs x y dy) || (contact_raq xraq x y dy)
    
    module FF = FreeFall (F)

    let rec run etat0 =
      unless (FF.run etat0) contact (fun etat -> run (rebond etat))
  end

module Init =
  struct

    include All

    let dt = 0.01
    let box_x = (0., long_ecran)
    let box_y = (0., haut_ecran)
  end

module Draw = Drawing (Init)
module Bounce = Bouncing (Init)

let _  =
  let position0 = (300., 400.) in
  let vitesse0 = (100., -500.) in
  let blocsInit = [
    (100, 500, black);
    (300, 500, red);
    (500, 500, yellow)] in
  Draw.draw (Bounce.run (position0, vitesse0, blocsInit));;




