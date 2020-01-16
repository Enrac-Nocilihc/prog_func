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


  
end

module Blocs =
struct

  include Parametres

  (* Blocs *)
  let largeur_bloc = 50
  let hauteur_bloc = 20

  let genererLigne y nbBlocs pui =
    let espace = (long_ecran -. float_of_int (nbBlocs * largeur_bloc)) /. (2. +. (float_of_int (nbBlocs - 1)) /. 2.) in
      let rec ajouterBloc x y nbBlocs pui =
        let bloc = (x, y, pui) in
          if nbBlocs = 0 then []
          else bloc::(ajouterBloc (x + int_of_float (espace /. 2.) + largeur_bloc) y (nbBlocs-1) pui)
      in ajouterBloc (int_of_float espace) y nbBlocs pui 

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

  (* let long_raq = 100. *)
  let hraq = 20.
  let yraq = 50
  let color_raq = red

  let get_xraq long_raq xsouris inv_contr = 
      let new_xsouris =
      (if xsouris > int_of_float (long_ecran -. long_raq/.2.) then int_of_float (long_ecran -. long_raq/.2.)
      else if xsouris < int_of_float (long_raq/.2.) then int_of_float (long_raq/.2.)
      else xsouris) in
      if inv_contr then int_of_float long_ecran - new_xsouris else new_xsouris

end


module Bonus =
struct

  (* Balle *)
  let rayon_bonus = 10
  let bonusTaille = (green, 1)
  let malusTaille = (red, 2) 
  let bonusBalle = (black, 3) 
  let invControle = (magenta, 4)

  let listeBonus = [malusTaille; bonusTaille; bonusBalle; invControle]
  let vitBon = -100.
  let plusLong = 50.
  let moinsLong = 25.


  let coord_relB x sens = x +. float_of_int(rayon_bonus * sens)
  let xdB x = coord_relB x 1
  let xgB x = coord_relB x (-1)
  let yhB y = coord_relB y 1
  let ybB y = coord_relB y (-1)


end


module All =
struct

  include Parametres
  include Raquette
  include Balle
  include Blocs
  include Bonus

end

module Init =
  struct

    include All

    let dt = 0.01
    let box_x = (0., long_ecran)
    let box_y = (0., haut_ecran)
    let alea = 1. (* Chances d'avoir un bonus *)

    let position0 = (Parametres.long_ecran /. 2., 150.)
    let vitesse0 = (200., 200.)
    let blocsInit = 
      (Blocs.genererLigne 500 5 5)
      @ (Blocs.genererLigne 450 6 4)
      @ (Blocs.genererLigne 400 7 3)
      @ (Blocs.genererLigne 350 8 2)
      @ (Blocs.genererLigne 300 9 1)
    
    let statut = (0, 1) (* Score / Vies *) 
    let bonusInit = [] (* Bonus de la forme (x,y,dy,type) *)
    let paramVariables = (100., false) (* Longueur raquette / Inversion des contrôles  *)
    let ballesInit = [(position0, vitesse0)] 
    let reset newStatut = (ballesInit, blocsInit, newStatut, paramVariables, bonusInit)
      
  end


module Drawing (F : Frame) =
  struct

  include All

  let draw r =

    let ref_r = ref r in
    let ref_handler_alrm = ref Sys.(Signal_handle (fun _ -> ())) in
    let ref_handler_int  = ref Sys.(Signal_handle (fun _ -> ())) in

    let handle_char c = if Char.code c = 27 then exit 0 in 
    
    let handler_alrm i =
      begin
        match Flux.uncons !ref_r with
        | None                          ->
            begin
              Sys.(set_signal sigalrm !ref_handler_alrm);
              Sys.(set_signal sigint  !ref_handler_int)
            end
        | Some ((balles, blocs, (score, vies), (long_raq, inv_contr), bonusTombants), r') ->
            begin
            (* Format.printf "r=(%f, %f); dr = (%f, %f)@." x y dx dy;*)
            Graphics.clear_graph ();


            (* Définition des fonctions graphiques *)
            
            let draw_raquette long_raq =
              set_color color_raq;
              let x = get_xraq long_raq (fst (mouse_pos ())) inv_contr in
                fill_rect (int_of_float (float_of_int x -. (long_raq)/.2.)) yraq (int_of_float long_raq) (int_of_float hraq);
            in

            let draw_balle ((x,y),_) =
              set_color color_ball;
              draw_circle (int_of_float x) (int_of_float y) rayon_ball;
            in

            let draw_bonus (x,y,_,(couleur,typeBonus)) =
              set_color couleur;
              draw_circle (int_of_float x) (int_of_float y) rayon_bonus;
            in

            let draw_bloc (x, y, power) =
              if power = 1 then set_color green;
              if power = 2 then set_color blue;
              if power = 3 then set_color magenta;
              if power = 4 then set_color red;
              if power = 5 then set_color black;
              fill_rect x y largeur_bloc hauteur_bloc
            in

            let draw_score score couleur =
              moveto 20 20;
              set_color couleur;
              set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
              draw_string "Score : ";
              draw_string (string_of_int score);
            in

            let draw_vies vies couleur =
              
              moveto (int_of_float long_ecran - 150) 20;
              set_color couleur;
              set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
              draw_string "Vies : ";
              draw_string (string_of_int vies);
            in

            let draw_game_over score vies =
              set_color black;
              moveto 0 0;
              fill_rect 0 0 (int_of_float long_ecran) (int_of_float haut_ecran);
              moveto (int_of_float (long_ecran /. 10.)) (int_of_float (haut_ecran /. 2.));
              set_color white;
              set_font "-*-fixed-medium-r-semicondensed--150-*-*-*-*-*-iso8859-1";
              draw_string "Game Over";
              moveto (int_of_float (long_ecran /. 3.2)) (int_of_float (haut_ecran /. 2.2));
              set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
              draw_string "Appuyez sur ESC pour sortir...";
              draw_score score white;
              draw_vies vies white;

            in 


            (* Application des fonctions graphiques *)
            if (vies <> 0) then
              (set_color black;
              List.iter draw_balle balles;
              List.iter draw_bonus bonusTombants;
              draw_raquette long_raq;
              draw_score score black;
              draw_vies vies red;
              List.iter draw_bloc blocs;)
            else
              draw_game_over score vies;

            Graphics.synchronize ();
            (* Traitement de la touche ESC *)
            if key_pressed () then handle_char (read_key ());
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

    let run (balles, blocs, statut, paramVariables, bonusTombants) =
      let rec newBalles listeBalles =
        match listeBalles with
        | [] -> Flux.constant ([])
        | (position0, vitesse0)::q ->
          let acceleration = Flux.constant (0., 0.) in
          let vitesse      = Flux.(map2 ( |+| ) (constant vitesse0) (integre F.dt acceleration)) in
          let position     = Flux.(map2 ( |+| ) (constant position0) (integre F.dt vitesse)) in
            let fluxCouples = (Flux.map2 (fun p v -> (p, v)) position vitesse) in
              Flux.map2 (fun elt l -> elt::l) fluxCouples (newBalles q) 
      in

      let rec newBonus listeBonus =
        match listeBonus with
        | [] -> Flux.constant ([])
        | (x, y, dy, typeBonus)::q ->
          let acceleration = Flux.constant (0., 0.) in
          let vitesse      = Flux.(map2 ( |+| ) (constant (0., dy)) (integre F.dt acceleration)) in
          let position     = Flux.(map2 ( |+| ) (constant (x, y)) (integre F.dt vitesse)) in
            let fluxCouples = (Flux.map2 (fun (x, y) (_, dy) -> (x, y, dy, typeBonus)) position vitesse) in
              Flux.map2 (fun elt l -> elt::l) fluxCouples (newBonus q) 
        in
        Flux.map2 (fun posvit bon -> (posvit, blocs, statut, paramVariables, bon)) (newBalles balles) (newBonus bonusTombants)
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
        

    let contact_bloc_y ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y dy = 
      xg x < xd_bloc && xd x > xg_bloc && (
        (yh y >= yb_bloc && yh y <= yb_bloc +. float_of_int hauteur_bloc /. 4. && dy > 0.) || 
        (yb y <= yh_bloc && yb y >= yh_bloc -. float_of_int hauteur_bloc /. 4. && dy < 0.))

    let contact_bloc_x ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y dx =
      (* not (contact_bloc_y ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y) && *)
      yb y < yh_bloc && yh y > yb_bloc && (
        (xd x >= xg_bloc && xd x <= xg_bloc +. float_of_int largeur_bloc /. 4. && dx > 0.) || 
        (xg x <= xd_bloc && xg x >= xd_bloc -. float_of_int largeur_bloc /. 4. && dx < 0.))
      

    let contact_blocs_x blocs x y dx = 
      List.fold_right (fun bloc reste -> (contact_bloc_x bloc x y dx) || reste) (bords blocs) false

    let contact_blocs_y blocs x y dy = 
      List.fold_right (fun bloc reste -> (contact_bloc_y bloc x y dy) || reste) (bords blocs) false

    let contact_blocs blocs x y dx dy =
      contact_blocs_x blocs x y dx || contact_blocs_y blocs x y dy

    let contact_x (infx, supx) blocs x y dx = 
      ((x -. (float_of_int rayon_ball)) <= infx && dx < 0.) || 
      ((x +. (float_of_int rayon_ball)) >= supx && dx > 0.) ||
      contact_blocs_x blocs x y dx

    let contact_y (infy, supy) blocs x y dy = 
      ((y +. (float_of_int rayon_ball) >= supy && dy > 0.)) ||
      ((y -. (float_of_int rayon_ball)) <= infy && dy < 0.) ||
      contact_blocs_y blocs x y dy

    let contact_raq xraq x y dy long_raq= 
      dy < 0. && 
      float_of_int yraq +. hraq > yb y && 
      float_of_int yraq < yb y && 
      float_of_int xraq -. long_raq /. 2. < x && 
      float_of_int xraq +. long_raq /. 2. > x

    let contact_bloc ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y dx dy= 
      contact_bloc_x ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y dx ||
      contact_bloc_y ((xg_bloc, xd_bloc), (yb_bloc, yh_bloc)) x y dy
              
    (*let contact_bloc blocs x dx = 
      List.*)

    let rebond_balle ((x, y), (dx, dy), blocs, (score,vies), (long_raq, inv_controles), bonusTombants) =
      
      let norme vx vy =
        sqrt ((vx ** 2.) +. (vy ** 2.))
      in

      let sign x = if x < 0. then -.1. else if x > 0. then 1. else 0. in

      let pivote (vx, vy) theta =
        let (new_vx, new_vy) = (vx *. cos theta +. vy *. sin theta,
        -.vx *. sin theta +. vy *. cos theta) in
        let n = norme new_vx new_vy in 
        let a = 3.1415/.8. in
          if (abs_float new_vx) /. n > cos a then (sign new_vx *. n *. cos a, sign vy *. n *. abs_float (sin a))
          else (new_vx, new_vy)
      in

      let xraq = get_xraq long_raq (fst (Graphics.mouse_pos ())) inv_controles in
        
      let new_dx = (if contact_x F.box_x blocs x y dx then -. dx *. acceleration else dx) in
      let new_dy = (if (contact_y F.box_y blocs x y dy) || (contact_raq xraq x (yb y) dy long_raq) then -. dy *. acceleration else dy) in
        let dist_centre = x -. (float_of_int xraq) in
        let ratio_centre = (dist_centre /. (long_raq /. 2.)) in
        let thetaAjoute = (if (contact_raq xraq x (yb y) dy long_raq) then ratio_centre *. 3.1415 /. 4. else 0.) in
        let newScore = score + (if contact_blocs blocs x y dx dy then 20 * vies else 0) in
        let (new_blocs, new_bonus) = List.fold_right (fun t q ->
            if (contact_bloc (bords_solo t) x y dx dy) then 
              (let (px, py, pui) = t in 
                if(pui = 1) then 
                  let rand = Random.float 1. in
                    if rand < Init.alea then 
                      let indBon = Random.int (List.length listeBonus) in 
                      let typeBon = (List.nth listeBonus indBon) in 
                      (fst q, (x, y, vitBon, typeBon)::(snd q))
                    else q
                else ((px, py, pui - 1)::(fst q), (snd q)))
            else (t::(fst q), snd q) ) blocs ([], bonusTombants)
        in
          ((x, y), pivote (new_dx, new_dy) thetaAjoute, new_blocs, newScore, (long_raq, inv_controles), new_bonus ) (* TODO : Longueur et inversion *)


    let contact_un_bonus xraq (x, y, _, _) long_raq =
      float_of_int yraq +. hraq > ybB y && 
      float_of_int yraq < yhB y && 
      float_of_int xraq -. long_raq /. 2. < x && 
      float_of_int xraq +. long_raq /. 2. > x

    let appliquer_bonus_touches (b, bl, s, v, bon) xraq=

      let appliquer_bonus (b, bl, s, (long_raq, inv_contr)) xraq bonus =
        let ((xb,yb),(dx,dy)) = List.hd b in 
        let (x,y,_,(_, typeBon)) = bonus in
          if contact_un_bonus xraq bonus long_raq then
            match typeBon with
            | 1 -> (b, bl, s, (long_raq +. Bonus.plusLong, inv_contr))
            | 2 -> (b, bl, s, (long_raq -. Bonus.moinsLong, inv_contr))
            | 3 -> (((xb,yb),(-.dx,dy))::b, bl, s, (long_raq, inv_contr))
            | 4 -> (b, bl, s, (long_raq, not inv_contr))
            | _ -> (b, bl, s, (long_raq, inv_contr))
          else (b, bl, s, (long_raq, inv_contr))
      in
      List.fold_right (fun t (b, bl, s, v, bon) -> 
        if contact_un_bonus xraq t (fst v) then
          let (bn, bln, sn, vn) = appliquer_bonus (b, bl, s, v) xraq t in (bn, bln, sn, vn, bon)
        else (b, bl, s, v, t::bon)) bon (b, bl, s, v, [])


    let rebond (balles, blocs, (score,vies), (long_raq, inv_controles), bonusTombants) =
      let xraq = get_xraq long_raq (fst (Graphics.mouse_pos ())) inv_controles in
      let rec rebondRec (balles, blocs, (score,vies), (long_raq, inv_controles), bonus) = 
        let addBalle (ba, bl, s, v, bon) b =
          (b::ba, bl, s, v, bon)
        in
          match balles with
          | [] -> ([], blocs, (score, vies), (long_raq, inv_controles), bonus)
          | (pos, vit)::q -> 
            if (yb (snd pos)) < 0. then rebondRec (q, blocs, (score,vies),  (long_raq, inv_controles), bonus)
            else
              let (pos2, vit2, newBlocs, newScore, (new_long_raq, new_inv_controles), new_bonus ) = rebond_balle (pos, vit, blocs, (score, vies), (long_raq, inv_controles), bonus) in
                addBalle (rebondRec (q, newBlocs, (newScore,vies), (new_long_raq, new_inv_controles), new_bonus)) (pos2, vit2)
      in
        let (b, bl, s, v, bon) = rebondRec (balles, blocs, (score,vies), (long_raq, inv_controles), bonusTombants) in
        match b with
        | [] -> if (snd s - 1) != 0  
                  then (Init.ballesInit, bl, (fst s, snd s - 1), Init.paramVariables, Init.bonusInit)
                  else ([], bl, (fst s, snd s - 1), Init.paramVariables, Init.bonusInit)
        | _ -> appliquer_bonus_touches (b, bl, s, v, bon) xraq


    let contact_raq_bonus xraq bonusTombants long_raq =
      List.fold_right (fun t q -> (contact_un_bonus xraq t long_raq || q)) bonusTombants false 
      

    let rec contact (balles, blocs, statut, paramVariables, bonusTombants) = 
      let (long_raq, inv_contr) = paramVariables in 
      let xraq = get_xraq long_raq (fst (Graphics.mouse_pos ())) inv_contr in
      match balles with
      | [] -> false
      | ((x,y), (dx,dy))::q -> 
        (contact_x F.box_x blocs x y dx) || 
        (contact_y F.box_y blocs x y dy) || 
        (contact_raq xraq x y dy long_raq) || 
        (contact (q, blocs, statut, paramVariables, bonusTombants)) ||
        (contact_raq_bonus xraq bonusTombants long_raq)
    
    module FF = FreeFall (F)

    let rec run etat0 =
      unless (FF.run etat0) contact (fun etat -> run (rebond etat))
  end


module Draw = Drawing (Init)
module Bounce = Bouncing (Init)

let _  =
  Random.init 7364;
  Draw.draw (Bounce.run (Init.reset Init.statut));;




