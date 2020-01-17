open Graphics

(* TODO : Continuer FreeFall *)

(* 
------------------------------------------
Interfaces et modules des objets utilisés 
------------------------------------------
*)

(* Solides = Blocs / Balle / Raquette *)

module type NombreItf =
  sig

  type nb

  val operator2 : nb -> nb -> (int -> int -> 'a) -> (float -> float -> 'a) -> 'a
  
  (* Opérations *)

  val ( /- ) : nb -> nb -> nb
  val ( /~ ) : nb -> int -> nb
  val ( /~. ) : nb -> float -> nb

  val ( +- ) : nb -> nb -> nb
  val ( +~ ) : nb -> int -> nb
  val ( +~. ) : nb -> float -> nb

  val ( -- ) : nb -> nb -> nb
  val ( -~ ) : nb -> int -> nb
  val ( -~. ) : nb -> float -> nb

  val ( *- ) : nb -> nb -> nb
  val ( *~ ) : nb -> int -> nb
  val ( *~. ) : nb -> float -> nb

  (* Comparaisons *)

  val ( >> ) : nb -> nb -> bool
  val ( >~ ) : nb -> int -> bool
  val ( >~. ) : nb -> float -> bool
  
  val ( << ) : nb -> nb -> bool
  val ( <~ ) : nb -> int -> bool
  val ( <~. ) : nb -> float -> bool
  
  val ( >>= ) : nb -> nb -> bool
  val ( >=~ ) : nb -> int -> bool
  val ( >=~. ) : nb -> float -> bool
  
  val ( <<= ) : nb -> nb -> bool
  val ( <=~ ) : nb -> int -> bool
  val ( <=~. ) : nb -> float -> bool
  

  val ( ->> ) : nb -> int -> nb
  val ( ->>. ) : nb -> float -> nb

  val toInt : int -> nb
  val toFloat : float -> nb

  val intv : nb -> int
  val floatv : nb -> float

  end

module Nombre : NombreItf =

  struct

  type nb = 
    | Int of int
    | Float of float

  
  let operatorInt a b opInt =
    match a with
    | Int x -> opInt x b
    | Float x -> opInt (int_of_float x) b

  let operatorFloat a b opFloat =
    match a with
    | Int x -> opFloat (float_of_int x) b
    | Float x -> opFloat x b
  
  let operator2 a b opInt opFloat =
    match (a, b) with
    | (Int x, Int y) -> opInt x y
    | (Int x, Float y) -> opFloat (float_of_int x) y
    | (Float x, Float y) -> opFloat x y
    | (Float x, Int y) ->  opFloat x (float_of_int y)

  let ( /- ) a b = operator2 a b (fun a b -> Int(a / b)) (fun a b -> Float(a /. b))
  let ( /~ ) a b = operatorInt a b (fun a b -> Int(a / b))
  let ( /~. ) a b = operatorFloat a b (fun a b -> Float(a /. b))

  let ( +- ) a b = operator2 a b (fun a b -> Int(a + b)) (fun a b -> Float(a +. b))
  let ( +~ ) a b = operatorInt a b (fun a b -> Int(a + b))
  let ( +~. ) a b = operatorFloat a b (fun a b -> Float(a +. b))

  let ( -- ) a b = operator2 a b (fun a b -> Int(a - b)) (fun a b -> Float(a -. b))
  let ( -~ ) a b = operatorInt a b (fun a b -> Int(a - b))
  let ( -~. ) a b = operatorFloat a b (fun a b -> Float(a -. b))

  let ( *- ) a b = operator2 a b (fun a b -> Int(a * b)) (fun a b -> Float(a *. b))
  let ( *~ ) a b = operatorInt a b (fun a b -> Int(a * b))
  let ( *~. ) a b = operatorFloat a b (fun a b -> Float(a *. b))

  let ( >> ) a b = operator2 a b ( > ) ( > )
  let ( >~ ) a b = operatorInt a b (fun a b -> a > b)
  let ( >~. ) a b = operatorFloat a b (fun a b -> a > b)

  let ( << ) a b = operator2 a b ( < ) ( < )
  let ( <~ ) a b = operatorInt a b (fun a b -> a < b)
  let ( <~. ) a b = operatorFloat a b (fun a b -> a < b)

  let ( <<= ) a b = operator2 a b ( <= ) ( <= )
  let ( <=~ ) a b = operatorInt a b (fun a b -> a <= b)
  let ( <=~. ) a b = operatorFloat a b (fun a b -> a <= b)

  let ( >>= ) a b = operator2 a b ( >= ) ( >= )
  let ( >=~ ) a b = operatorInt a b (fun a b -> a >= b)
  let ( >=~. ) a b = operatorFloat a b (fun a b -> a >= b)

  let toInt a = Int(a)
  let toFloat a = Float(a)

  let intv a = 
    match a with
    | Int x -> x
    | Float x -> int_of_float x

  let floatv a = 
    match a with
    | Int x -> float_of_int x
    | Float x -> x

  (* Convertit b en même type que a *)
  let (->>) a b = 
    match a with
    | Int x -> Int(b)
    | Float x -> Float(float_of_int b)
  
  let (->>.) a b = 
    match a with
    | Int x -> Int(int_of_float b)
    | Float x -> Float(b)

  end


(* Paramètres généraux *)
module Parametres =
  struct

    include Nombre

    let long_ecran = toInt 800
    let haut_ecran = toInt 600

    let zone_blocs = (
      (long_ecran /~ 8, (long_ecran *~ 7) /~ 8), (* xmin, xmax *)
      (haut_ecran /~ 2, (haut_ecran *~ 7) /~ 8)) (* ymin, ymax *)

  end


module type SolideItf =
  sig

    include NombreItf

    type ('a, 'b) t

    (* Construit un solide *)
    val cons: (nb * nb) -> nb -> nb -> color -> 'a -> ('a, nb) t 

    (* Getters et Setters *)
    val long : (_, nb) t -> nb
    val setLong : ('a, nb) t -> nb -> ('a, nb) t
    val haut : (_, nb) t -> nb
    val couleur : (_, _) t -> color
    val setCouleur : ('a, nb) t -> color -> ('a, nb) t
    val param : ('a, _) t -> 'a
    val setParam : ('a, nb) t -> 'a -> ('a, nb) t

    (* Rayon sur x et y (distance au centre)*)
    val rx : (_, nb) t -> nb
    val ry : (_, nb) t -> nb

    (* Coordonnées à différents niveaux *)
    val xd : (_, nb) t -> nb  (* x Droite *)
    val xg : (_, nb) t -> nb  (* x Gauche *)
    val yh : (_, nb) t -> nb  (* y Haut *)
    val yb : (_, nb) t -> nb  (* y Bas *)
    val xc : (_, nb) t -> nb 
    val yc : (_, nb) t -> nb

  end

module Solide : SolideItf with type nb = Nombre.nb =
  struct

    include Nombre
    (* Position du coin inf gauche * hauteur * longueur * couleur * parametres propres *)

    type ('a, 'b) t = ('b * 'b) * 'b * 'b * color * 'a 

    let cons (x,y) l h color elt = ((x,y), l, h, color, elt)

    let xg ((x,y), l, h, color, _) = x
    let yb ((x,y), l, h, color, _) = y
    let long ((x,y), l, h, color, _) = l
    let haut ((x,y), l, h, color, _) = h
    let couleur ((x,y), l, h, color, _) = color
    let param ((x,y), l, h, color, elt) = elt

    let setParam (pos, l, h, color, elt) param = cons pos l h color param
    let setCouleur (pos, l, h, color, elt) newC = cons pos l h newC elt
    let setLong (pos, l, h, color, elt) longueur = cons pos longueur h color elt
    
    let rx s = (long s) /~ 2
    let ry s = (haut s) /~. 2.

    let xc s = xg s +- rx s
    let yc s = yb s +- ry s
    let yh s = yb s +- haut s
    let xd s = xg s +- long s

    
  end



(* Blocs *)
module type BlocItf =
  sig

    include SolideItf

    type tb

    (* Construit un bloc *)
    val cons : (nb * nb) -> nb -> nb -> int -> (tb, nb) t

    (* Convertit la donnée de puissance en couleur *)
    val power_to_couleur : int -> color

    (* Diminue de 1 la puissance d'un bloc *)
    val downgrade : (tb, nb) t -> (tb, nb) t

    (* Génère une ligne de blocs d'une certaine puissance et d'une certaine taille *)
    val genererLigne : nb -> nb -> nb -> int -> int -> nb -> (tb, nb) t list

    (* Génère une ligne de blocs d'une certaine puissance et d'une certaine taille *)
    val genererNiveau : int -> (tb, nb) t list

    

  end

module Bloc : BlocItf with type nb = Nombre.nb  =
  struct

    include Solide
    include Parametres

    (* Représente la puissance du bloc *)
    type tb = int


    let fun_evo_lignes_nb = fun niveau hauteur -> (5 + 2 * (niveau + hauteur))
    let fun_evo_lignes_pw = fun niveau hauteur -> (niveau + niveau * hauteur)
    let fun_nb_lignes = fun niveau -> niveau + 5


    let power_to_couleur power = 
      if(power <= -1) then black
      else if power = 1 then green
      else if power = 2 then blue
      else if power = 3 then magenta
      else if power = 4 then red
      else if power >= 5 then black
      else white


    let cons (x,y) long haut power = Solide.cons (x,y) long haut (power_to_couleur power) power


    let downgrade bloc = setParam bloc ((param bloc) - 1)


    let genererLigne y xmin xmax nbBlocs power h = 
      let l = (xmax -- xmin) /~ nbBlocs
      in
        let rec ajouterBloc x nbBlocs power =
          let bloc = cons (x,y) l h power in
            if nbBlocs = 0 then []
            else if nbBlocs < 0 then raise (Failure "Oops !")
            else bloc::(ajouterBloc (x +- l) (nbBlocs - 1) power)
        in 
        ajouterBloc xmin nbBlocs power 
    
    
    let genererNiveau n =

      let ((xmin, xmax), (ymin, ymax)) = zone_blocs in
        let nbLignes = fun_nb_lignes n in
          let h = (ymax -- ymin) /~ nbLignes in
            let rec ajouterLigne y nbL =
              if nbL = 0 then []
              else let ligne = genererLigne y xmin xmax (fun_evo_lignes_nb n nbL) (fun_evo_lignes_pw n nbL) h in
              ligne::(ajouterLigne (y -- h) (nbL - 1))
            in 
              List.flatten (ajouterLigne ymax nbLignes)


  end




(* Raquette *)
module type RaquetteItf =
  sig
    include SolideItf
    type tr

    val init : (tr, nb) t
    val sens : (tr, nb) t -> bool
    val changeSens : (tr, nb) t -> (tr, nb) t
    val ajouterTaille : (tr, nb) t -> int -> (tr, nb) t
    val getXRaq : (tr, nb) t -> nb -> nb 
  end

module Raquette : RaquetteItf with type nb = Nombre.nb =
  struct

    include Solide
    include Parametres

    type tr = bool
    
    let haut_raq = toInt 100
    let long_raq = toInt 100
    let y_raq = toInt 20
    let color_raq = red

    let init = cons ((long_ecran /~ 2), y_raq) long_raq haut_raq color_raq true
    let sens raq = param raq
    let changeSens raq = setParam raq (not (param raq))
    let ajouterTaille raq taille = setLong raq (toInt(max 0 (intv (long raq) + taille)))

    let getXRaq raq xsouris =
      let new_xsouris =
        if xsouris > long_ecran -- rx raq then long_ecran -- rx raq
        else if xsouris < rx raq          then rx raq
        else xsouris 
      in
        if sens raq then new_xsouris else long_ecran -- new_xsouris
    
  end


(* Balle *)
module type BalleItf =
  sig

    include SolideItf

    type tba

    val init : (tba, nb) t

    (* Retourne true si la balle fournie en argument est en collision avec la raquette fournie en argument *)
    val collision_raq : (tba, nb) t -> (Raquette.tr, nb) t -> bool

    (* Retourne true si la balle fournie en argument est en collision 
    (sur l'axe précisé, si précisé) avec le(s) bloc(s) fourni(s) en argument *)
    val collision_x_bloc : (tba, nb) t -> (Bloc.tb, nb) t -> bool
    val collision_x_blocs : (tba, nb) t -> (Bloc.tb, nb) t list -> bool
    val collision_y_bloc : (tba, nb) t -> (Bloc.tb, nb) t -> bool
    val collision_y_blocs : (tba, nb) t -> (Bloc.tb, nb) t list -> bool
    val collision_bloc : (tba, nb) t -> (Bloc.tb, nb) t -> bool
    val collision_blocs : (tba, nb) t -> (Bloc.tb, nb) t list -> bool
    val dupliquer : (tba, nb) t list -> (tba, nb) t list

  end

module Balle : BalleItf with type nb = Nombre.nb =
  struct

    include Solide

    (* Représente la vitesse de la balle *)
    type tba = nb * nb

    let posInit = (toInt 120, toInt 100)
    let diametre = toInt 10
    let vitesseInit = (toFloat 100., toFloat 100.)

    let init = cons posInit diametre diametre black vitesseInit

    let vit balle = param balle

    let collision_raq balle raq = 
      let (dx, dy) = vit balle in
        dy << toFloat(0.) && 
        yh raq > yb balle && 
        yc raq < yb balle && 
        xg raq < xc balle && 
        xd raq > xc balle


    let collision_x_bloc balle bloc = let (dx, dy) = vit balle in
      yb balle << yh bloc && yh balle >> yb bloc && (
      (xd balle >>= xg bloc && xd balle <<= xc bloc && dx >~. 0.) || 
      (xg balle <<= xd bloc && xg balle >>= xc bloc && dx <~. 0.) )

    let collision_y_bloc balle bloc = let (dx, dy) = vit balle in
      xg balle << xd bloc && yh balle >> xg bloc && (
      (yh balle >>= yb bloc && yh balle <<= xc bloc && dy >~. 0.) || 
      (yb balle <<= yh bloc && yb balle >>= xc bloc && dy <~. 0.) )

    let collision_bloc balle bloc = collision_x_bloc balle bloc || collision_y_bloc balle bloc 

    let collision_inc_blocs balle blocs = 
      fun f -> List.fold_right (fun t qt -> qt || f balle t) blocs false
    
    let collision_x_blocs balle bloc = collision_inc_blocs balle bloc collision_x_bloc
    let collision_y_blocs balle bloc = collision_inc_blocs balle bloc collision_y_bloc
    let collision_blocs balle bloc = collision_inc_blocs balle bloc collision_bloc
   
    let dupliquer listeBalles = 
      let prem = List.hd listeBalles in
        let (vx, vy) = param prem in
          (setParam prem (vx *~ (-1), vy))::listeBalles

    
  end




(* Bonus *)
module type BonusItf =
  sig

    include SolideItf 
        
    type bonusType
    type 'a tbo


    val fun_tailleRaquetteUp : (Raquette.tr, nb) Raquette.t tbo * color
    val fun_tailleRaquetteDown : (Raquette.tr, nb) Raquette.t tbo * color
    val fun_tailleRaquetteInv : (Raquette.tr, nb) Raquette.t tbo * color

    val fun_multiballes : (Balle.tba, Balle.nb) Balle.t list tbo * color

    val fun_vieSupp : int tbo * color
    
    val listeBonus : bonusType list

    val init : ('a tbo * color) -> nb -> nb -> ('a tbo, nb) t
    
  end


module Bonus : BonusItf with type nb = Nombre.nb =
  struct

    include Solide

    (* Evolution de l'objet *)

    type 'a tbo = 'a -> 'a
    type bonusType = 
      | BonusRaq of (nb -> nb -> ((Raquette.tr, nb) Raquette.t tbo, nb) t)
      | BonusBal of (nb -> nb -> ((Balle.tba, nb) Balle.t list tbo, nb) t)
      | BonusVie of (nb -> nb -> (int tbo, nb) t)
      
    let vitesse = (toFloat 0., toFloat (-.100.))
    let rayon = toInt 10

    let fun_tailleRaquetteUp = ((fun raq -> Raquette.ajouterTaille raq 20), green)
    let fun_tailleRaquetteDown = ((fun raq -> Raquette.ajouterTaille raq (-20)), red) 
    let fun_tailleRaquetteInv = ((Raquette.changeSens), magenta)
    let fun_vieSupp = ((fun vies -> vies + 1), red)
    
    let fun_multiballes = ((fun listeBalles -> Balle.dupliquer listeBalles), black)


    let init (func, coul) x y  = cons (x,y) (rayon *~ 2) (rayon *~ 2) coul func

    let listeBonus = [
      BonusRaq(init fun_tailleRaquetteUp);
      BonusRaq(init fun_tailleRaquetteDown);
      BonusRaq(init fun_tailleRaquetteInv);
      BonusBal(init fun_multiballes);
      BonusVie(init fun_vieSupp)
    ]

  end

(* Jeu *)
module type JeuItf =
  sig
  
    include NombreItf

    type g
    type ('a, 'b) t
  
    val cons : (Balle.tba, nb) Balle.t list -> (Bloc.tb, nb) Bloc.t list -> ('b Bonus.tbo, nb) Bonus.t list -> (Raquette.tr, nb) Raquette.t -> g -> (g, 'b) t
    
    val balles : (g, 'b)t -> (Balle.tba, nb) Balle.t list
    val blocs : (g, 'b) t -> (Bloc.tb, nb) Bloc.t list
    val bonus : (g, 'b) t -> ('b Bonus.tbo, nb) Bonus.t list
    val raquette : (g, 'b) t -> (Raquette.tr, nb) Raquette.t
    val param : (g, 'b) t -> g
    
    val ajouterBalle : (g, 'b) t -> (Balle.tba, nb) Balle.t -> (g, 'b) t
    val ajouterBloc : (g, 'b) t -> (Bloc.tb, nb) Bloc.t -> (g, 'b) t
    val ajouterBonus : (g, 'b) t -> ('b Bonus.tbo, nb) Bonus.t -> (g, 'b) t
    val remplRaquette : (g, 'b) t -> (Raquette.tr, nb) Raquette.t -> (g, 'b) t
    
    
    val score : (g, 'b) t -> int
    val vies : (g, 'b) t -> int
    val niveau : (g, 'b) t -> int
    
    val init : (g, 'b) t
    
    val niveauTermine : (g, 'b) t -> bool
    val passerNiveau : (g, 'b) t -> (g, 'b) t

  
  end

module Jeu : JeuItf with type nb = Nombre.nb =
  struct

    include Nombre

    (* Balles / Blocs / Bonus Tombants / Paramètres généraux*)
    type ('a, 'b) t = (Balle.tba, nb) Balle.t list * (Bloc.tb, nb) Bloc.t list * ('b Bonus.tbo, nb) Bonus.t list * (Raquette.tr, nb) Raquette.t * 'a
    (* Type des paramètres généraux du jeu : Score / Vies / Niveau en cours *)
    type g = int * int * int

    let viesInit = 3
    let lvlInit = 1

    let cons balles blocs bonus raquette param = (balles, blocs, bonus, raquette, param)

    let balles (b, blocs, bonus, raquette, param) = b
    let blocs (balles, b, bonus, raquette, param) = b
    let bonus (balles, blocs, b, raquette, param) = b
    let raquette (balles, blocs, bonus, r, param) = r
    let param (balles, blocs, bonus, raquette, p) = p
    let score jeu = let (s, v, n) = param jeu in s
    let vies jeu = let (s, v, n) = param jeu in v
    let niveau jeu = let (s, v, n) = param jeu in n

    let ajouterBalle jeu b = 
      cons (b::(balles jeu)) (blocs jeu) (bonus jeu) (raquette jeu) (param jeu)
    let ajouterBloc jeu b = 
      cons (balles jeu) (b::(blocs jeu)) (bonus jeu) (raquette jeu) (param jeu)
    let ajouterBonus jeu b = 
      cons (balles jeu) (blocs jeu) (b::(bonus jeu)) (raquette jeu) (param jeu)
    let remplRaquette jeu r = 
      cons (balles jeu) (blocs jeu) (bonus jeu) r (param jeu)

    let init = ajouterBalle (cons [] (Bloc.genererNiveau 1) [] Raquette.init (0, viesInit, 1)) Balle.init
    
    let niveauTermine jeu = List.length (blocs jeu) = 0
    let passerNiveau jeu = 
      let params = (score jeu, vies jeu, 1 + niveau jeu) in
      let niveau = Bloc.genererNiveau (1 + niveau jeu) in
      ajouterBalle (cons [] niveau [] Raquette.init params) Balle.init


  end




(*
---------------------------
Modules fournis par le tp7
---------------------------
*)


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

module type Frame =
  sig
    val dt : float
    val box_x : float * float
    val box_y : float * float
  end

module Init =
  struct

    include Parametres
    
    let dt = 0.01
    let box_x = (0, long_ecran)
    let box_y = (0, haut_ecran)

  end



(*
--------------------------
Partie graphique
--------------------------
*)

module Drawing (F : Frame) =
  struct

  include Parametres

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
        | Some (jeu, r') ->
            begin
            (* Format.printf "r=(%f, %f); dr = (%f, %f)@." x y dx dy;*)
            Graphics.clear_graph ();


            (* Définition des fonctions graphiques *)
            
            let draw_raquette raq =
             Raquette.(
              set_color (couleur raq);
              let xsouris = (fst (mouse_pos ())) in
              let x = getXRaq raq xsouris in
                fill_rect (xg raq - rx raq) (yb raq) (long raq) (haut raq);
             ) 
            in

            let draw_balle balle =
              Balle.(
                set_color (couleur balle);
                draw_circle (xc balle) (yc balle) (rx balle);
              )
            in

            let draw_bonus bonus =
              Bonus.(
                set_color (couleur bonus);
                draw_circle (xc bonus) (xc bonus) (rx bonus);
              )
            in

            let draw_bloc bloc =
              Bloc.(
                set_color (couleur bloc);              
                fill_rect (xg bloc) (yb bloc) (long bloc) (haut bloc)
              )
            in

            let draw_score score couleur =
              moveto 20 20;
              set_color couleur;
              set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
              draw_string "Score : ";
              draw_string (string_of_int score);
            in

            let draw_vies vies couleur =
              
              moveto (long_ecran - 150) 20;
              set_color couleur;
              set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
              draw_string "Vies : ";
              draw_string (string_of_int vies);
            in

            let draw_game_over score vies =
              (* Background *)
              set_color black;
              moveto 0 0;
              fill_rect 0 0 (long_ecran) (haut_ecran);
              (* Texte Game over *)
              moveto ((long_ecran / 10)) ((haut_ecran / 2));
              set_color white;
              set_font "-*-fixed-medium-r-semicondensed--150-*-*-*-*-*-iso8859-1";
              draw_string "Game Over";
              (* Texte ESC et vies/score*)
              moveto (long_ecran / 3) (haut_ecran / 2);
              set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
              draw_string "Appuyez sur ESC pour sortir...";
              draw_score score white;
              draw_vies vies white;
            in 


            (* Application des fonctions graphiques *)
            
              if Jeu.vies jeu <> 0 then
                Jeu.(
                  set_color black;
                  List.iter draw_balle (balles jeu);
                  List.iter draw_bonus (bonus jeu);
                  draw_raquette (raquette jeu);
                  draw_score (score jeu) black;
                  draw_vies (vies jeu) red;
                  List.iter draw_bloc (blocs jeu);
                )
              else
                draw_game_over (Jeu.score jeu) (Jeu.vies jeu);
            

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

module type Params =
  sig
    val dt : float
    val masse0 : float
    val position0 : float * float
    val vitesse0  : float * float
  end

(* Déplacement des balles et bonus *)
module FreeFall (F : Frame) =
  struct
    let (|+|) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
    let (|*|) k (x, y) = (k *. x, k *. y)

    let integre dt flux =
      let init = (0., 0.) in
      let rec acc =
        Tick (lazy (Some (init, Flux.map2 (fun a f -> a |+| (dt |*| f)) acc flux)))
      in acc

    let run jeu =
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
