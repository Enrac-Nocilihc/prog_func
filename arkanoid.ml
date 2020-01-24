open Graphics

(* 

Petite note à propos des include (particulièrement pour Paramètres et Nombre) :

Cette manière de faire n'est peut être pas la façon la plus propre de
gérer les modules, et l'utilisation de foncteurs aurait peut être été 
plus judicieuse, mais ce type de notation permet une écriture plus
compacte et lisible. Le projet n'ayant pas une envergure colossale, 
nous avons fait ce choix.

En revanche, nous jugeons que les includes de Solide ont bien plus de sens ici.

*)


(* 
------------------------------------------
Interfaces et modules des objets utilisés 
------------------------------------------
*)

(* Solides = Blocs / Balle / Raquette *)

module type NombreItf =
  sig

  type nb
  
  (* Opérations *)
  (* ...- : Opération entre nb *)
  (* ...~ : Opération entre nb et entier *)
  (* ...~ : Opération entre nb et flottant *)
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
  

  (* Transformation de int/float à nb *)
  val toInt : int -> nb
  val toFloat : float -> nb

  (* Transformation de nb à int/float *)
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
  let ( >~ ) a b = operatorInt a b ( > )
  let ( >~. ) a b = operatorFloat a b ( > )

  let ( << ) a b = operator2 a b ( < ) ( < )
  let ( <~ ) a b = operatorInt a b ( < )
  let ( <~. ) a b = operatorFloat a b ( < )

  let ( <<= ) a b = operator2 a b ( <= ) ( <= )
  let ( <=~ ) a b = operatorInt a b ( <= )
  let ( <=~. ) a b = operatorFloat a b ( <= )

  let ( >>= ) a b = operator2 a b ( >= ) ( >= )
  let ( >=~ ) a b = operatorInt a b ( >= )
  let ( >=~. ) a b = operatorFloat a b ( >= )

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

  end


(* Paramètres généraux *)
module Parametres =
  struct

    let long_ecran = Nombre.(toInt 800)
    let haut_ecran = Nombre.(toInt 600)
    let zone_blocs = Nombre.((
      (long_ecran /~ 8, (long_ecran *~ 7) /~ 8), (* xmin, xmax *)
      (haut_ecran /~ 2, (haut_ecran *~ 7) /~ 8)) (* ymin, ymax *)
    )

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

    (* Retourne la position du centre du solide *)
    val posC : (_, nb) t -> nb * nb
    
    (* Retourne la position Bas/Droite du solide *)
    val posBG : (_, nb) t -> nb * nb

    val setPosC : ('a, nb) t -> (nb * nb) -> ('a, nb) t
    val setPosBG : ('a, nb) t -> (nb * nb) -> ('a, nb) t

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
    
    let rx s = (long s) /~ 2
    let ry s = (haut s) /~. 2.

    let setPosC (pos, l, h, color, elt) (x,y) = let s = (pos, l, h, color, elt) in 
      cons (x -- rx s, y -- ry s) l h color elt (* centre *)
    let setPosBG (pos, l, h, color, elt) newPos = cons newPos l h color elt (* bas droite *)
    let setParam (pos, l, h, color, elt) param = cons pos l h color param
    let setCouleur (pos, l, h, color, elt) newC = cons pos l h newC elt
    let setLong (pos, l, h, color, elt) longueur = cons pos longueur h color elt


    let xc s = xg s +- rx s
    let yc s = yb s +- ry s
    let yh s = yb s +- haut s
    let xd s = xg s +- long s

    let posC s = (xc s, yc s)
    let posBG s = (xg s, yb s)

    
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

    (* Score de base d'un bloc pouvant être détruit en un coup. (En considérant vie = 1) *)
    val scoreBase : int

    (* Nombre de coups nécessaires avant de détruire le bloc. Ce nombre est négatif s'il est invulnérable. *)
    val power : (tb, nb) t -> int

    (* Score du bloc fourni en argument à la destruction *)
    val score : (tb, nb) t -> int

    (* Diminue de 1 la puissance d'un bloc *)
    val downgrade : (tb, nb) t -> (tb, nb) t

    (* Génère une ligne de blocs d'une certaine puissance et d'une certaine taille *)
    val genererLigne : nb -> nb -> nb -> int -> int -> nb -> (tb, nb) t list

    (* Génère une ligne de blocs d'une certaine puissance et d'une certaine taille *)
    val genererNiveau : int -> (tb, nb) t list

    (* Redéfinition de la couleur d'un bloc *)
    val couleur : (tb, nb) t -> color

    (* Dessine un bloc *)
    val draw : (tb, nb) t -> unit

    

  end

module Bloc : BlocItf with type nb = Nombre.nb  =
  struct

    include Solide
    include Parametres

    (* Représente la puissance du bloc / Score à la destruction *)
    type tb = int * int

    let fun_evo_lignes_nb = fun niveau hauteur -> (7 + 2 * niveau)
    let fun_evo_lignes_pw = fun niveau hauteur -> (niveau + ((niveau * hauteur / 3 )) )
    let fun_nb_lignes = fun niveau -> 8
    let propInvincibles = 0.1

    let power_to_couleur power = 
      if power < 0 then white
      else
        let nbRot = 15. in
        let p = float_of_int (power mod int_of_float nbRot) in
        let prop = 3. *. p /. nbRot -. float_of_int( int_of_float (3. *. p /. nbRot)) in
        let (r,g,b) = 
          if p = 0. then (0., 1., 0.)
          else if p = nbRot /. 3. then (0., 0., 1.)
          else if p = 2. *. nbRot /. 3. then (1., 0., 0.)
          else if p < nbRot /. 3. then (0., cos (prop *. 3.1415 /. 2.), sin (prop *. 3.1415 /. 2.)) 
          else if  p > 2. *. nbRot /. 3. then (cos (prop *. 3.1415 /. 2.), sin (prop *. 3.1415 /. 2.), 0.) 
          else (sin (prop *. 3.1415 /. 2.), 0., cos (prop *. 3.1415 /. 2.)) in
        rgb (int_of_float (255. *. r)) (int_of_float  (255. *. g)) (int_of_float  (255. *. b))

    let scoreBase = 20

    let espacement = 3

    let calcScore power = (power * scoreBase)

    let cons (x,y) long haut power = Solide.cons (x,y) long haut (power_to_couleur power) (power, (calcScore power))
    
    let power bloc = fst (param bloc)

    let score bloc = snd (param bloc)

    let downgrade bloc = setParam bloc ((power bloc) - 1, score bloc)


    let genererLigne y xmin xmax nbBlocs power h = 
      let e = espacement in
      let l = ((xmax -- xmin) /~. float_of_int nbBlocs) -~ e
      in
        let rec ajouterBloc x nbBlocs power =
          let realPower = (
            let randf = Random.float 1. in 
            if randf < propInvincibles then -1 else power) in
          let bloc = cons (x,y) l h realPower in
            if nbBlocs = 0 then []
            else if nbBlocs < 0 then raise (Failure "Oops !")
            else bloc::(ajouterBloc (x +- l +~ e) (nbBlocs - 1) power)
        in 
        ajouterBloc xmin nbBlocs power 
    
    
    let genererNiveau n =

      let ((xmin, xmax), (ymin, ymax)) = zone_blocs in
        let nbLignes = fun_nb_lignes n in
        let e = espacement in
          let h = ((ymax -- ymin) /~ nbLignes) -~ e in
            let rec ajouterLigne y nbL =
              if nbL = 0 then []
              else let ligne = genererLigne y xmin xmax (fun_evo_lignes_nb n nbL) (fun_evo_lignes_pw n nbL) h in
              ligne::(ajouterLigne (y -- h -~ e) (nbL - 1))
            in 
              List.flatten (ajouterLigne ymax nbLignes)

    let couleur bloc = power_to_couleur (power bloc)
    let draw bloc =
        set_color (couleur bloc);              
        fill_rect (intv(xg bloc)) (intv(yb bloc)) (intv(long bloc)) (intv(haut bloc));
        if power bloc < 0 then (
          set_color black;              
          fill_rect (intv(xg bloc) + espacement) (intv(yb bloc) + espacement) (intv(long bloc) - espacement * 2) (intv(haut bloc) -  espacement * 2);
        )
  end




(* Raquette *)
module type RaquetteItf =
  sig
    include SolideItf
    type tr

    (* Initialise la raquette *)
    val init : (tr, nb) t

    (* Renvie true si la raquette est dans le bon sens, false sinon *)
    val sens : (tr, nb) t -> bool

    (* Change le sens de la raquette *)
    val changeSens : (tr, nb) t -> (tr, nb) t

    (* Agrandit (ou rétrécit) la raquette *)
    val ajouterTaille : (tr, nb) t -> int -> (tr, nb) t

    (* Redéfinition des coordonnées en fonction de la souris *)
    val xg : (tr, nb) t -> nb 
    val xd : (tr, nb) t -> nb 
    val xc : (tr, nb) t -> nb 

    (* Dessine la raquette *)
    val draw : (tr, nb) t -> unit 
    
  end

module Raquette : RaquetteItf with type nb = Nombre.nb =
  struct

    include Solide
    include Parametres

    type tr = bool
    
    let haut_raq = toInt 20
    let long_raq = toInt 120
    let y_raq = toInt 100
    let color_raq = red

    let init = cons ((long_ecran /~ 2), y_raq) long_raq haut_raq color_raq true
    let sens raq = param raq
    let changeSens raq = setParam raq (not (param raq))
    let ajouterTaille raq taille = setLong raq (toInt(max 0 (intv (long raq) + taille)))

    let xg raq = 
      let x = toInt (fst (mouse_pos ()))  in
        (let new_x =
          if x >> long_ecran -- rx raq then long_ecran -- long raq
          else if x << rx raq          then toFloat 0.
          else x -- rx raq
        in
          if sens raq then new_x else long_ecran -- long raq -- new_x)
    
    let xc s = xg s +- rx s
    let xd s = xg s +- long s

    let draw raq = (
      set_color (couleur raq);
      fill_rect (intv (xg raq)) (intv (yb raq)) (intv (long raq)) (intv (haut raq));
      set_color black;
      draw_rect (intv (xg raq)) (intv (yb raq)) (intv (long raq)) (intv (haut raq));)

  end


(* Balle *)
module type BalleItf =
  sig

    include SolideItf

    type tba

    val init : (tba, nb) t

    (* Retourne true si la balle fournie en argument est en collision avec la raquette fournie en argument *)
    val collision_raq : (tba, nb) t -> (Raquette.tr, nb) Raquette.t -> bool
    
    val collision_bloc : (tba, nb) t -> (Bloc.tb, nb) Bloc.t -> bool * bool * bool * bool
    val collision_blocs : (tba, nb) t -> (Bloc.tb, nb) Bloc.t list -> bool -> bool
    val collision_xy_blocs : (tba, nb) t -> (Bloc.tb, nb) Bloc.t list  -> bool -> bool * bool

    val collision_effective : (tba, nb) t -> (Bloc.tb, nb) Bloc.t -> bool

    (* Retourne true si la balle fournie en argument est en collision 
    (sur l'axe précisé, si précisé) avec le(s) bloc(s) fourni(s) en argument 
    
    val collision_x_blocs_effective : (tba, nb) t -> (Bloc.tb, nb) Bloc.t list -> bool
    
    val collision_bloc_effective : (tba, nb) t -> (Bloc.tb, nb) Bloc.t -> bool (* Debug d'effet ping pong en 1 frame + Equilibre l'effet traverse de la balle *)
    val collision_blocs : (tba, nb) t -> (Bloc.tb, nb) Bloc.t list -> bool*)
    
    (* Duplique la première balle de la liste, et change l'orientation de sa vitesse *)
    val dupliquer : (tba, nb) t list -> (tba, nb) t list

    (* Opérations sur la vitesse de la balle *)
    val vit : (tba, nb) t -> (nb * nb)
    val setVit : (tba, nb) t -> (nb * nb) -> (tba, nb) t 

    val dx : (tba, nb) t -> nb
    val dy : (tba, nb) t -> nb

    (* Pouvoir de la balle à traverser les blocs, sauf indestructibles*)
    val traverse : (tba, nb) t -> int
    val setTraverse : (tba, nb) t -> int -> (tba, nb) t 

    val blocsTouches : (tba, nb) t -> (Bloc.tb, nb) Bloc.t list
    val setBlocsTouches : (tba, nb) t -> (Bloc.tb, nb) Bloc.t list -> (tba, nb) t 

    (* Pivote le vecteur vitesse de la balle d'un certain angle *)
    val pivote : (tba, nb) t -> float -> (tba, nb) t

    (* Dessine la balle fournie *)
    val draw : (tba, nb) t -> unit

  end

module Balle : BalleItf with type nb = Nombre.nb =
  struct

    include Solide

    (* Vitesse / Rebonds avant fin de traversabilité / Liste blocs touchés instant t-1 *)
    type tba = (nb * nb) * int * (Bloc.tb, nb) Bloc.t list

    let posInit = (toInt 400, toInt 200)
    let diametre = toInt 12
    let vitesseInit = (toFloat 200., toFloat 200.)

    let init = cons posInit diametre diametre white (vitesseInit , 0, [])

    let vit balle = let (v, pw, _) = param balle in v
    let setVit balle vit = let (v, pw, b) = param balle in setParam balle (vit, pw, b)
    let traverse balle = let (v, pw, _) = param balle in pw
    let setTraverse balle pow = let (v, pw, b) = param balle in setParam balle (v, pow, b)
    let blocsTouches balle = let (v, pw, b) = param balle in b
    let setBlocsTouches balle bl = let (v, pw, b) = param balle in setParam balle (v, pw, bl)
    

    let dx balle = fst (vit balle)
    let dy balle = snd (vit balle)

    let collision_raq balle raq = 
        dy balle <~. 0. &&
        Raquette.yh raq >> yb balle &&
        Raquette.yb raq << yb balle && 
        Raquette.xg raq << xc balle && 
        Raquette.xd raq >> xc balle

    let collision_effective balle bloc = 
      (List.fold_right (fun t qt -> qt && (intv(Bloc.xc bloc) != intv(Bloc.xc t) || intv(Bloc.yc bloc) != intv(Bloc.yc t)) ) (blocsTouches balle) true)


    let collision_bloc balle bloc =
      let distance (x1,y1) (x2,y2) = 
        toFloat((floatv(x2 -- x1) ** 2. +. floatv(y2 -- y1) ** 2.) ** 0.5)
      in
      let conda = 
        yc balle << Bloc.yh bloc &&
        yh balle >> Bloc.yb bloc &&
        xg balle << Bloc.xd bloc &&
        xc balle >> Bloc.xg bloc &&
        not (
          distance (posC balle) (Bloc.xd bloc, Bloc.yb bloc) >> rx balle && 
          xc balle >> Bloc.xd bloc &&
          yc balle << Bloc.yb bloc)
      in
      let condb = 
        yc balle << Bloc.yh bloc &&
        yh balle >> Bloc.yb bloc &&
        xc balle << Bloc.xd bloc &&
        xd balle >> Bloc.xg bloc &&
        not (
          distance (posC balle) (Bloc.xg bloc, Bloc.yb bloc) >> rx balle && 
          xc balle << Bloc.xg bloc &&
          yc balle << Bloc.yb bloc)
      in
      let condc = 
        yb balle << Bloc.yh bloc &&
        yc balle >> Bloc.yb bloc &&
        xg balle << Bloc.xd bloc &&
        xc balle >> Bloc.xg bloc &&
        not (
          distance (posC balle) (Bloc.xd bloc, Bloc.yh bloc) >> rx balle && 
          xc balle >> Bloc.xd bloc &&
          yc balle >> Bloc.yh bloc)
      in
      let condd = 
        yb balle << Bloc.yh bloc &&
        yc balle >> Bloc.yb bloc &&
        xc balle << Bloc.xd bloc &&
        xd balle >> Bloc.xg bloc &&
        not (
          distance (posC balle) (Bloc.xg bloc, Bloc.yh bloc) >> rx balle && 
          xc balle << Bloc.xg bloc &&
          yc balle >> Bloc.yh bloc)
      in
      (conda, condb, condc, condd)

    let emplacementsCollisions balle blocs effectivite =
      
      
      List.fold_right (
      fun t (a0, b0, c0, d0, l) -> let eff = collision_effective balle t in
      let (a1, b1, c1, d1) = collision_bloc balle t in 
        let (a,b,c,d) = 
        (a0 || (a1 && (eff || not effectivite)),
        b0 || (b1 && (eff || not effectivite)),
        c0 || (c1 && (eff || not effectivite)),
        d0 || (d1 && (eff || not effectivite))) in
        (a,b,c,d,(if a || b || c || d then t::l else l))) blocs (false, false, false, false, [])

    let collision_xy_blocs balle blocs effectivite =
      let (a, b, c, d, _) = emplacementsCollisions balle blocs effectivite in
      let ex = dx balle >~ 0 in
      let ey = dy balle >~ 0 in
      let coll_x = (c && not d && not ex) || (not c && d && ex) || (a && not b && not ex) || (not a && b && ex) in
      let coll_y = (d && not b && not ey) || (not d && b && ey) || (c && not a && not ey) || (not c && a && ey) in
      (coll_x, coll_y)
    let collision_blocs balle blocs effectivte =
      let (a,b,c,d, _) = emplacementsCollisions balle blocs effectivte in
      a || b || c || d 

    let dupliquer listeBalles = 
      let prem = List.hd listeBalles in
        let (vx, vy) = vit prem in
          (setVit prem (vx *~ (-1), vy))::listeBalles

    let pivote balle theta =

      setVit balle (let (vx, vy) = vit balle in

      let norme vx vy = sqrt ((vx ** 2.) +. (vy ** 2.)) in
      let sign x = if x < 0. then -.1. else if x > 0. then 1. else 0. in

      let (new_vx, new_vy) = (vx *~. cos theta +- vy *~. sin theta,
      vx *~. -. sin theta +- vy *~. cos theta) in
      let n = norme (floatv new_vx) (floatv new_vy) in 
      let a = 3.1415/.8. in
        if (abs_float(floatv new_vx)) /. n > cos a then 
          (toFloat (sign (floatv new_vx) *. n *. cos a), 
           toFloat (sign (floatv vy) *. n *. abs_float (sin a)))
        else (new_vx, new_vy))

    
    let draw balle =(
        (if traverse balle > 0 then (
          set_color yellow; 
          fill_circle (intv (xc balle)) (intv (yc balle)) (intv(rx balle) + 2);
          set_color red;) else (set_color (couleur balle);));
        fill_circle (intv (xc balle)) (intv (yc balle)) (intv(rx balle));)


  end


(* Bonus *)
module type BonusItf =
  sig

    include SolideItf 
    

    type 'a evo = 'a -> 'a
    
    type bonusType = 
      | BonusRaq of (Raquette.tr, nb) Raquette.t evo
      | BonusBal of (Balle.tba, nb) Balle.t list evo
      | BonusVie of int evo
      | BonusScore of int evo

    type tbo

    (* Fonction augmentant la taille de la raquette *)
    val tailleRaquetteUp : bonusType

    (* Fonction diminuant la taille de la raquette *)
    val tailleRaquetteDown : bonusType

    (* Fonction inversant le sens de la raquette *)
    val tailleRaquetteInv : bonusType

    (* Fonction ajoutant une balle à l liste de balle fournie en argument *)
    val multiballes : bonusType

    (* Fonction ajoutant une vie supplémentaire *)
    val vieSupp : bonusType

    (* Fonction initialisant un bonus en mouvement *)
    val init : (bonusType * color * float) -> (nb * nb) -> (nb * nb) -> (tbo, nb) t

    (* Paramètres de vitesse *)
    val vit : (tbo, nb) t -> (nb * nb)
    val setVit : (tbo, nb) t -> (nb * nb) -> (tbo, nb) t

    (* Génère (aléatoirement) un bonus tombant de type aléatoire *)
    val genererAlea : (Bloc.tb, nb) Bloc.t -> (tbo, nb) t option

    (* Liste des bonus, avec leurs couleur à la chute, et leur probabilité relative *)
    val listeBonus : (bonusType * color * float) list

    (* Retourne true si le bonus est en collision avec la raquette *)
    val collisionRaquette : (tbo, nb) t -> (Raquette.tr, nb) Raquette.t -> bool

    (* Retourne true si un des bonus de la liste est en collision avec la raquette *)
    val collisionsRaquette : (tbo, nb) t list -> (Raquette.tr, nb) Raquette.t -> bool

    (* Récupère la fonction modificatrice d'un bonus *)
    val func : (tbo, nb) t -> bonusType

    (* Dessine le bonus fourni *)
    val draw : (tbo, nb) t -> unit
    
  end


module Bonus : BonusItf with type nb = Nombre.nb =
  struct

    include Solide

    (* Evolution de l'objet / Vitesse *)

    type 'a evo = ('a -> 'a) 

    type bonusType = 
      | BonusRaq of (Raquette.tr, nb) Raquette.t evo
      | BonusBal of (Balle.tba, nb) Balle.t list evo
      | BonusVie of int evo
      | BonusScore of int evo
    
    (* Type de bonus / Vitesse / Probabilité relative *)
    type tbo = bonusType * (nb * nb) * float

    (* Bonus disponibles *)
    let tailleRaquetteUp = BonusRaq(fun raq -> Raquette.ajouterTaille raq 20)
    let tailleRaquetteDown = BonusRaq(fun raq -> Raquette.ajouterTaille raq (-20))
    let tailleRaquetteInv = BonusRaq(Raquette.changeSens)
    let vieSupp = BonusVie(fun vies -> vies + 1)
    let multiballes = BonusBal(fun listeBalles -> Balle.dupliquer listeBalles)
    let acceleration = BonusBal(fun listeBalles -> List.map( fun balle -> Balle.(setVit balle (dx balle *~. 1.2, dy balle *~. 1.2) )) listeBalles)
    let deceleration = BonusBal(fun listeBalles -> List.map( fun balle -> Balle.(setVit balle (dx balle *~. 0.95, dy balle *~. 0.95) )) listeBalles)
    let ballesTraversantes = BonusBal(fun listeBalles -> List.map (fun balle -> Balle.setTraverse balle 10) listeBalles)
    let gainPoints = BonusScore(fun score -> int_of_float (float_of_int score *. 1.1))

    let vitesseInit = (toFloat 0., toFloat (-.100.))
    let rayon = toInt 10
    let probaApparition = 0.33

    let init (func, color, probaRel) pos vit = cons pos (rayon *~ 2) (rayon *~ 2) color (func, vit, probaRel)
    
    let listeBonus = [
      (tailleRaquetteUp, green, 2.);
      (tailleRaquetteDown, rgb 150 0 0, 2.);
      (tailleRaquetteInv, magenta, 3.);
      (vieSupp, red, 1.);
      (gainPoints, yellow, 2.);
      (multiballes, white, 2.);
      (acceleration, white, 2.);
      (deceleration, white, 2.);
      (ballesTraversantes, cyan, 1.);
    ]

    let poidsProbas = List.fold_right (fun t qt -> let (_, _, p) = t in p +. qt) listeBonus 0.
    let proba bonus = let (f, v, p) = param bonus in p /. poidsProbas
    

    let vit bonus = let (f, v, _) = param bonus in v
    let setVit bonus newV = let (f, v, p) = param bonus in setParam bonus (f, newV, p)

    let genererAlea bloc = 
        if Random.float 1. < probaApparition 
        then let randBon = Random.float 1. in
          snd (List.fold_right (
            fun t (pcalc, bon) -> 
              match bon with
              | Some _ -> (pcalc, bon)
              | None ->
                let b = init t (Bloc.posC bloc) vitesseInit in
                let p = pcalc +. proba b in 
                  if p <= randBon then (p, None)
                  else (p, Some(b))
              ) listeBonus (0., None))
        else None
    
    let collisionRaquette bonus raq =
      Raquette.yh raq >> yb bonus && 
      Raquette.yb raq << yb bonus && 
      Raquette.xg raq << xc bonus && 
      Raquette.xd raq >> xc bonus

    let collisionsRaquette listeBonus raq =
      List.fold_right (fun t qt -> qt || collisionRaquette t raq) listeBonus false

    let func bonus = let (f, v, p) = param bonus in f
    

    let draw bonus =
      set_color (couleur bonus);
      set_line_width 2;
      draw_circle (intv(xc bonus)) (intv(yc bonus)) (intv(rx bonus));
      set_line_width 1;

  end

(* Jeu *)
module type JeuItf =
  sig
  
    include NombreItf

    type g
    type ('a, 'b) t
  
    (* Construit un jeu *)
    val cons : (Balle.tba, nb) Balle.t list -> (Bloc.tb, nb) Bloc.t list -> (Bonus.tbo, nb) Bonus.t  list -> (Raquette.tr, nb) Raquette.t -> g -> (g, 'b) t
    
    (* Getters *)
    val balles : (g, 'b)t -> (Balle.tba, nb) Balle.t list
    val blocs : (g, 'b) t -> (Bloc.tb, nb) Bloc.t list
    val bonus : (g, 'b) t ->  (Bonus.tbo, nb) Bonus.t list
    val raquette : (g, 'b) t -> (Raquette.tr, nb) Raquette.t
    val param : (g, 'b) t -> g

    val score : (g, 'b) t -> int  
    val vies : (g, 'b) t -> int 
    val niveau : (g, 'b) t -> int 

    (* Ajout d'éléments *)
    val ajouterBalle : (g, 'b) t -> (Balle.tba, nb) Balle.t -> (g, 'b) t
    val ajouterBloc : (g, 'b) t -> (Bloc.tb, nb) Bloc.t -> (g, 'b) t
    val ajouterBonus : (g, 'b) t -> (Bonus.tbo, nb) Bonus.t  -> (g, 'b) t
    
    (* Setters *)
    val remplRaquette : (g, 'b) t -> (Raquette.tr, nb) Raquette.t -> (g, 'b) t
    val remplBalles : (g, 'b) t -> (Balle.tba, nb) Balle.t list -> (g, 'b) t
    val remplBonus : (g, 'b) t -> (Bonus.tbo, nb) Bonus.t  list -> (g, 'b) t
    val remplMobiles : (g, 'b) t -> (Balle.tba, nb) Balle.t list -> (Bonus.tbo, nb) Bonus.t list -> (g, 'b) t
    val remplBlocs : (g, 'b) t -> (Bloc.tb, nb) Bloc.t list -> (g, 'b) t
    
    val setScore : (g, 'b) t -> int -> (g, 'b) t 
    val setVies : (g, 'b) t -> int -> (g, 'b) t 
    val perdreVie : (g, 'b) t -> (g, 'b) t 

    (* Renvoie le jeu correspondant au niveau 1 *)
    val init : (g, 'b) t
    
    (* Renvoie true si le niveau est détecté comme étant terminé, false sinon *)
    val niveauTermine : (g, 'b) t -> bool

    (* Passe au niveau suivant *)
    val passerNiveau : (g, 'b) t -> (g, 'b) t

    (* Vérifie si une balle a eu une collision avec un élément du jeu sur l'axe x/y *)
    val collision_xy : (g, 'b) t -> (Balle.tba, nb) Balle.t -> (nb * nb) -> (nb * nb) -> bool -> bool * bool

    (* Vérifie si la balle est en collision avec autre chose que les blocs *)
    val collision_autre : (g, 'b) t -> (Balle.tba, nb) Balle.t -> (nb * nb) -> (nb * nb) -> bool * bool

    (* Applique les bonus touchés par la raquette *)
    val appliquerBonusTouches : (g, 'b) t -> (g, 'b) t 

    (* Vérifie si un évènement s'est produit (collision bonus, balle, ...) *)
    val contact : (nb * nb) -> (nb * nb) -> (g, 'b) t -> bool
  
  end

module Jeu : JeuItf with type nb = Nombre.nb =
  struct

    include Nombre

    (* Balles / Blocs / Bonus Tombants / Paramètres généraux*)
    type ('a, 'b) t = (Balle.tba, nb) Balle.t list * (Bloc.tb, nb) Bloc.t list * (Bonus.tbo, nb) Bonus.t  list * (Raquette.tr, nb) Raquette.t * 'a
    (* Type des paramètres généraux du jeu : Score / Vies / Niveau en cours *)
    type g = int * int * int

    let viesInit = 3
    let lvlInit = 1
    let scoreInit = 0

    let cons balles blocs bonus raquette param = (balles, blocs, bonus, raquette, param)

    let balles (b, blocs, bonus, raquette, param) = b
    let blocs (balles, b, bonus, raquette, param) = b
    let bonus (balles, blocs, b, raquette, param) = b
    let raquette (balles, blocs, bonus, r, param) = r
    let param (balles, blocs, bonus, raquette, p) = p
    let score jeu = let (s, v, n) = param jeu in s
    let vies jeu = let (s, v, n) = param jeu in v
    let niveau jeu = let (s, v, n) = param jeu in n


    let remplBalles jeu b = 
      cons b (blocs jeu) (bonus jeu) (raquette jeu) (param jeu)
    let remplBonus jeu b = 
      cons (balles jeu) (blocs jeu) b (raquette jeu) (param jeu)
    let remplRaquette jeu r = 
      cons (balles jeu) (blocs jeu) (bonus jeu) r (param jeu)
    let remplParam jeu p = 
      cons (balles jeu) (blocs jeu) (bonus jeu) (raquette jeu) p
    let remplMobiles jeu balles bonus = 
      remplBalles (remplBonus jeu bonus) balles
    let remplBlocs jeu b = 
      cons (balles jeu) b (bonus jeu) (raquette jeu) (param jeu)
    let ajouterBalle jeu b = 
      cons (b::(balles jeu)) (blocs jeu) (bonus jeu) (raquette jeu) (param jeu)
    let ajouterBloc jeu b = 
      cons (balles jeu) (b::(blocs jeu)) (bonus jeu) (raquette jeu) (param jeu)
    let ajouterBonus jeu b = 
      cons (balles jeu) (blocs jeu) (b::(bonus jeu)) (raquette jeu) (param jeu)

    let setScore jeu sc = let (s, v, n) = param jeu in remplParam jeu (sc, v, n)
    let setVies jeu vies = let (s, v, n) = param jeu in  remplParam jeu (s, vies, n)
    let perdreVie jeu = let jeuEvo = setVies jeu (vies jeu - 1) in
      if vies jeu = 0 then remplBalles jeuEvo [] else remplRaquette (remplBonus (ajouterBalle jeuEvo (Balle.init)) []) Raquette.init

    let init = ajouterBalle (cons [] (Bloc.genererNiveau lvlInit) [] Raquette.init (scoreInit, viesInit, lvlInit)) Balle.init
    
    let niveauTermine jeu = List.fold_right(fun t qt -> (Bloc.power t) < 0 && qt) (blocs jeu) true
    let passerNiveau jeu = 
      let params = (score jeu, vies jeu, 1 + niveau jeu) in
      let niveau = Bloc.genererNiveau (1 + niveau jeu) in
      ajouterBalle (cons [] niveau [] Raquette.init params) Balle.init

    let collision_autre jeu balle (infx, supx) (infy, supy) =
      (
        (Balle.xg balle <<= infx && Balle.dx balle <~. 0.) || 
        (Balle.xd balle >>= supx && Balle.dx balle >~. 0.) ,
        (Balle.yh balle >>= supy && Balle.dy balle >~. 0.) ||
        (Balle.collision_raq balle (raquette jeu))
      )
    let collision_xy jeu balle (infx, supx) (infy, supy) effectivite = 
      let (xbloc, ybloc) = Balle.collision_xy_blocs balle (blocs jeu) effectivite in
      let (xautre, yautre) = collision_autre jeu balle (infx, supx) (infy, supy) in
      ( xautre || xbloc, yautre || ybloc )
      
    let collisionQlq jeu balle (xmin, xmax) (ymin, ymax) effectivite =
      let (xcol, ycol) = collision_xy jeu balle (xmin, xmax) (ymin, ymax) effectivite in
      vies jeu > 0 && (
      xcol || ycol || (Balle.yh balle <<= ymin && Balle.dy balle <~. 0.) ||
      Bonus.collisionsRaquette (bonus jeu) (raquette jeu))

    let appliquerBonusTouches jeu =

        let appliquer j bonus =
          let func = Bonus.func bonus in
          match func with
          | BonusRaq f -> remplRaquette j (f (raquette jeu))
          | BonusVie f -> setVies j (f (vies jeu))
          | BonusBal f -> remplBalles j (f (balles jeu)) 
          | BonusScore f -> setScore j (f (score jeu)) 
        in
        List.fold_right (fun t qt -> if Bonus.collisionRaquette t (raquette qt) then appliquer qt t else ajouterBonus qt t) (bonus jeu) (remplBonus jeu []) 
  

    let contact limx limy jeu =
      List.fold_right(fun t qt -> collisionQlq jeu t limx limy true || qt) (balles jeu) false


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
    val dt : Nombre.nb
    val box_x : Nombre.nb * Nombre.nb
    val box_y : Nombre.nb * Nombre.nb
    val xmin : Nombre.nb
    val ymin : Nombre.nb
    val xmax : Nombre.nb
    val ymax : Nombre.nb

  end

module Init : Frame =
  struct

    include Parametres
    
    let dt = Nombre.toFloat 0.01
    let xmin = Nombre.toFloat 0.
    let xmax = long_ecran
    let ymin = Nombre.toFloat 0.
    let ymax = haut_ecran
    let box_x = (xmin, xmax)
    let box_y = (ymin, ymax)

  end




module Drawing (F : Frame) =
  struct

  include Parametres

  let draw r = Nombre.(

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

            (* Background *)
            set_color black;
            moveto 0 0;
            fill_rect 0 0 (intv long_ecran) (intv haut_ecran);

            (* Définition des fonctions graphiques *)

            let draw_score score couleur =
              moveto 20 20;
              set_color couleur;
              draw_string "Score : ";
              draw_string (string_of_int score);
            in

            let draw_vies vies couleur =
              
              moveto (intv long_ecran - 150) 20;
              set_color couleur;
              draw_string "Vies : ";
              draw_string (string_of_int vies);
            in


            let draw_niveau niveau couleur =
              
              moveto 20 50;
              set_color couleur;
              draw_string "NIVEAU ";
              draw_string (string_of_int niveau);
            in

            let draw_game_over score vies =
              (* Texte Game over *)
              moveto (intv (long_ecran /~. 10.)) (intv (haut_ecran /~. 2.));
              set_color white;
              set_font "-*-fixed-medium-r-semicondensed--150-*-*-*-*-*-iso8859-1";
              draw_string "Game Over";
              (* Texte ESC et vies/score*)
              moveto (intv (long_ecran /~. 3.2)) (intv (haut_ecran /~. 2.2));
              set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
              draw_string "Appuyez sur ESC pour sortir...";
              draw_score score white;
              draw_vies vies white;
            in 


            (* Application des fonctions graphiques *)
            
              if Jeu.vies jeu <> 0 then
                Jeu.(
                  set_color black;
                  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
                  
                  List.iter Bonus.draw (bonus jeu);
                  List.iter Bloc.draw (blocs jeu);
                  List.iter Balle.draw (balles jeu);
                  Raquette.draw (raquette jeu);
                  draw_score (score jeu) white;
                  draw_vies (vies jeu) (rgb 255 150 150);
                  draw_niveau (niveau jeu) white;
                  set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
                  
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
      let size_x = intv(sup_x -- inf_x) in
      let size_y = intv(sup_y -- inf_y) in
      Graphics.open_graph (Format.sprintf " %dx%d" size_x size_y);
      Graphics.auto_synchronize false;
      Sys.(ref_handler_alrm := signal sigalrm (Signal_handle handler_alrm));
      Sys.(ref_handler_int  := signal sigint  (Signal_handle handler_int));
      Unix.(setitimer ITIMER_REAL { it_interval = floatv F.dt; it_value = floatv F.dt })
    end    
  )
  end

module type Params =
  sig
    val dt : float
    val masse0 : float
    val position0 : float * float
    val vitesse0  : float * float
  end

(* Déplacement des balles et bonus *)
module Mouvement (F : Frame) =

  struct
  
    include Nombre

    let (|+|) (x1, y1) (x2, y2) = (x1 +- x2, y1 +- y2)
    let (|*|) k (x, y) = (k *- x, k *- y)

    let integre dt flux =
      let init = (toFloat 0., toFloat 0.) in
      let rec acc =
        Tick (lazy (Some (init, Flux.map2 (fun a f -> a |+| (dt |*| f)) acc flux)))
      in acc


    let run jeu =

      let rec newBalles listeB =
        Balle.(List.fold_right (fun balle qt -> 
          let (pos0, vit0) = (posC balle, vit balle) in
            let vit1 = Flux.constant vit0 in
            let pos1 = Flux.(map2 ( |+| ) (constant pos0) (integre (F.dt) vit1)) in
              let fluxCouples = (Flux.map2 (fun p v -> (p, v)) pos1 vit1) in
                Flux.map2 (fun (pos1, vit1) l -> (setVit (setPosC balle pos1) vit1)::l) fluxCouples qt
        ) listeB (Flux.constant ([])))

      in let rec newBonus listeB =
        Bonus.(List.fold_right (fun bonus qt -> 
        let (pos0, vit0) = (posC bonus, vit bonus) in
          let vit1 = Flux.constant vit0 in
          let pos1 = Flux.(map2 ( |+| ) (constant pos0) (integre (F.dt) vit1)) in
            let fluxCouples = (Flux.map2 (fun p v -> (p, v)) pos1 vit1) in
              Flux.map2 (fun (pos1, vit1) l -> (setVit (setPosC bonus pos1) vit1)::l) fluxCouples qt
        ) listeB (Flux.constant ([])))
      in
      let balles = Jeu.balles jeu in 
      let bonus = List.fold_right (fun t qt -> if Bonus.yh t >~. 0. then t::qt else qt) (Jeu.bonus jeu) [] in
      Flux.map2 (fun balles bonus -> Jeu.remplMobiles jeu balles bonus) (newBalles balles) (newBonus bonus)
  end



module Bouncing (F : Frame) =
  struct

    include Nombre

    (* % d'augmentation de la vitesse après un rebond *)
    let acceleration = toFloat 1.005

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
    
    
    let balleBounce jeu balle blocsT =
      Balle.(
        let (xcol, ycol) = Jeu.collision_xy jeu balle F.box_x F.box_y true in
        let (xautre, yautre) = Jeu.collision_autre jeu balle F.box_x F.box_y in
      let new_dx = (if xcol && (traverse balle <= 0 || xautre) then dx balle *~ (-1) *- acceleration else dx balle) in
      let new_dy = (if ycol && (traverse balle <= 0 || yautre) then dy balle *~ (-1) *- acceleration else dy balle) in
        let newTraverse = if floatv (dx balle) != floatv (new_dx) || floatv (dy balle) != floatv (new_dy) then traverse balle - 1 else traverse balle in
        let raq = Jeu.raquette jeu in
        let dist_centre = xc balle -- Raquette.xc raq in
        let ratio_centre = (dist_centre /- (Raquette.long raq)) in
        let thetaAjoute = (if (collision_raq balle (Jeu.raquette jeu)) then floatv ratio_centre *. (3.1415 /. 2.) else 0.) in
        setBlocsTouches (setTraverse (pivote (setVit balle (new_dx, new_dy)) thetaAjoute) newTraverse) blocsT
      )
    
    let blocsBounce jeu balle =
      let blocs = Jeu.blocs jeu in 
        List.fold_right (fun t (morts, vivants, touches) -> 
          let (a,b,c,d) = Balle.collision_bloc balle t in
          if a || b || c || d then 
            if Balle.collision_effective balle t then
              (if Bloc.power t = 1 then (t::morts, vivants, t::touches) 
              else (morts, (Bloc.downgrade t)::vivants, t::touches))
            else (morts, t::vivants, t::touches)
          else (morts, t::vivants, touches)) blocs ([], [], [])


    let rebond_balle jeu balle =
      
      let (blocsMorts, blocsRestants, blocsTouches) = blocsBounce jeu balle in
      let balleSuiv = balleBounce jeu balle blocsTouches in
      let newScore = List.fold_right (fun t qt -> Bloc.score t + qt) blocsMorts (Jeu.score jeu) in
      let newBonus = List.fold_right (fun t qt -> match Bonus.genererAlea t with | None -> qt | Some b -> b::qt) blocsMorts (Jeu.bonus jeu) in
      let newJeu = Jeu.remplBlocs (Jeu.setScore (Jeu.remplBonus jeu newBonus) newScore) blocsRestants in
      (newJeu, balleSuiv)

    
    let rebond jeu =
      let newJeu = List.fold_right (fun t qt ->
          let (j, balle) = rebond_balle qt t in 
            if Balle.yb balle <~ 0 then j
            else Jeu.ajouterBalle j balle ) (Jeu.balles jeu) (Jeu.remplBalles jeu []) 
      in
        match Jeu.balles newJeu with
        | [] -> Jeu.perdreVie newJeu
        | _ -> let jeuBonus = Jeu.appliquerBonusTouches newJeu in
          if Jeu.niveauTermine jeuBonus then Jeu.passerNiveau jeuBonus else jeuBonus
    
    module MV = Mouvement (F)

    let rec run etat0 =
      unless (MV.run etat0) (Jeu.contact F.box_x F.box_y) (fun etat -> run (rebond etat))
  end


module Draw = Drawing (Init)
module Bounce = Bouncing (Init)


let _  =
  Random.init 7364;
  Draw.draw (Bounce.run (Jeu.init));; 