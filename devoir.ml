(*
	PROJET PFA : SYMBIOZ
	Auteur : Camille Vauchel

*)


let _ = Random.self_init ();;

let l1 = [0;1;2;3;4];;
let lsome = [None; Some 1; None; Some 2];;

(* Q1 *)
(* 
   insère un élément à un index donné, on suppose l'index pas trop grand
*)
let insert_at_pos e l index =
  let rec insert_at_pos e lg lres index =
    match (lres, index) with
    | (lres, 0) -> (lg @ [e]) @ lres
    | (x::xs, n) -> insert_at_pos e (lg @ [x]) xs (n-1)
    | _ -> raise (Failure "Bizarre")
  in insert_at_pos e [] l index;;

(* Q2 *)
let insert_at_random e l =
  let pos = Random.int ((List.length l)+1) in
  insert_at_pos e l pos;;

(* Q3 *)
let shuffle l =
  let rec shuffle lbon lres =
    match lres with
    | [] -> lbon
    | x::xs -> shuffle (insert_at_random x lbon) xs
  in shuffle [] l;;

(* Q4 *)
let random_get l =
  match l with
  | [] -> None
  | l -> let pos = Random.int (List.length l) in
	 Some (List.nth l pos);;

(* Q5 *)
let clean_list l =
  let rec clean_list lbon lres =
    match lres with
    | [] -> lbon
    | (None)::ls -> clean_list lbon ls
    | (Some e)::ls -> clean_list (e::lbon) ls
  in List.rev (clean_list [] l);;

(* Q6 *)
type sexe = Masculin | Feminin;;
type age = Bebe | Enfant | Adulte;;

(* Q7 *)
let random_sexe () =
  let r = Random.int 2 in
  if r == 0 then Masculin else Feminin;;

let random_age () =
  let r = Random.int 3 in
  if r == 0 then Bebe
  else if r == 1 then Enfant 
  else Adulte;;

(* Q8 *)
let string_of_sexe s =
  match s with
  | Masculin -> "Masculin"
  | Feminin -> "Feminin";;

let string_of_age a =
  match a with
  | Bebe -> "Bebe"
  | Enfant -> "Enfant"
  | Adulte -> "Adulte";;



(* Q9 *)
module type PLANETE =
  sig
    type pos;;

    val egal_pos : pos -> pos -> bool;;

    val ouest : pos -> pos;;
    val est : pos -> pos;;
    val nord : pos -> pos;;
    val sud : pos -> pos;;

    val random_pos : unit -> pos;;
    val at_pos : ('a -> pos) -> pos -> 'a list -> 'a list;;
    val sort_by_pos : ('a -> pos) -> 'a list -> 'a list list;;
    val print_pos : pos -> unit;;

    val display : (pos -> unit) -> pos -> unit;;
    val clear : unit -> unit;;
  end;;

(*************************************** SYMBIOZ *********************************************)

module Symbioz : PLANETE with type pos = int * int =
  struct
    type pos = int * int;;
    let size_x = 10 and size_y = 10;;

    let egal_pos p1 p2 = p1 = p2;;

    let ouest (x, y) = 
      if x == 1 
      then (size_x, y)
      else (x-1 , y);;

    let est (x, y) = (((x + 1) mod size_x), y);;

    let nord (x, y) = (x, ((y + 1) mod size_y));;

    let sud (x, y) =
      if y == 1
      then (x, size_y)
      else (x, y-1);;

    let random_pos () = 
      let x = (Random.int (size_x)) + 1
      and y = (Random.int (size_y)) + 1 in
      (x, y);;

    (* 
       f : fonction qui indique comment calculer la position d'un element 
       p : une position donnee
       l : liste des elements     
       retour : liste des elements a la position p
    *)
    let at_pos f p l =
      let filtre el = (f el = p)
      in List.filter filtre l;;
      
    (* 
       f : fonction qui indique comment calculer la position d'un element 
       l : liste des elements
       retour : liste de liste des elements par positions (autant de listes que de cases)
    *)
    let sort_by_pos f l = 
      let ll = ref [] in
      for i = 1 to size_x do
	  for j = 1 to size_y do
	      ll := !ll @ [(at_pos f (i,j) l)];
	  done
      done;
      !ll;;

    let print_pos (p1, p2) =
      print_string "("; print_int p1; print_string ", "; print_int p2; print_string ")";;


    let clear () = ();;
    let display f (x,y) = f (x,y);;
      
  end;;



(************************************ GENETIQUE ELEMENTAIRE  ****************************************)

module type INDIVIDU =
  sig
    type pos;;
    type individu;;

    val egal : individu -> individu -> bool;;
    val random_individu : unit -> individu;;

    val get_pos : individu -> pos;;
    val get_sexe : individu -> sexe;;
    val get_age : individu -> age;;

    val manger : int -> individu -> individu option;;
    val bouger : (pos -> int) -> individu -> individu;;
    val reproduire : int -> individu -> individu -> individu list;;
    val vieillir : individu -> individu option;;
    val afficher : individu -> unit;;
 
  end;;


module type MAKE_INDIVIDU = functor (P : PLANETE) -> INDIVIDU with type pos = P.pos;;

(****************************************** Make_Zherb *************************************************)
module Make_Zherb : MAKE_INDIVIDU = 
  functor (P : PLANETE) ->
  struct
    type pos = P.pos;;
    type individu = {id : int ref; age : age ref; pos : pos ref};;

    (* Getters / Setters *)
    let get_id ind = !(ind.id);;
    exception SexException;;
    let get_sexe ind = raise SexException;;
    let get_age ind = !(ind.age);;
    let get_pos ind = !(ind.pos);;
    let set_id ind id =
      ind.id := id;;
    let set_age ind age = 
      ind.age := age;;
    let set_pos ind pos =
      ind.pos := pos;;
      

    let last_id = ref 0;;
    let incr_last_id () =
      last_id := !last_id + 1;;

    let egal ind1 ind2 = (get_id ind1) = (get_id ind2);;

    let random_individu () = 
      let id = ref 0 in
      let age = ref (random_age ()) in
      let pos = ref (P.random_pos ()) in
      id := !last_id + 1;
      incr_last_id ();
      {id = id; age = age; pos = pos};;

    let creer_bebe pos =
      let id = ref 0 in
      let age = ref Bebe in
      let pos = ref pos in
      id := !last_id + 1;
      incr_last_id ();
      {id = id; age = age; pos = pos};;
 
   
    let manger quantite ind = Some ind;; 

    let bouger f ind = ind;;

    (* crée nbenfant dans les cases alentours, ind2 ignoré *)
    let reproduire nbenfant ind1 ind2 =
      if (get_age ind1) != Adulte then []
      else
	let pos = get_pos ind1 in
	let rec reproduire nb l =
	  match nb with
	  | 0 -> []
	  | nb -> let rand = Random.float 1. in	
		  if rand < 0.1 
		  then 
		    (creer_bebe (P.ouest pos)) :: (reproduire (nb-1) l)
		  else if rand > 0.1 && rand < 0.2 
		  then 
		    (creer_bebe (P.nord pos)) :: (reproduire (nb-1) l)			
		  else if rand > 0.2 && rand < 0.3 
		  then 
		    (creer_bebe (P.est pos)) :: (reproduire (nb-1) l)
		  else if rand > 0.3 && rand < 0.4 
		  then 
		    (creer_bebe (P.sud pos)) :: (reproduire (nb-1) l)
		  else (creer_bebe pos) :: (reproduire (nb-1) l)
	in reproduire nbenfant [];;


    let vieillir ind =
      match (get_age ind) with
	| Bebe -> (set_age ind Enfant); Some ind
	| Enfant -> (set_age ind Adulte); Some ind
	| Adulte -> None;;

    let afficher ind =
      let p = get_pos ind in
      print_string "individu de type zherb, identifiant : ";
      print_int (get_id ind);
      print_string " en position ";
      P.print_pos p;
      print_string "\n";

  end;;

(*********************************** Make_Krapit ****************************************************)
module Make_Krapit : MAKE_INDIVIDU = 
  functor (P : PLANETE) ->
  struct 
    type pos = P.pos;;
    (* tour : nombre de tour que l'individu existe 
       esperance_vie : nombre de tour que vivra le krapit à l'age adulte
       pv : nombre de point de vie de l'individu
     *)
    type individu = {id : int ref; age : age ref; sexe : sexe ref; tour : int ref;
		     esperance_vie : int ref; pos : pos ref; pv : int ref};;

    (* Getters / Setters *)
    let get_id ind = !(ind.id);;
    let get_age ind = !(ind.age);;
    let get_sexe ind = !(ind.sexe);;
    let get_tour ind = !(ind.tour);;
    let get_esperance_vie ind = !(ind.esperance_vie);;
    let get_pos ind = !(ind.pos);;
    let get_pv ind = !(ind.pv);;
    let set_id ind id =
      ind.id := id;;
    let set_age ind age = 
      ind.age := age;;
    let set_sexe ind sexe =
      ind.sexe := sexe;;
    let set_tour ind tour =
      ind.tour := tour;;
    let set_esperance_vie ind esperance_vie =
      ind.esperance_vie := esperance_vie;;
    let set_pos ind pos =
      ind.pos := pos;;
    let set_pv ind pv =
      ind.pv := pv;;

    let last_id = ref 0;;
    let incr_last_id () =
      last_id := !last_id + 1;;

    let esperance_vie_max = 4;;
    let pv_max = 6;;
    let pv_par_tour = 1;;
    let seuil_faim = 3;;
    let nb_bebe_par_portee_max = 5;;

    let egal ind1 ind2 = 
      (get_id ind1) = (get_id ind2);;

    let random_individu () = 
      let id = ref 0 in
      let age = ref (random_age ()) in
      let sexe = ref (random_sexe ()) in
      let tour = ref 0 in
      let esperance_vie = ref 0 in
      let pos = ref (P.random_pos ()) in
      let pv = ref pv_max in
      id := !last_id + 1;
      incr_last_id ();
      {id = id; age = age; sexe = sexe; tour = tour; esperance_vie = esperance_vie; pos = pos; pv = pv};;

    let creer_bebe pos =
      let id = ref 0 in
      let age = ref Bebe in
      let sexe = ref (random_sexe ()) in
      let tour = ref 0 in
      let esperance_vie = ref 0 in
      let pos = ref pos in
      let pv = ref pv_max in
      id := !last_id + 1;
      incr_last_id ();
      {id = id; age = age; sexe = sexe; tour = tour; esperance_vie = esperance_vie; pos = pos; pv = pv};;

    
    let manger quantite ind = 
      if quantite > 0 
      then 
	begin
	  set_pv ind pv_max;
	  Some ind
	end
      else 
	begin
	  set_pv ind ((get_pv ind) - pv_par_tour);
	  if (get_pv ind) = 0 
	  then None 
	  else Some ind
	end;;


    let bouger f ind =
      if ((get_pv ind) >= seuil_faim) 
      then ind
      else 
	let pos = (get_pos ind) in
	let rand = Random.float 1. in
	if rand < 0.25
	then 
	  begin
	    set_pos ind (P.ouest pos);
	    ind
	  end
	else if rand < 0.5
	then 
	  begin
	    set_pos ind (P.nord pos);
	    ind
	  end
	else if rand < 0.75
	then 
	  begin
	    set_pos ind (P.est pos);
	    ind
	  end
	else 
	  begin
	    set_pos ind (P.sud pos);
	    ind
	  end;;

    let reproduire nbenfant ind1 ind2 =
      if ((get_age ind1) != Adulte) || ((get_age ind2) != Adulte) || 
	   ((get_pos ind1) != (get_pos ind2)) || ((get_sexe ind1) = (get_sexe ind2))
      then []
      else
	let pos = get_pos ind1 in
	let nbenfant = (Random.int nb_bebe_par_portee_max) + 1 in
	let rec reproduire nb l =
	  match nb with
	  | 0 -> []
	  | nb -> let rand = Random.float 1. in	
		  if rand < 0.2 
		  then 
		    (creer_bebe (P.ouest pos)) :: (reproduire (nb-1) l)
		  else if rand < 0.4
		  then 
		    (creer_bebe (P.nord pos)) :: (reproduire (nb-1) l)			
		  else if rand < 0.6
		  then 
		    (creer_bebe (P.est pos)) :: (reproduire (nb-1) l)
		  else if rand < 0.8
		  then 
		    (creer_bebe (P.sud pos)) :: (reproduire (nb-1) l)
		  else (creer_bebe pos) :: (reproduire (nb-1) l)
	in reproduire nbenfant [];;

    let vieillir ind =
      let age = get_age ind in
      match age with
      | Bebe -> set_age ind Enfant; Some ind
      | Enfant -> set_age ind Adulte; 
		  set_esperance_vie ind ((Random.int esperance_vie_max) + 1);
		  Some ind
      | Adulte -> if (get_tour ind) >= (get_esperance_vie ind) 
		  then None 
		  else 
		    begin
		      set_tour ind ((get_tour ind) + 1);
		      Some ind
		    end;;
      

    let afficher ind =
      let p = get_pos ind in
      print_string "Krapit, identifiant : ";
      print_int (get_id ind);
      print_string " pv : ";
      print_int (get_pv ind);
      print_string " en position ";
      P.print_pos p;
      print_string "\n";;

  end;;



(****************************************** Make_Krogul *****************************************)

module Make_Krogul : MAKE_INDIVIDU =
  functor (P : PLANETE) ->
  struct
    type pos = P.pos;;
    type individu = {id : int ref; age : age ref; sexe : sexe ref; tour : int ref;
		     esperance_vie : int ref; pos : pos ref; pv : int ref};;

      
    (* Getters / Setters *)
    let get_id ind = !(ind.id);;
    let get_age ind = !(ind.age);;
    let get_sexe ind = !(ind.sexe);;
    let get_tour ind = !(ind.tour);;
    let get_esperance_vie ind = !(ind.esperance_vie);;
    let get_pos ind = !(ind.pos);;
    let get_pv ind = !(ind.pv);;
    let set_id ind id =
      ind.id := id;;
    let set_age ind age = 
      ind.age := age;;
    let set_sexe ind sexe =
      ind.sexe := sexe;;
    let set_tour ind tour =
      ind.tour := tour;;
    let set_esperance_vie ind esperance_vie =
      ind.esperance_vie := esperance_vie;;
    let set_pos ind pos =
      ind.pos := pos;;
    let set_pv ind pv =
      ind.pv := pv;;


    let last_id = ref 0;;
    let incr_last_id () =
      last_id := !last_id + 1;;

    let pv_max = 10;;
    let pv_par_tour = 2;;
    let seuil_faim = 6;;
    let nb_pv_manger = 5;;
    let seuil_deplacement = 4;;
    let nb_bebe_par_portee_max = 2;;

    let nb_tour_bebe = 2;;
    let nb_tour_enfant = 2;;
    let nb_tour_adulte_min = 2;;
    let nb_tour_adulte_max = 5;;

    let egal ind1 ind2 = 
      (get_id ind1) = (get_id ind2);;

    let random_individu () = 
      let id = ref 0 in
      let age = ref (random_age ()) in
      let sexe = ref (random_sexe ()) in
      let tour = ref 0 in
      let esperance_vie = ref 0 in
      let pos = ref (P.random_pos ()) in
      let pv = ref pv_max in
      id := !last_id + 1;
      incr_last_id ();
      {id = id; age = age; sexe = sexe; tour = tour; esperance_vie = esperance_vie; pos = pos; pv = pv};;

    let creer_bebe pos =
      let id = ref 0 in
      let age = ref Bebe in
      let sexe = ref (random_sexe ()) in
      let tour = ref 0 in
      let esperance_vie = ref 0 in
      let pos = ref pos in
      let pv = ref pv_max in
      id := !last_id + 1;
      incr_last_id ();
      {id = id; age = age; sexe = sexe; tour = tour; esperance_vie = esperance_vie; pos = pos; pv = pv};;


    let manger quantite ind =
      if quantite > 0 
      then
	begin
	  if (get_pv ind) > seuil_faim
	  then
	    Some ind
	  else
	    begin
	      let pv = ((get_pv ind) + nb_pv_manger) mod (pv_max + 1) in
	      set_pv ind pv;
	      Some ind
	    end
	end
      else
	begin
	  set_pv ind ((get_pv ind) - pv_par_tour);
	  if (get_pv ind) = 0 
	  then None 
	  else Some ind
	end;;


    (*
      Renvoie la liste des meilleures positions 
     *)
    let determiner_pos f ind =
      let pos = (get_pos ind) in
      let meilleure_pos = ref pos in
      let meilleure_val = ref (f !(meilleure_pos)) in
      let lpos = [P.ouest pos; P.nord pos; P.est pos; P.sud pos] in
      let rec determiner_pos_rec lrestante lmeilleure =
	begin
	  match lrestante with
	  | [] -> lmeilleure
	  | p::p' -> let pos_val = f p in
		     (* meilleure valeur *)
		     if pos_val > !meilleure_val
		     then 
		       begin
			 meilleure_val := pos_val;
			 determiner_pos_rec p' [p]
		       end
		     (* valeur identique *)
		     else if pos_val = !meilleure_val
		     then determiner_pos_rec p' (p::lmeilleure)
		     (* valeur moins bonne *)
		     else determiner_pos_rec p' lmeilleure
	end
      in determiner_pos_rec lpos [];; 

       
      (* 
          f : évaluation des positions de type pos -> int
       *)
    let bouger f ind =
      if (get_pv ind) > seuil_deplacement
      then ind
      else
	begin	  
	  let ltraitee = determiner_pos f ind in
	  let pos_choisie = List.nth ltraitee (Random.int (List.length ltraitee)) in 
	  set_pos ind pos_choisie;
	  ind
	end;;


    let reproduire nbenfant ind1 ind2 =
      if ((get_age ind1) != Adulte) || ((get_age ind2) != Adulte) || 
	   ((get_pos ind1) != (get_pos ind2)) || ((get_sexe ind1) = (get_sexe ind2))
      then []
      else
	let pos = get_pos ind1 in
	let nbenfant = (Random.int (nb_bebe_par_portee_max + 1)) in
	let rec reproduire nb l =
	  match nb with
	  | 0 -> []
	  | nb -> let rand = Random.float 1. in	
		  if rand < 0.05 
		  then 
		    (creer_bebe (P.ouest pos)) :: (reproduire (nb-1) l)
		  else if rand < 0.1
		  then 
		    (creer_bebe (P.nord pos)) :: (reproduire (nb-1) l)			
		  else if rand < 0.15
		  then 
		    (creer_bebe (P.est pos)) :: (reproduire (nb-1) l)
		  else if rand < 0.2
		  then 
		    (creer_bebe (P.sud pos)) :: (reproduire (nb-1) l)
		  else (creer_bebe pos) :: (reproduire (nb-1) l)
	in reproduire nbenfant [];;
      

    let incr_tour ind =
      set_tour ind ((get_tour ind) + 1);;
    let vieillir ind =
      let age = get_age ind in
      match age with
      | Bebe -> if (get_tour ind) >= nb_tour_bebe
		then
		  begin
		    set_age ind Enfant;
		    set_tour ind 0;
		    Some ind
		  end
		else
		  begin
		    incr_tour ind;
		    Some ind
		  end
		
      | Enfant -> if (get_tour ind) >= nb_tour_enfant
		  then
		    begin
		      let esperance_vie = (Random.int (nb_tour_adulte_max - nb_tour_adulte_min + 1))
					    + nb_tour_adulte_min in
		      set_age ind Adulte;
		      set_tour ind 0;
		      set_esperance_vie ind esperance_vie;
		      Some ind
		    end
		  else
		    begin
		      incr_tour ind;
		      Some ind
		    end
		      
      | Adulte -> if (get_tour ind) >= (get_esperance_vie ind) 
		  then None 
		  else 
		    begin
		      incr_tour ind;
		      Some ind
		    end;;
      

    let afficher ind =
      let p = get_pos ind in
      print_string "Krogul, identifiant : ";
      print_int (get_id ind);
      print_string " pv : ";
      print_int (get_pv ind);
      print_string " en position ";
      P.print_pos p;
      print_string "\n";;

  end;;
 

(************************************** POPULATION *********************************************)

module type POPULATION =
  sig
    type pos;;
    type individu;;
    type population;;
    type nourriture;;

 
    (* crée une population *)
    val random_population : int -> population;;   

    val get_random_individu : population -> individu option;;
    
    val sous_population : population -> pos -> population;;   
    val tuer_individu : population -> individu -> population;;
     
    val map : (individu -> 'a) -> population -> 'a list;;
    val iter : (individu -> unit) -> population -> unit;;
    val reduce : (individu -> 'a -> 'a) -> population -> 'a -> 'a;;
  
    val vieillissement : population -> population;;
    val reproduction : population -> population;;
    val mouvement : nourriture -> population -> population;;
    val nourriture : nourriture -> population -> (population * nourriture);;
    val affichage : population -> unit;;
   

  end;;


module type MAKE_PLANTES = 
  functor (P : PLANETE) ->
(*  functor (IND : INDIVIDU with type pos = P.pos) -> *)
    functor (MI : MAKE_INDIVIDU) ->
      POPULATION with type pos = P.pos and type individu = MI(P).individu and type nourriture = unit;;

module type MAKE_ANIMAUX =
  functor (P : PLANETE) ->
  functor (PROIE : POPULATION with type pos = P.pos) ->
 (* functor (IND : INDIVIDU with type pos = P.pos) -> *)
    functor (MI : MAKE_INDIVIDU) -> 
  POPULATION with type pos = P.pos and type individu = MI(P).individu and type nourriture = PROIE.population;;


(****************************************** Make_Zherbs ************************************)
module Make_Zherbs : MAKE_PLANTES =
  functor (P : PLANETE) ->
  (*  functor (IND : INDIVIDU with type pos = P.pos) ->*)
    functor (MI:MAKE_INDIVIDU) ->
  struct
    module IND = MI(P);;
    type pos = P.pos;;
    type individu = IND.individu;;
    type population = individu list;;
    type nourriture = unit;;
      
    let random_population nb_individu =
      let rec random_population n l =
	match n with
	| 0 -> l
	| n -> random_population (n-1) ((IND.random_individu ()) :: l)
      in random_population nb_individu [];;

    let get_random_individu popu = random_get popu;;
        
    let sous_population popu p = P.at_pos (IND.get_pos) p popu;;

    let tuer_individu popu ind = List.filter (fun x -> not(IND.egal x ind)) popu;;
             
    let map f pop = List.map f pop;;
    let iter f pop = List.iter f pop;;
    let reduce f pop a = List.fold_right f pop a;;

    let vieillissement popu = clean_list (map (IND.vieillir) popu);;


    (* retourne une liste de couples (individu, nb_adulte_dans_case *)
    let nb_adulte_case popu =
      let rec nb_adulte_case popu_restante l =
	match popu_restante with
	| [] -> l
	| ind :: ind' -> 
	   nb_adulte_case ind' ((ind, List.length (sous_population popu  (IND.get_pos ind))) :: l)
      in nb_adulte_case popu [];;

    (* reproduit la population 
       trie la population pour ne garder que les adultes,
       reproduit les adultes,
       retourne la population totale (ancienne population + nouveaux enfants)
     *)
    let reproduction popu =	
      let popu_adulte = 
	List.filter (fun ind -> (IND.get_age ind) = Adulte) popu in
      let liste_nb_adulte_case = nb_adulte_case popu_adulte in
      let rec reproduction liste_restante ll =
	match liste_restante with
	| [] -> ll
	| (ind, n) :: lcouple -> 
	   let liste_enfant = IND.reproduire n ind ind in
	   reproduction lcouple (ll @ liste_enfant)
      in popu @ (reproduction liste_nb_adulte_case []);;


    let mouvement nourri popu = popu;;	
    let nourriture nourri popu = (popu, nourri);;     
    let affichage popu = iter (IND.afficher) popu;;

  end;;


(**************************************** Make_Bestioles **********************************)


module Make_Bestioles : MAKE_ANIMAUX =
  functor (P : PLANETE) ->
  functor (PROIE : POPULATION with type pos = P.pos) ->
    (* functor (IND : INDIVIDU with type pos = P.pos) -> *)
    functor (MI : MAKE_INDIVIDU) ->
  struct
    module IND = MI(P);;
    type pos = P.pos;;
    type individu = IND.individu;;
    type population = individu list;;
    type nourriture = PROIE.population;;

    let random_population nb_individu =
      let rec random_population n l =
	match n with
	| 0 -> l
	| n -> random_population (n-1) ((IND.random_individu ()) :: l)
      in random_population nb_individu [];;

    let get_random_individu popu = random_get popu;;
        
    let sous_population popu p = P.at_pos (IND.get_pos) p popu;;

    let tuer_individu popu ind = 
      List.filter (fun x -> not(IND.egal x ind)) popu;;
             
    let map f pop = List.map f pop;;
    let iter f pop = List.iter f pop;;
    let reduce f pop a = List.fold_right f pop a;;

    let vieillissement popu = clean_list (map (IND.vieillir) popu);;
      
    (* let reproduction popu = ;; *)

    (* retourne une liste de couples (individu, nb_adulte_dans_case *)
    let nb_adulte_case popu =
      let rec nb_adulte_case popu_restante l =
	match popu_restante with
	| [] -> l
	| ind :: ind' -> 
	   nb_adulte_case ind' ((ind, List.length (sous_population popu  (IND.get_pos ind))) :: l)
      in nb_adulte_case popu [];;

    (* reproduit la population 
       trie la population pour ne garder que les adultes,
       reproduit les adultes,
       retourne la population totale (ancienne population + nouveaux enfants)
     *)
    let reproduction popu =	
      let popu_adulte = 
	List.filter (fun ind -> (IND.get_age ind) = Adulte) popu in
      let liste_nb_adulte_case = nb_adulte_case popu_adulte in
      let rec reproduction liste_restante ll =
	match liste_restante with
	| [] -> ll
	| (ind, n) :: lcouple -> 
	   let liste_enfant = IND.reproduire n ind ind in
	   reproduction lcouple (ll @ liste_enfant)
      in popu @ (reproduction liste_nb_adulte_case []);;


    (* Pour le moment fonction de stratégie constante *)
    let strategie pos = 1;;
    let mouvement nourri popu = map (IND.bouger strategie) popu;; 
      
    (*
    let nourriture nourri popu = (popu, nourri);;
    *)

    let nourriture nourri popu =
      let rec nutri_rec popu_restante popu_bonne nourriture_bonne =
	match popu_restante with
	| [] -> (popu_bonne, nourriture_bonne)
	| predateur :: predateur' ->
	  let nourriture_case = PROIE.sous_population nourriture_bonne (IND.get_pos predateur) in
	  let victime = PROIE.get_random_individu nourriture_case in
	  match victime with
	  (* pas de nourriture dans la case *)
	  | None -> 
	    begin
	      let mangeur = IND.manger 0 predateur in
	      match mangeur with
	      (* cas où le prédateur est mort de faim *)
	      | None -> nutri_rec predateur' popu_bonne nourriture_bonne
	      (* cas où il n'a pas mangé mais ne meurt pas de faim *)
	      | Some mangeur -> 
		let nouvelle_popu_bonne = (mangeur :: (tuer_individu popu_bonne predateur)) in
		nutri_rec predateur' nouvelle_popu_bonne nourriture_bonne
	    end
	  (* cas où il y a de la nourriture dans la case *)
	  | Some victime -> 
	    let mangeur = IND.manger 1 predateur in
	    let nourriture_restante = PROIE.tuer_individu nourriture_bonne victime in
	    let nouvelle_popu_bonne = (mangeur :: (tuer_individu popu_bonne predateur)) in
	    nutri_rec predateur' nouvelle_popu_bonne nourriture_restante
      in nutri_rec popu [] nourri;;
	    
	    
	    
    let affichage popu = iter (IND.afficher) popu;;
   

  end;;




(* Tests *)
module Zherb = Make_Zherb (Symbioz);;
module Zherbs = Make_Zherbs (Symbioz) (Make_Zherb);;

module Krapit = Make_Krapit (Symbioz);;
module Krapits = Make_Bestioles (Symbioz) (Zherbs) (Make_Krapit);;

let pop_zherbs = Zherbs.random_population 10;;
Zherbs.affichage pop_zherbs;;
let pop2 = Zherbs.vieillissement pop_zherbs;;
Zherbs.affichage pop2;;
let pop3 = Zherbs.vieillissement pop_zherbs;;
Zherbs.affichage pop3;;
let pop4 = Zherbs.vieillissement pop3;;
Zherbs.affichage pop4;;




let pop = Krapits.random_population 10;;
Krapits.affichage pop;;
Krapits.mouvement pop_zherbs pop;;
Krapits.affichage pop;;
Krapits.nourriture

