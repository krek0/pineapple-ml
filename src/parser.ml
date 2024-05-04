open Type

(*number of priority*)
let max_op = List.length Lexer.operators - 1
(*List de tout les non terminaux qui seront*)
(*nécessaire pour les derivations*)
let f = List.init max_op (fun i -> F (i+1))
let op = List.init (max_op+1) (fun i -> LOp (i,""))

let derivation = [
    (E,[F 1]);
    (E,[LLet;LVar "";LOp (0,"=");E;LIn;E]);
    (E,[LIf;E;LThen;E;LElse;E]);
    (E,[LFun;LVar "";LArrow;E]);
    (E,[LLet;LRec;LVar "";LOp (0,"=");E;LIn;E]);
    (E,[E;LOp (0,"");F 1]);

    (F (max_op),[G]);
    (F (max_op),[LOp (max_op,"");F max_op]);
    (F (max_op),[LOp (1,"-");F max_op]); (*negatives numbers*)
    (F (max_op),[F max_op;G]);
  
    (G,[LConst ""]);
    (G,[LVar ""]);
    (G,[LTrue]);
    (G,[LFalse]);
    (G,[LLeftPar;E;LRightPar]);
    (G,[LLeftPar;E;LComma;E;LRightPar]);
  ]

let derivation = derivation @ List.concat @@ List.init (max_op-1)
                              (fun i -> let i = i + 1 in
                               [(F i,[F (i+1)]);
                                (F i,[F i;LOp (i,"");F (i+1)])])

let start = E

let lexems = [
  End;
  LConst "";
  LVar "";
  LFun;
  LRec ; LArrow;
  LLet ; LIn;
  LIf ; LThen ; LElse;
  LLeftPar ; LRightPar ; LComma;
  E ; G; S]@op@f
let non_terminaux = [E;G;S]@f

let derivation_a = Array.of_list derivation

(*Reduction (i,len), règle i en replacant len lexems*)
type action = Goto of int | Reduction of int*int | Success

type automate = {
  mutable nb : int; (*cell number*)
  items_to_int : ((lexem*lexem list*lexem list) list, int) Hashtbl.t;
  int_to_items : (int, (lexem*lexem list*lexem list) list) Hashtbl.t;
  actions : (int * lexem, action) Hashtbl.t;
}

let parser = {
  nb = 0;
  items_to_int = Hashtbl.create 0;
  int_to_items = Hashtbl.create 0;
  actions = Hashtbl.create 0;
}

let remove_duplicates l =
  let d = Hashtbl.create @@ List.length l in
  List.iter (fun x -> Hashtbl.replace d x 0) l;
  Hashtbl.fold (fun a b c -> a::c) d [];;

let items = (S,[],[start])::List.map (fun e -> (fst e,[],snd e)) derivation

let trd (_,_,z) = z
let fst (x,_,_) = x

let fermeture item =
  (*trouve les non_terminaux qui vont sont just aprés le point*)
  let t = List.filter_map (fun e ->
      if List.is_empty @@ trd e then None
      else
        let h = List.hd @@ trd e in
        if List.mem h non_terminaux then
          Some h
        else None ) 
    item in
  (*Ajoute item à sa fermeture*)
  remove_duplicates @@ item @ List.filter (fun e -> List.mem (fst e) t ) items

let rec next_item item t =
  let rec aux acc e = match e with
    | (h,l1,tt::tl2)::tl when tt = t -> aux ((h,tt::l1,tl2)::acc) tl
    | _::tl -> aux acc tl
    | [] -> acc
  in
  fermeture @@ fermeture @@ fermeture @@ fermeture @@ fermeture @@ aux [] item


let add_one_transition item l i =
  match Hashtbl.find_opt parser.items_to_int item with
  (*Si l'item est déja présent*)
  | Some j -> Hashtbl.replace parser.actions (i,l) @@ Goto j; None
  (*Sinon*)
  | None -> let j = parser.nb in
            parser.nb <- parser.nb + 1;
            Hashtbl.replace parser.actions (i,l) @@ Goto j;
            Hashtbl.replace parser.items_to_int item j;
            Hashtbl.replace parser.int_to_items j item;
            Some j

let rec init_parser () =
  (*Création des deplacement sur l'automate*)
  Hashtbl.replace parser.items_to_int items 0;
  Hashtbl.replace parser.int_to_items 0 items;
  parser.nb <- 1;
  let rec add_transitions item i =
    (*Les transitions à ajouter*)
    let t = remove_duplicates @@ List.filter_map (fun e -> if List.is_empty @@ trd e then None else Some (List.hd @@ trd e)) item in
    List.iter (fun lex -> let next_it = next_item item lex in
                          match add_one_transition next_it lex i with
                            | None -> ()
                            | Some j -> add_transitions next_it j
              ) t
  in
  add_transitions items 0;
  
  (*Ajout des succés*)
  for i = 0 to parser.nb - 1 do
    let item = Hashtbl.find parser.int_to_items i in
    if List.mem (S,[E],[]) item then
      Hashtbl.replace parser.actions (i,End) Success
  done;

  (*Ajout des reductions*)
  for i = 1 to parser.nb - 1 do
    let item = Hashtbl.find parser.int_to_items i in
    List.iter (fun e -> match e with
      | (a,l,[]) when a <> S ->
            let j = Option.get @@ List.find_index ((=) (a,List.rev l)) derivation in
            let len = List.length l in
            (*Si y a pas de go déja dispo alors réduction*)
            List.iter (fun l -> if Hashtbl.find_opt parser.actions (i,l) = None then Hashtbl.replace parser.actions (i,l) @@ Reduction (j,len)) lexems
      | _ -> ()
    ) item
  done


let read li =
    let input = Stack.create () in
    List.iter (fun e -> Stack.push e input) @@ List.rev li;
  
    let pile = Stack.create () in
    Stack.push 0 pile;

    let out = Stack.create () in

    let sucess = ref false in
 
    while not !sucess do
      (*Stack.iter (Printf.printf "%i ") pile;
      print_newline ();*)
      (*On lit l'élément en haut de la pile et on dépile l'entrée*)
      let i = Stack.top pile in
      let e = Stack.top input in
      (*Enleve les parametres aux vars,csts,op pour utiliser les règles derivations génériques*)
      let e' = match e with
        | LVar _ -> LVar ""
        (*Si = est l'opérateur d'atribution on le garde*)
        | LOp (j, s) -> (match Hashtbl.find_opt parser.actions (i,e) with
                          | Some _ -> e
                          | None -> LOp (j, "") )
        | LConst _ -> LConst ""
        | _ -> e
      in
      match Hashtbl.find parser.actions (i,e') with
              (*On enleve un élément de l'entrée puis on ajoute la déstination sur la pile*)
              | Goto j -> ignore @@ Stack.pop input; Stack.push j pile
              (*On dépile len états de la pile puis on ajoute la réduction sur sur l'entrée*)
              | Reduction (i,len) -> let (l,_) = derivation_a.(i) in
                                     for i = 1 to len do ignore @@ Stack.pop pile done;
                                     Stack.push l input;
                                     Stack.push i out;
              | Success -> sucess := true
    done;
    out

type tree = T of int*lexem*(tree list)
let make_tree input =
  let s = read input in
  let named = Stack.create () in
  List.iter (fun e -> match e with LOp _ | LVar _ |LConst _ -> Stack.push e named | _ -> ()) input;
  let rec aux e =
    let i = Stack.pop s in
    let l = List.map (fun e ->
                      if List.mem e non_terminaux then
                        aux e
                     else match e with
                      | LVar "" | LConst "" | LOp _ -> T(-1,Stack.pop named,[])
                      | _ -> T(-1,e,[]))
            @@ List.rev @@ snd derivation_a.(i) in
    T(i,e,List.rev l)
  in
  aux start;;

(*if c then t else e*)
let iff c t e = App(Op "opif", Pair(c,Pair(Fun("",t),Fun("",e))))

let rec parse t = match t with
  | T (1,_,[_;T(_,LVar s,[]);_;t1;_;t2]) -> Let (s,parse t1, parse t2)
  | T (2,_,[_;t1;_;t2;_;t3]) -> iff (parse t1) (parse t2) (parse t3)
  | T (3,_,[_;T(_,LVar s,[]);_;t]) -> Fun (s,parse t)
  | T (4,_,[_;_;T(_,LVar f, []);_;t1;_;t2]) -> Let (f,App(Op "opfix",Fun(f,parse t1)),parse t2)
  | T (5,_,[t1;T(_,LOp (0,s),[]);t2]) -> App(Op s,Pair(parse t1,parse t2))
  
  | T (7,_,[T(_,LOp (_,s),[]);t]) -> App(Op s,parse t)
  | T (8,_,[T(_,LOp (_,s),[]);t]) -> App(Op s,parse t)
  | T (9,_,[t1;t2]) -> App(parse t1,parse t2)

  | T (10,l,[T(_,LConst s,[])]) -> Const (int_of_string s)
  | T (11,l,[T(_,LVar s,[])]) -> Var s
  | T (12,l,[T(_,LTrue,[])]) -> True
  | T (13,l,[T(_,LFalse,[])]) -> True
  | T (14,_,[_;t;_]) -> parse t
  | T (15,_,[_;t1;_;t2;_]) -> Pair (parse t1, parse t2)
  
  | T (_,_,[t1;T(_,LOp (_,s),[]);t2]) -> App (Op s, Pair(parse t1, parse t2))
  | T (_,_,[t]) -> parse t
  
  | T(i,_,_) -> failwith "make parse"
