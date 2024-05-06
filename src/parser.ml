open Type

(*number of priority*)
let max_op = List.length Lexer.operators

(*Aditional terminal symbol to operator priority*)
let f = List.init max_op (fun i -> F i)
(*All operators lexems*)
let op = List.init (max_op+1) (fun i -> LOp (i,""))
let aditional_derivation = List.concat @@ List.init (max_op-1)
  (fun i -> [(F i,[F (i+1)]); (F i,[F i;LOp (i,"");F (i+1)])])

let derivation = [
    (E,[F 0]);
    
    (E,[E;LSemicolon;E]);
    (E,[E;LSemicolon]);

    (F (max_op-1),[G]);
    (F (max_op-1),[LOp (max_op-1,"");F (max_op-1)]);
    (F (max_op-1),[LOp (1,"-");F (max_op-1)]); (*negatives numbers*)
    (F (max_op-1),[F (max_op-1);G]);

    (G,[LLet;LVar "";LOp (1,"=");E;LIn;E]);
    (G,[LIf;E;LThen;E;LElse;E]);
    (G,[LFun;LVar "";LArrow;E]);
    (G,[LLet;LRec;LVar "";LOp (1,"=");E;LIn;E]);
    (G,[LNumber ""]);
    (G,[LVar ""]);
    (G,[LTrue]);
    (G,[LFalse]);
    (G,[LUnit]);
    (G,[LLeftPar;LRightPar]);
    (G,[LLeftPar;E;LRightPar]);
    (G,[LLeftPar;E;LComma;E;LRightPar]);
  ] @ aditional_derivation

let start = E

let lexems = [
  End;
  LNumber "";
  LVar "";
  LFun;
  LRec ; LArrow;
  LLet ; LIn;
  LIf ; LThen ; LElse;
  LLeftPar ; LRightPar ; LComma;
  LUnit;
  LSemicolon;
  E ; G; S]@op@f

let non_terminal = [E;G;S]@f

let derivation_a = Array.of_list derivation
let trd (_,_,z) = z

(*Reduction (i,len), follow derivation i by replacing len lexems*)
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


(*defaults items*)
let default_items = (S,[],[start])::List.map (fun e -> (fst e,[],snd e)) derivation

(*Return the closure of a list of item*)
let closure items =
  let fst (x,_,_) = x in
  (*Do the operation while new item are added*)
  let rec aux items_aux =
    (*find none terminal at the beginig of the list*)
    let t = List.filter_map (fun e ->
        if List.is_empty @@ trd e then None
        else
          let h = List.hd @@ trd e in
          if List.mem h non_terminal then
            Some h
          else None ) 
      items_aux in
    let new_item = remove_duplicates @@ items_aux @ List.filter (fun e -> List.mem (fst e) t ) default_items in
    if List.length new_item = List.length items_aux then items_aux
    else aux new_item
  in
  aux items

(*Return the next item list when chose lexem t*)
let rec next_item item t =
  let rec aux acc e = match e with
    | (h,l1,tt::tl2)::tl when tt = t -> aux ((h,tt::l1,tl2)::acc) tl
    | _::tl -> aux acc tl
    | [] -> acc
  in
  closure @@ aux [] item


let add_one_transition item l i =
  match Hashtbl.find_opt parser.items_to_int item with
  | Some j -> Hashtbl.replace parser.actions (i,l) @@ Goto j; None
  | None -> let j = parser.nb in
            parser.nb <- parser.nb + 1;
            Hashtbl.replace parser.actions (i,l) @@ Goto j;
            Hashtbl.replace parser.items_to_int item j;
            Hashtbl.replace parser.int_to_items j item;
            Some j

let rec init_parser () =
  (*Goto creation*)
  Hashtbl.replace parser.items_to_int default_items 0;
  Hashtbl.replace parser.int_to_items 0 default_items;
  parser.nb <- 1;
  let rec add_transitions item i =
    let t = remove_duplicates @@ List.filter_map (fun e -> if List.is_empty @@ trd e then None else Some (List.hd @@ trd e)) item in
    List.iter (fun lex -> let next_it = next_item item lex in
                          match add_one_transition next_it lex i with
                            | None -> ()
                            | Some j -> add_transitions next_it j
              ) t
  in
  add_transitions default_items 0;
  
  (*Add succes*)
  for i = 0 to parser.nb - 1 do
    let item = Hashtbl.find parser.int_to_items i in
    if List.mem (S,[E],[]) item then
      Hashtbl.replace parser.actions (i,End) Success
  done;

  (*Add reductions*)
  for i = 1 to parser.nb - 1 do
    let item = Hashtbl.find parser.int_to_items i in
    List.iter (fun e -> match e with
      | (a,l,[]) when a <> S ->
            let j = Option.get @@ List.find_index ((=) (a,List.rev l)) derivation in
            let len = List.length l in
            (*Add reduction only when no go possible, (to change)*)
            List.iter (fun l -> if Hashtbl.find_opt parser.actions (i,l) = None then Hashtbl.replace parser.actions (i,l) @@ Reduction (j,len)) lexems
      | _ -> ()
    ) item
  done

(*remove first n element of a list if possible*)
let rec cut l n = match l,n with
  | _,0 -> l
  | [],_ -> l
  | a::r,i -> cut r (i-1) 

type tree = T of lexem*(tree list) | L of lexem

(*Return list of derivation used by a liste of lexem*)
let make_parse_stack li =
    let input = Stack.create () in
    List.iter (fun e -> Stack.push e input) @@ List.rev li;
    let out = Stack.create () in
  
    let rec aux input pile = match input,pile with
        (*Remove lex seeting to use generics rules*)
      | e::r,i::r' ->( let e' = match e with
          | LVar _ -> LVar ""
          | LNumber _ -> LNumber ""
          (*To detect the = of atribution*)
          | LOp (j, s) -> (match Hashtbl.find_opt parser.actions (i,e) with
                            | Some _ -> e
                            | None -> LOp (j, "") )
          | _ -> e
        in
        match Hashtbl.find parser.actions (i,e') with
                | Goto j -> aux r (j::i::r')
                | Reduction (i,len) -> let (l,_) = derivation_a.(i) in
                                       Stack.push i out;
                                       aux (l::e::r) (cut r' (len-1))
                                       (*len-1  beacause i is alread removed from pile*)
                | Success -> ()
      )
      | _ -> failwith "parsing"
    in
    aux li [0];
    out

let _make_tree input =
  let s = make_parse_stack input in
  let terminaux = Stack.create () in
    List.iter (fun e -> if e <> End then Stack.push e terminaux) input;
  let rec aux e =
    let i = Stack.pop s in
    let l = List.map
            (fun e ->
              if List.mem e non_terminal then
                aux e
              else L (Stack.pop terminaux))
            @@ List.rev @@ snd derivation_a.(i) in
    T(e,List.rev l)
  in
  aux start;;

(*if c then t else e*)
let iff c t e = App(Op "opif", Pair(c,Pair(Fun("",t),Fun("",e))))

let rec parse input =
  let tree = _make_tree input in
  let rec aux t = match t with
    | T (_,[L LLet;L LVar s ;L (LOp (1,"="));t1;L LIn;t2]) -> Let (s,aux t1, aux t2)
    | T (_,[L LIf;t1;L LThen;t2;L LElse;t3]) -> iff (aux t1) (aux t2) (aux t3)
    | T (_,[L LFun;L LVar s;L LArrow;t]) -> Fun (s,aux t)
    | T (_,[L LLet;L LRec;L LVar f ;L (LOp (1,"="));t1;L LIn;t2]) -> Let (f,App(Op "opfix",Fun(f,aux t1)),aux t2)
    | T (_,[t1;L LSemicolon;t2]) -> Multiple (aux t1, aux t2) 
    | T (_,[t1;L LSemicolon]) -> aux t1 
  
    | T (_,[L LOp (_,s);t]) -> App(Op s,aux t)

    | T (_,[L LNumber s]) -> Number (int_of_string s)
    | T (_,[L LVar s]) -> Var s
    | T (_,[L LTrue]) -> True
    | T (_,[L LFalse]) -> False
    | T (_,[L LUnit]) -> Unit
    | T (_,[L LLeftPar; L LRightPar]) -> aux t
    | T (_,[L LLeftPar;t; L LRightPar]) -> aux t
    | T (_,[L LLeftPar;t1;L LComma;t2;L LRightPar]) -> Pair (aux t1, aux t2)

    | T (_,[t1;t2]) -> App(aux t1,aux t2)
  
    | T (_,[t1;L LOp (_,s);t2]) -> App (Op s, Pair(aux t1, aux t2))
    | T (_,[t]) -> aux t
  
    | _ -> failwith "parse"
  in
  aux tree

