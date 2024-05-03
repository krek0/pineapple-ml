open Type
  
let keywords = [
  LFun;
  LRec; LArrow;
  LLet; LIn;
  LIf; LThen; LElse;
  LLeftPar; LRightPar; LComma
]

let lexique = [
    "fun";
    "rec"; "->";
    "let"; "in";
    "if"; "then"; "else";
    "("; ")"; ","
  ]

let operators = [
    ["=";">"; ">="; "<"; "<="];
    ["+";"-"];
    ["*";"/";"%"];
    ["fst";"snd"] (*prefix operators (except -)*)
  ]

let vars = [
  'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';
  '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
  '_'
  ]

let consts = [
  '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
  ]

let spaces = [
    ' ';'\n';'\t'
  ]

exception LexingError

type automate = {
  mutable nb : int; (*cell number*)
  final : (int, lexem option) Hashtbl.t;
  delta : (int * char, int) Hashtbl.t
}

let lexeur = {
  nb = 0;
  final = Hashtbl.create 0;
  delta = Hashtbl.create 0;
}

let _add_transition q a =
  match Hashtbl.find_opt lexeur.delta (q,a) with
    | Some q' -> q'
    | None -> lexeur.nb <- lexeur.nb + 1;
              let q' = lexeur.nb in
              Hashtbl.replace lexeur.final q' None;
              Hashtbl.replace lexeur.delta (q,a) q';
              q'

let _add_one_lexem s l =
  let n = String.length s in
  let rec aux i q =
    if i <> n then
      aux (i+1) @@ _add_transition q s.[i]
    else
      Hashtbl.replace lexeur.final q @@ Some(l)
  in
  aux 0 0

let init_lexer () =
  Hashtbl.replace lexeur.final 0 None;
  
  (*constants*)
  lexeur.nb <- lexeur.nb + 1;
  let q_consts = lexeur.nb in
  Hashtbl.replace lexeur.final q_consts (Some (LConst ""));
  List.iter (fun c -> Hashtbl.replace lexeur.delta (0,c) q_consts) consts; (*0->consts*)
  List.iter (fun c -> Hashtbl.replace lexeur.delta (q_consts,c) q_consts) consts; (*consts->consts*)


  (*Keywords*)
  List.iter2 _add_one_lexem lexique keywords;
  
  (*Operators*)
  List.iteri (fun i l -> List.iter (fun s -> _add_one_lexem s @@ LOp (i,s)) l) operators;

  (*vars*)
  lexeur.nb <- lexeur.nb + 1;
  let q_vars = lexeur.nb in
  Hashtbl.replace lexeur.final q_vars (Some (LVar ""));
  List.iter (fun c -> Hashtbl.replace lexeur.delta (q_vars,c) q_vars) vars; (*var->var*) 

  (*Add vars accecible from any destination:*)
  let rec add_vars q =
      if q <> 0 && Hashtbl.find lexeur.final q = None then
        Hashtbl.replace lexeur.final q @@ Some(LVar "");
      let aux c = match Hashtbl.find_opt lexeur.delta (q,c) with
        | None    -> Hashtbl.replace lexeur.delta (q,c) q_vars (*q->var*)
        | Some q' -> if q <> q' then add_vars q'
    in
    List.iter aux vars;
  in
  add_vars 0;

  (*Skip spaces*)
  List.iter (fun c -> Hashtbl.replace lexeur.delta (0,c) 0) spaces

let _next_lexem s i =
  let n = String.length s in
  let conclude j q =
    match Hashtbl.find lexeur.final q with
      | Some l -> (
                    match l with
                    | LVar _    -> (j,LVar  (String.trim @@ String.sub s i (j-i)))
                    | LConst _  -> (j,LConst (String.trim @@ String.sub s i (j-i)))
                    | _         -> (j,l)
                  )
      | None   -> raise LexingError
  in
  (*Find the lexem*)
  let rec aux j q =
      if j >= n then (*End of the string*)
        if q = 0 then (*Empty word*)
          (j,End)
        else
          conclude j q
      else
      (
        match Hashtbl.find_opt lexeur.delta (q,s.[j]) with
          | None   -> conclude j q
          | Some q -> aux (j+1) q
      )
    in
    aux i 0;;

let lex s =
  let n = String.length s in
  let rec aux acc i =
    if i >= n then
      acc
    else
      let (j,l) = _next_lexem s i in
      aux (l::acc) (j)
  in
  List.rev @@ End::aux [] 0;;

