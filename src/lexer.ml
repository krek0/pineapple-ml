open Type

exception Lexing
  
let keywords = [
  LFun;
  LRec; LArrow;
  LLet; LIn;
  LIf; LThen; LElse;
  LTrue; LFalse;
  LUnit;
  LLeftPar; LRightPar; LComma;
  LSemicolon
]

let lexique = [
    "fun";
    "rec"; "->";
    "let"; "in";
    "if"; "then"; "else";
    "true"; "false";
    "()";
    "("; ")"; ",";
    ";"
  ]

(*each sub-list represents progressively higher priorities*)
let prefix_operators = [
    ["&&";"||"];
    ["=";">"; ">="; "<"; "<="; "!="];
    ["+";"-"];
    ["*";"/";"%"];
  ]

let infix_operators =
  ["fst";"snd";"print"]

let vars = [
  'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';
  '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
  '_'
  ]

let vars_start = [
  'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';
  ]

let numbers = [
  '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'_'
  ]

let spaces = [
    ' ';'\n';'\t'
  ]

let operators = prefix_operators @ [infix_operators]

type automate = {
  mutable nb : int; (*cell number*)
  final : (int, lexem option) Hashtbl.t;
  delta : (int * char, int) Hashtbl.t
}

let lexer = {
  nb = 0;
  final = Hashtbl.create 0;
  delta = Hashtbl.create 0;
}

(*Add a transition from q with car a, return pointed cell*)
let _add_transition q a =
  match Hashtbl.find_opt lexer.delta (q,a) with
    | Some q' -> q'
    | None -> lexer.nb <- lexer.nb + 1;
              let q' = lexer.nb in
              Hashtbl.replace lexer.final q' None;
              Hashtbl.replace lexer.delta (q,a) q';
              q'

(*Add the lexem l represented by the string l*)
let _add_one_lexem s l =
  let n = String.length s in
  let rec aux i q =
    if i <> n then
      aux (i+1) @@ _add_transition q s.[i]
    else
      Hashtbl.replace lexer.final q @@ Some(l)
  in
  aux 0 0

let init_lexer () =
  Hashtbl.replace lexer.final 0 None;
  
  (*Numbers*)
  lexer.nb <- lexer.nb + 1;
  let q_numbers = lexer.nb in
  Hashtbl.replace lexer.final q_numbers (Some (LNumber ""));
  List.iter (fun c -> Hashtbl.replace lexer.delta (0,c) q_numbers) numbers; (*0->numbers*)
  List.iter (fun c -> Hashtbl.replace lexer.delta (q_numbers,c) q_numbers) numbers; (*numbers->numbers*)

  (*Keywords*)
  List.iter2 _add_one_lexem lexique keywords;
  
  (*Operators*)
  List.iteri (fun i l -> List.iter (fun s -> _add_one_lexem s @@ LOp (i,s)) l) operators;

  (*vars*)
  lexer.nb <- lexer.nb + 1;
  let q_vars = lexer.nb in
  Hashtbl.replace lexer.final q_vars (Some (LVar ""));
  List.iter (fun c -> Hashtbl.replace lexer.delta (q_vars,c) q_vars) vars; (*var->var*) 

  (*Make vars accessible from any destination:*)
  let rec add_vars q =
      if q <> 0 && Hashtbl.find lexer.final q = None then
        Hashtbl.replace lexer.final q @@ Some(LVar "");
      let aux c = match Hashtbl.find_opt lexer.delta (q,c) with
        | None    -> Hashtbl.replace lexer.delta (q,c) q_vars (*q->var*)
        | Some q' -> if q <> q' then add_vars q'
    in
    if q = 0 then
      List.iter aux vars_start
    else
      List.iter aux vars
  in
  add_vars 0;

  (*Skip spaces*)
  List.iter (fun c -> Hashtbl.replace lexer.delta (0,c) 0) spaces

let _next_lexem s i =
  let n = String.length s in
  let conclude j q =
    match Hashtbl.find lexer.final q with
      | Some l -> (
                    match l with
                    | LVar _    -> (j,LVar  (String.trim @@ String.sub s i (j-i)))
                    | LNumber _  -> (j,LNumber (String.trim @@ String.sub s i (j-i)))
                    | _         -> (j,l)
                  )
      | None   -> raise Lexing
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
        match Hashtbl.find_opt lexer.delta (q,s.[j]) with
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

