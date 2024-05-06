open Type
open Print

(*x by v in e*)
let rec sub e x v = match e with
  | Var y when y = x -> v
  | Number _ | Op _ | Var _ | True | False | Unit -> e
  | Fun (y,_) when y = x -> e
  | Fun (y,e1) -> Fun(y,sub e1 x v)
  | App (e1,e2) -> App(sub e1 x v, sub e2 x v)
  | Pair (e1,e2) -> Pair(sub e1 x v, sub e2 x v)
  | Let(y,e1,e2) when y = x -> Let(y,sub e1 x v,e2)
  | Let(y,e1,e2) -> Let(y,sub e1 x v, sub e2 x v)
  | Multiple(e1,e2) -> Multiple(sub e1 x v,sub e2 x v)

let rec eval e = match e with
  | Number _ | Op _ | Fun _ | True | False | Unit -> e
  | Multiple(e1,e2) -> ignore @@ eval e1; eval e2
  | Pair(e1,e2) -> Pair(eval e1, eval e2)
  | Let(x,e1,e2) -> eval @@ sub e2 x (eval e1)
  | App (e1,e2) -> (
      match eval e1 with
        | Fun(x,e) -> eval @@ sub e x (eval e2)
        | Op "print" -> (
          let e2' = eval e2 in
            print_expression e2'; print_newline (); Unit)
        (*Lazy evaluation for && and ||*)
        | Op "&&" -> (
          match e2 with
            | Pair(f,s) -> (
                if eval f = False then False
                else if eval s = False then False else True
              )
            | _ -> failwith "eval &&" )
        | Op "||" -> (
          match e2 with
            | Pair(f,s) -> (
                if eval f = True then True
                else if eval s = True then True else False
              )
            | _ -> failwith "eval ||" )
        | Op "+" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> Number(n1+n2)
            | _ -> failwith "eval +" )
        | Op "-" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> Number(n1-n2)
            | Number n -> Number(-n)
            | _ -> failwith "eval -" )
        | Op "*" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> Number(n1*n2)
            | _ -> failwith "eval *" )
        | Op "/" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> Number(n1/n2)
            | _ -> failwith "eval /" )
        | Op "%" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> Number(n1 mod n2)
            | _ -> failwith "eval %" )
        | Op "=" -> (
          match eval e2 with
            | Pair(n1, n2) -> if n1=n2 then True else False
            | _ -> failwith "eval =" )
        | Op "<" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> if n1<n2 then True else False
            | _ -> failwith "eval <" )
        | Op "<=" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> if n1<=n2 then True else False
            | _ -> failwith "eval <=" )
        | Op ">" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> if n1>n2 then True else False
            | _ -> failwith "eval >" )
        | Op ">=" -> (
          match eval e2 with
            | Pair(Number n1, Number n2) -> if n1>=n2 then True else False
            | _ -> failwith "eval >=" )
        | Op "fst" -> (
          match eval e2 with
            | Pair(v1,v2) -> v1
            | _ -> failwith "eval fst" )
        | Op "snd" -> (
          match eval e2 with
            | Pair(v1,v2) -> v2
            | _ -> failwith "eval snd" )
        (*Lazy evaluation by having vtrue and vfalse in abstraction*)
        | Op "opif" -> (
          match eval e2 with
            | Pair(c, Pair(Fun(_,vtrue), Fun (_,vfalse))) -> if c = True then eval vtrue else eval vfalse
            | _ -> failwith "eval if" )
        | Op "opfix" -> (
          match eval e2 with
            | Fun(f,e) -> eval @@ sub e f (App((Op "opfix"),Fun(f,e)))
            | _ -> failwith "eval fix" )
        | _ -> failwith "eval operator"
    )
  | _ -> failwith "eval error"
