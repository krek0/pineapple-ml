open Type

(*e[x<-v]*)
let rec sub e x v = match e with
  | Var y when y = x  -> v
  | Const _ | Op _ | Var _ -> e
  | Fun (y,_) when y = x -> e
  | Fun (y,e1) -> Fun(y,sub e1 x v)
  | App (e1,e2) -> App(sub e1 x v, sub e2 x v)
  | Pair (e1,e2) -> Pair(sub e1 x v, sub e2 x v)
  | Let(y,e1,e2) when y = x -> Let(y,sub e1 x v,e2)
  | Let(y,e1,e2) -> Let(y,sub e1 x v, sub e2 x v)

let rec eval e = match e with
  | Const _ | Op _ | Fun _ -> e
  | Pair(e1,e2) -> Pair(eval e1, eval e2)
  | Let(x,e1,e2) -> eval @@ sub e2 x (eval e1)
  | App (e1,e2) -> (
      match eval e1 with
        | Fun(x,e) -> eval @@ sub e x (eval e2)
        | Op "+" -> (
          match eval e2 with
            | Pair(Const n1, Const n2) -> Const(n1+n2)
            | _ -> failwith "eval +" )
        | Op "-" -> (
          match eval e2 with
            | Pair(Const n1, Const n2) -> Const(n1-n2)
            | Const n -> Const(-n)
            | _ -> failwith "eval -" )
        | Op "*" -> (
          match eval e2 with
            | Pair(Const n1, Const n2) -> Const(n1*n2)
            | _ -> failwith "eval *" )
        | Op "/" -> (
          match eval e2 with
            | Pair(Const n1, Const n2) -> Const(n1/n2)
            | _ -> failwith "eval /" )
        | Op "%" -> (
          match eval e2 with
            | Pair(Const n1, Const n2) -> Const(n1 mod n2)
            | _ -> failwith "eval %" )
        | Op "=" -> (
          match eval e2 with
            | Pair(Const n1, Const n2) -> Const(if n1=n2 then 1 else 0)
            | _ -> failwith "eval =" )
        | Op "fst" -> (
          match eval e2 with
            | Pair(v1,v2) -> v1
            | _ -> failwith "eval fst" )
        | Op "snd" -> (
          match eval e2 with
            | Pair(v1,v2) -> v2
            | _ -> failwith "eval snd" )
        | Op "opif" -> ( (*vtrue and vfalse are in a abstraction to freeze evaluation
          before matching in order to evaluate only the selected branch after matching*)
          match eval e2 with
            | Pair(Const n1, Pair(Fun(_,vtrue), Fun (_,vfalse))) -> if n1 = 1 then eval vtrue else eval vfalse
            | _ -> failwith "eval if" )
        | Op "opfix" -> (
          match eval e2 with
            | Fun(f,e) -> eval @@ sub e f (App((Op "opfix"),Fun(f,e)))
            | _ -> failwith "eval fix" )
        | _ -> failwith "eval operator"
    )
  | _ -> failwith "eval error"

(*
let plus_1 = Fun("x",App( Op("+"),Pair(Var("x"),Const(1))));;
let fix = Fun("f",App(Fun("x",App(Var "f", Fun("y",App(Var"x",App(Var "x", Var "y"))))),Fun("x",App(Var "f", Fun("y",App(Var"x",App(Var "x", Var "y")))))));;
let fact = recc "fact" "n" @@ iff (App(Op "=", Pair(Var "n", Const 0))) (Const 1) (App(Op "*", Pair(Var "n",App(Var "fact",App(Op "+",Pair(Var "n",Const (-1)))))))
*)
