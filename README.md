# Pineapple ML
A small "mini ml" interpreter in ocaml

The goal is to do as many things as possible myself.

The lexer and parser are done by me, mostly helped by [wikipedia](https://wikipedia.org), and for the interpreter part, I was helped by [this course](https://www.enseignement.polytechnique.fr/informatique/INF564/poly.pdf) by Jean-Christophe Filliâtre.

# How to use

## Interactively
Do `make run` with utop installed on your computer

Then you can do `run "<code>"`

## Executable
Usage: `pml [--debug] <file>`

# Code example
`let f = fun x -> x+12 in f 12`

`let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1) in fact 10`

`let a = 4 in let f = fun x -> x + 1 in f a`

# Todo
- [x] Cleanup
- [ ] Change the way of handle operator priority
- [ ] Add type support (currently working on it)
- [x] Make the binary usable with args
- [x] Better args support
- [ ] Add error messages
