# Pineapple ML
A small "mini ml" interpreter in ocaml

The project is not yet usable

# How to use
Currently you have to do `make run` with utop installed on your computer

Then you can do `run "<code>"`

# Code example
`let f = fun x -> x+12 in f 12`

`let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1) in fact 10`

`let a = 4 in let f = fun x -> x + 1 in f a`

# Todo
- [ ] Cleanup
- [ ] Add type support
- [ ] Make the binary usable with args
- [ ] Add error messages
