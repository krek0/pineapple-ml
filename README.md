# Pineapple ML
A small "mini ml" interpreter in ocaml

The project is not yet usable

# How to use

## Interactively
Do `make run` with utop installed on your computer

Then you can do `run "<code>"`

## Executable
Do `make` to build then run the executable in builds/out with a file path at argument

# Code example
`let f = fun x -> x+12 in f 12`

`let rec fact = fun n -> if n = 0 then 1 else n * fact (n-1) in fact 10`

`let a = 4 in let f = fun x -> x + 1 in f a`

# Todo
- [x] Cleanup
- [ ] Change the way of handle operator priority
- [ ] Add type support
- [x] Make the binary usable with args
- [ ] Add argument to binary
- [ ] Add error messages
