MAIN := main.ml
DEPS := type.cmo lexer.cmo print.cmo eval.cmo parser.cmo run.cmo
SRC := src
OUT := builds/pml

build: $(DEPS:%=$(SRC)/%) $(MAIN:%=$(SRC)/%)
	ocamlc -I $(SRC) $^ -o $(OUT)

run: $(DEPS:%=$(SRC)/%)
	ocamlc -I $(SRC) -c $^
	utop -I $(SRC) $(DEPS) -init $(SRC)/$(MAIN)

clean:
	rm -f $(SRC)/*.cmi $(SRC)/*.cmo $(OUT)
