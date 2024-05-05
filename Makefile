MAIN := main.ml
DEPS := type.ml eval.ml lexer.ml parser.ml print.ml
TMP_MAIN := /tmp/main_tmp.ml
SRC := src
OUT := builds

build: $(DEPS:%=$(SRC)/%) $(MAIN:%=$(SRC)/%)
	ocamlc -I $(SRC) $^ -o $(OUT)/out

run: $(DEPS:%=$(SRC)/%)
	ocamlc -I $(SRC) -c $^
	echo $(foreach dep,$(DEPS:%=$(SRC)/%),"#load \"$(notdir $(dep:.ml=.cmo))\";;") > $(TMP_MAIN)
	cat $(MAIN:%=$(SRC)/%) >> $(TMP_MAIN)
	utop -I $(SRC) -init $(TMP_MAIN)

clean:
	rm -f $(SRC)/*.cmi $(SRC)/*.cmo $(TMP_MAIN)
