# Michaël PÉRIN, Verimag / Université Grenoble-Alpes, Mars 2017 
#
# Part of the project TURING MACHINES FOR REAL
#
#
SOL =
#

MODULES = Fresh Lambda_Calcul Option Tricks MyList MyString Pretty Bit_Vector Logger Color Html Pattern Symbol State Alphabet Band Action Transition Turing_Machine Configuration Execution LC_by_MT Demo Emulator $(SOL)

ML  = $(addsuffix  .ml, $(MODULES))
CMO = $(addsuffix .cmo, $(MODULES))
CMA = 

# The compiler with option
OCAMLC = ocamlc -w -8 -w -26

# directory needed for logging execution traces
log_dir = _log


#

help:
	@echo ""
	@echo "Here are the possible commands:"
	@echo "  make cmo ...... produces .cmo files. This command is used by"
	@echo "  make run ...... produces an executable called \"run\" and executes it"
	@echo "  make play ..... load the .cmo files in the ocaml interpreter. Type #use \"main.ml\";;  in the interpreter to execute the main function."
	@echo "  make clean .... delete compilation files"

cmo: $(ML) main.ml
	@$(OCAMLC) $(CMA) $(ML) main.ml -o run


play: $(log_dir)
	@make cmo
	@echo "#use \"main.ml\";;" > .ledit_history
	@ledit ocaml $(CMA) $(CMO) 

run: $(log_dir)
	@make cmo
	@$(OCAMLC) $(CMA) $(CMO) main.ml -o run
	@./run

clean:
	@rm -f a.out run *.cm* *~
#

%.ml: %.cp4
	@camlp4o -impl $< -o $@

%.cmo: %.ml
	@ocamlc *.cmo $< 

$(log_dir):
	@if test ! -d $(log_dir); then mkdir $(log_dir)/; fi

.PHONY: help cmo play run clean