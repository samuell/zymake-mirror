# This makefile should work with any unix make or Microsoft's nmake
# It builds zymake from source

MODULE_SOURCES = util.ml mystring.ml eval.ml parser.ml lexer.ml topo.ml load.ml build.ml compile.ml kvsmap.ml repr.ml dag.ml firule.ml version.ml
GEN_SOURCES = $(MODULE_SOURCES) parseopt.ml main.ppo.ml
#HEADERS = types.cmi parser.cmi topo.cmi load.cmi
HEADERS = types.cmi

# change to ocamlc / cma for bytecode
OCAMLC = ocamlopt
LIBEXT = cmxa

zymake: $(HEADERS) $(GEN_SOURCES)
	$(OCAMLC) -thread -o $@ unix.$(LIBEXT) threads.$(LIBEXT) $(GEN_SOURCES)

# dependencies for this program

parser.cmi: types.cmi

## build rules for ocaml

.mll.ml:
	ocamllex -o $@ $<

.mli.cmi:
	ocamlc $<

.mly.ml:
	ocamlyacc -v $<

.mly.mli:
	ocamlyacc -v $<

.SUFFIXES: .mll .ml .cmi .mli .mly

clean:
	$(RM) *.obj *.cm* *.o zymake zymake.exe
