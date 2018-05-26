TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= parse_info.ml \
     ast_fuzzy.ml meta_ast_generic.ml \
     skip_code.ml \
     scope_code.ml \
     pretty_print_code.ml

# See also graph_code/graph_code.ml! closely related to h_program-lang/

SYSLIBS= str.cma unix.cma
LIBS=../commons/lib.cma
INCLUDEDIRS= $(TOP)/commons \
 $(TOP)/external/jsonwheel \
 $(TOP)/h_files-format

# other sources:
#  prolog_code.pl, facts.pl, for the prolog-based code query engine

# dead: visitor_code, statistics_code, programming-language, ast_generic
##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


archi_code_lexer.ml: archi_code_lexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f archi_code_lexer.ml
beforedepend:: archi_code_lexer.ml

##############################################################################
# install
##############################################################################
LIBNAME=pfff-h_program-lang
EXPORTSRC=\
  ast_fuzzy.mli \
  meta_ast_generic.mli \
  parse_info.mli \
  scope_code.mli \
  skip_code.mli

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
	  pretty_print_code.cmi
