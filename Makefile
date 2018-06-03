#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP:=$(shell pwd)

SRC=find_source.ml

TARGET=pfff

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------

PROGS=pfff

PROGS+=pfff_test

OPTPROGS= $(PROGS:=.opt)

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------

#format: XXXDIR, XXXCMD, XXXCMDOPT, XXXINCLUDE (if different XXXDIR), XXXCMA
#template:
#  ifeq ($(FEATURE_XXX), 1)
#  XXXDIR=xxx
#  XXXCMD= $(MAKE) -C xxx &&  $(MAKE) xxx -C commons
#  XXXCMDOPT= $(MAKE) -C xxx &&  $(MAKE) xxx.opt -C commons
#  XXXCMA=xxx/xxx.cma  commons/commons_xxx.cma
#  XXXSYSCMA=xxx.cma
#  XXXINCLUDE=xxx
#  else
#  XXXCMD=
#  XXXCMDOPT=
#  endif


# should be FEATURE_OCAMLGRAPH, or should give dependencies between features

JSONDIR=external/jsonwheel
JSONCMA=external/jsonwheel/jsonwheel.cma
#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
BASICSYSLIBS=nums.cma bigarray.cma str.cma unix.cma

# used for sgrep and other small utilities which I dont want to depend
# on too much things
BASICLIBS=commons/commons.cma \
 commons_core/commons_core.cma \
 $(JSONCMA) \
 globals/lib.cma \
 h_program-lang/lib.cma \
 lang_cpp/parsing/lib.cma \
 lang_c/parsing/lib.cma \
 generators/nim/lib.cma

#       commons/commons_features.cma \

SYSLIBS=nums.cma bigarray.cma str.cma unix.cma
SYSLIBS+=$(OCAMLCOMPILERCMA)

# use for the other programs
LIBS= commons/commons.cma \
    commons_core/commons_core.cma \
	$(JSONCMA) \
    globals/lib.cma \
	h_files-format/lib.cma \
	h_program-lang/lib.cma \
    lang_cpp/parsing/lib.cma \
    lang_c/parsing/lib.cma \
    generators/nim/lib.cma

MAKESUBDIRS=commons commons_core \
  $(JSONDIR) \
  globals \
  h_files-format \
  h_program-lang \
  lang_cpp/parsing \
  lang_c/parsing \
  generators/nim

INCLUDEDIRS=$(MAKESUBDIRS)

PP=-pp "cpp $(CLANG_HACK) -DFEATURE_BYTECODE=$(FEATURE_BYTECODE) -DFEATURE_CMT=$(FEATURE_CMT)"

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

.PHONY:: all clean distclean

#note: old: was before all: rec $(EXEC) ... but can not do that cos make -j20
#could try to compile $(EXEC) before rec. So here force sequentiality.

all:: Makefile.config
	$(MAKE) rec
	$(MAKE) $(PROGS)

opt:
	$(MAKE) rec.opt
	$(MAKE) $(OPTPROGS)
all.opt: opt


#	$(MAKE) features -C commons
#	$(MAKE) features.opt -C commons

rec:
	$(MAKE) -C commons
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done

rec.opt:
	$(MAKE) all.opt -C commons
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done

$(TARGET): $(BASICLIBS) $(OBJS) main.cmo
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^


$(TARGET).top: $(LIBS) $(OBJS)
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) threads.cma $^


clean::
	rm -f $(TARGET)
clean::
	rm -f $(TARGET).top
clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done
clean::
	rm -f *.opt

depend::
	set -e; for i in $(MAKESUBDIRS); do echo $$i; $(MAKE) -C $$i depend; done

Makefile.config:
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
	rm -f Makefile.config
	rm -f globals/config_pfff.ml
	rm -f TAGS
#	find -name ".#*1.*" | xargs rm -f

# add -custom so dont need add e.g. ocamlbdb/ in LD_LIBRARY_PATH
CUSTOM=-custom

static:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) STATIC="-ccopt -static" $(EXEC).opt
	cp $(EXEC).opt $(EXEC)

purebytecode:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) BYTECODE_STATIC="" $(EXEC)

#------------------------------------------------------------------------------
# codegraph (was pm_depend)
#------------------------------------------------------------------------------

pfff_test: $(LIBS) $(OBJS) main_test.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
pfff_test.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_test.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^
clean::
	rm -f pfff_test

tests:
	$(MAKE) rec && $(MAKE) pfff_test
	./pfff_test -verbose all
test:
	$(MAKE) rec && $(MAKE) pfff_test
	./pfff_test -verbose all

##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

##############################################################################
# Install
##############################################################################

VERSION=$(shell cat globals/config_pfff.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')

# note: don't remove DESTDIR, it can be set by package build system like ebuild
install: all
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(SHAREDIR)
	cp -a $(PROGS) $(DESTDIR)$(BINDIR)
	cp -a data $(DESTDIR)$(SHAREDIR)
	@echo ""
	@echo "You can also install pfff by copying the programs"
	@echo "available in this directory anywhere you want and"
	@echo "give it the right options to find its configuration files."

uninstall:
	rm -rf $(DESTDIR)$(SHAREDIR)/data


INSTALL_SUBDIRS= \
  commons \
  lang_cpp/parsing

LIBNAME=pfff
install-findlib:: all all.opt
	ocamlfind install $(LIBNAME) META
	set -e; for i in $(INSTALL_SUBDIRS); do echo $$i; $(MAKE) -C $$i install-findlib; done

uninstall-findlib::
	set -e; for i in $(INSTALL_SUBDIRS); do echo $$i; $(MAKE) -C $$i uninstall-findlib; done

version:
	@echo $(VERSION)


install-bin:
	cp $(PROGS) ../pfff-binaries/mac

##############################################################################
# Package rules
##############################################################################

PACKAGE=$(TARGET)-$(VERSION)
TMP=/tmp

package:
	make srctar

srctar:
	make clean
	cp -a .  $(TMP)/$(PACKAGE)
	cd $(TMP); tar cvfz $(PACKAGE).tgz  --exclude=CVS --exclude=_darcs  $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)

#todo? automatically build binaries for Linux, Windows, etc?
#http://stackoverflow.com/questions/2689813/cross-compile-windows-64-bit-exe-from-linux

# making an OPAM package:
# - git push from pfff to github
# - make a new release on github: https://github.com/facebook/pfff/releases
# - get md5sum of new archive
# - update opam file in opam-repository/pfff-xxx/
# - test locally?
# - commit, git push
# - do pull request on github
