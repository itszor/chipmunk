
CC 		:= gcc
CFLAGS		:= -O2 -g
LEX		:= ocamllex
YACC		:= ocamlyacc -v
CAMLC		:= OCAMLFIND_COMMANDS='ocamlc=ocamlc.opt' ocamlfind ocamlc -g
CAMLP4O		:= camlp4o -I . -I opt
PP		:= -pp "camlp4o"
CAMLMKTOP 	:= ocamlmktop
CAMLCOPT 	:= OCAMLFIND_COMMANDS='ocamlopt=ocamlopt.opt' ocamlfind ocamlopt -inline 100 -ccopt -g
INCLUDE 	:= -I../sys -I../libjtype
CAMLINC 	:= -I . -I opt -package extlib
CAMLLIB		:= -linkpkg -package extlib
CAMLDEP 	:= ocamldep -I . -I opt

OBJS =	i32op.cmo common.cmo trace.cmo serial.cmo memory.cmo lookup.cmo \
	fetch.cmo syscalls.cmo profiler.cmo opt/alloc.cmo processor.cmo \
	opt/graph.cmo opt/index.cmo opt/tag.cmo state.cmo opt/id.cmo \
	iformat.cmo decode.cmo disassemble.cmo loadelf.cmo \
	opt/remotestate.cmo opt/mergeldt.cmo opt/translate.cmo opt/sets.cmo \
	opt/stitch.cmo opt/tcache.cmo seq.cmo opt/ast.cmo opt/memotree.cmo \
	opt/mtree.cmo opt/disast.cmo opt/asmsupport.cmo opt/rules.cmo \
	opt/select.cmo opt/block.cmo opt/interference2.cmo \
	opt/simplifyflow.cmo opt/dominator.cmo opt/dumps.cmo opt/stmt.cmo \
	opt/phi.cmo opt/sanity.cmo opt/mergetransfer.cmo opt/encode.cmo \
	opt/usedef.cmo opt/deadcode.cmo opt/interference.cmo \
	opt/precolour.cmo opt/regpool.cmo opt/colour.cmo opt/synchronise.cmo \
	opt/constprop.cmo opt/consteval.cmo opt/flatten.cmo opt/selectdrv.cmo \
	opt/layout.cmo opt/solidify.cmo opt/killmov.cmo opt/driver.cmo \
	opt/replace.cmo exec.cmo floatsupport.cmo pipeline.cmo regress.cmo \
	main.cmo cache.cmo

COBJS =	loadelf_c.o floatsupport_c.o

# Reduced set of objects needed for preprocessing other files
PEXTOBJ = common.cmo i32op.cmo iformat.cmo seq.cmo opt/id.cmo \
	opt/graph.cmo opt/sets.cmo \
	opt/ast.cmo opt/memotree.cmo opt/mtree.cmo opt/rulelex.cmo \
	opt/asmlex.cmo opt/assemble.cmo opt/ruleparse.cmo

PEXTLIB =	pr_o.cmo
# PEXTLIB =	pr_dump.cmo

LIBS 	:= bigarray.cma unix.cma

NLIBS =	$(LIBS:.cma=.cmxa)

NOBJS =	$(OBJS:.cmo=.cmx)

.PHONY:	clean cleaner x86test

all:    sim

opt:	sim.opt

clean:
	rm -f *.cmx *.cmo *.cmi *.o opt/*.cmx opt/*.cmo opt/*.cmi opt/*.o \
	sim sim.opt

cleaner:
	rm -f *.cmx *.cmo *.cmi *.o *.d opt/*.cmx opt/*.cmo opt/*.cmi \
	opt/*.o opt/*.d sim sim.opt;

%.d:	%.ml
	$(CAMLDEP) $< $(subst .ml,.mli,$<) > $@
#	$(CAMLDEP) $< > $@

# client.d:	client.ml
#	$(CAMLDEP) $(PP) $< > $@

%.d:	%.c
	$(CC) $(INCLUDE) -MM $< > $@

sim:   $(OBJS) $(COBJS)
	$(CAMLC) $(CAMLLIB) -custom $(COBJS) -o $@ $(LIBS) $(OBJS)

simtop:	$(OBJS) $(COBJS)
	$(CAMLMKTOP) -custom $(COBJS) -o $@ $(LIBS) $(OBJS)

sim.opt:	$(NOBJS) $(COBJS)
	$(CAMLCOPT) $(CAMLLIB) $(NLIBS) $(NOBJS) $(COBJS) -o $@

# opt/asmlex.ml:	opt/asmlex.mll

# opt/rulelex.ml: opt/rulelex.mll

opt/assemble.cmo:	opt/assemble.ml opt/assemble.cmi opt/asmlex.ml \
			opt/asmlex.cmi opt/asmlex.cmo
	$(CAMLC) -pp "$(CAMLP4O) q_MLast.cmo pa_extend.cmo" \
	-I +camlp4 -I opt -c $< -o $@

opt/ruleparse.cmo:	opt/ruleparse.ml opt/rulelex.ml opt/rulelex.cmo \
			opt/mtree.cmo opt/ruleparse.cmi opt/ast.cmo \
			opt/asmlex.cmi opt/asmlex.cmo \
			opt/assemble.cmi opt/assemble.cmo
	$(CAMLC) -pp "$(CAMLP4O) q_MLast.cmo pa_extend.cmo" \
	-I +camlp4 -I opt -c $< -o $@

opt/assemble.cmx:	opt/assemble.ml opt/asmlex.ml opt/asmlex.mll \
			opt/asmlex.cmx
	$(CAMLCOPT) -pp "$(CAMLP4O) q_MLast.cmo pa_extend.cmo" -I +camlp4 \
	-c $< -o $@

opt/ruleparse.cmx:	opt/ruleparse.ml opt/rulelex.ml opt/rulelex.cmx \
			opt/mtree.cmx opt/ruleparse.cmi opt/ast.cmx
	$(CAMLCOPT) -pp "$(CAMLP4O) q_MLast.cmo pa_extend.cmo" -I +camlp4 \
	-c $< -o $@

opt/assemble.cmi:	opt/assemble.mli
	$(CAMLC) -I +camlp4 -c $< -o $@

opt/ruleparse.cmi:	opt/ruleparse.mli opt/rulelex.mli opt/rulelex.cmi \
			opt/mtree.cmi
	$(CAMLC) -I +camlp4 -c $< -o $@

# shouldn't be needed any more, look at opt/rules_pp.ml
# rules.out:	opt/rules.ml $(PEXTOBJ)
#	$(CAMLP4O) $(PEXTOBJ) $(PEXTLIB) opt/rules.ml > rules.out

x86tok.cmo:	x86tok.ml

x86tok.cmi:	x86tok.mli

x86tok.ml:	x86tok.mll
	$(LEX) $<

x86parse.cmo:	x86parse.ml x86tok.ml x86tok.mll x86tok.cmo
	$(CAMLC) -pp "$(CAMLP4O) q_MLast.cmo pa_extend.cmo" -I +camlp4 \
	-c $< -o $@

x86parse.d:	x86parse.ml
	$(CAMLDEP) -pp '$(CAMLP4O) q_MLast.cmo pa_extend.cmo' -I +camlp4 \
	-c $< > $@

x86parse.cmo:	x86parse.cmi

x86test: 	x86parse.cmo x86tok.cmo x86support_c.o seq.cmo x86support.cmo
	ocamlmktop -custom x86support_c.o -I +camlp4 camlp4o.cma q_MLast.cmo \
	  pa_extend.cmo x86tok.cmo seq.cmo x86support.cmo x86parse.cmo \
	  -o x86testtop
	ledit ./x86testtop

opt/rules.ml: opt/rules_pp.ml $(PEXTOBJ)
	$(CAMLP4O) $(PEXTOBJ) $(PEXTLIB) $< -o $@

# client.cmo:	client.ml
#	$(CAMLC) $(PP) -c $< -o $@

%.cmo:	%.ml %.cmi
	$(CAMLC) $(CAMLINC) -c $< -o $@

%.cmi:	%.mli
	$(CAMLC) $(CAMLINC) -c $< -o $@

# client.cmx:	client.ml
#	$(CAMLCOPT) $(CAMLINC) $(PP) -c $< -o $@

%.cmx:	%.ml %.cmi
	$(CAMLCOPT) $(CAMLINC) -c $< -o $@

%.ml:	%.mll
	$(LEX) $<

opt/%.ml:	opt/%.mll
	$(LEX) $<

%.o:	%.c
	$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleaner)
include $(OBJS:.cmo=.d)
include $(COBJS:.o=.d)
endif
endif

