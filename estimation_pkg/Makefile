# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Configurações globais para as regras de construção
##default target keyword
.DEFAULT_GOAL := build
## phony target .FORCE to force executing keyword recipes ignoring like-named files
.PHONY: .FORCE
##default mode
mode ?= safe
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# TARGETS do Makefile:
##compilar e linkar
build: .FORCE
	make BUILDS/$(mode)/bin/main.elf mode=$(mode) -B
##rodar
run: BUILDS/$(mode)/bin/main.elf .FORCE
	BUILDS/$(mode)/bin/main.elf
##rodar no debugger
debug: BUILDS/$(mode)/bin/main.elf .FORCE
	cgdb BUILDS/$(mode)/bin/main.elf
##deletar objetos antigos
clean: .FORCE
	rm -f BUILDS/$(mode)/obj/*.o
	rm -f BUILDS/$(mode)/obj/*.mod
	rm -f BUILDS/$(mode)/bin/*.elf
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Arquivos e receitas do projeto:
## Receita para o programa final:
BUILDS/$(mode)/bin/main.elf: dassl_src/* ziggurat_src/* estimation_src/* MODEL_src/*
	#clean
	make clean mode=$(mode)
	#dassl
	$(COMPILER) $(DASSL_BASIC_OPTIONS) -JBUILDS/$(mode)/obj -c dassl_src/dlinpk.f -o BUILDS/$(mode)/obj/dlinpk.o
	$(COMPILER) $(DASSL_BASIC_OPTIONS) -JBUILDS/$(mode)/obj -c dassl_src/ddassl.f -o BUILDS/$(mode)/obj/ddassl.o
	#ziggurat
	$(COMPILER) $(FCOPTS) -Wno-conversion -JBUILDS/$(mode)/obj -c ziggurat_src/ziggurat.f90 -o BUILDS/$(mode)/obj/ziggurat.o
	#estimation
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/global.f90 -o BUILDS/$(mode)/obj/global.o
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/WLSobjF.f90 -o BUILDS/$(mode)/obj/WLSobjF.o
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/Swarm.f90 -o BUILDS/$(mode)/obj/Swarm.o
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/GaussNewton.f90 -o BUILDS/$(mode)/obj/GaussNewton.o
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/estimation.f90 -o BUILDS/$(mode)/obj/estimation.o
	#main
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c MODEL_src/main.f90 -o BUILDS/$(mode)/obj/main.o
	#link
	$(LINKER) $(LINK_OPTS) BUILDS/$(mode)/obj/*.o -o $@ -lgsl -lgslcblas
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Configurações globais do Makefile:
##Compilador e linker
COMPILER = gfortran
LINKER = gfortran
##flags for each mode 
###basic
BASIC_OPTS = -cpp -fmax-errors=1 -ffree-line-length-0 -Wall -Wextra -fimplicit-none -g
###safe
safe_OPTS = -O0 -fbacktrace -fcheck=bounds -fcheck=array-temps -fcheck=do -fcheck=mem
###fast (https://wiki.gentoo.org/wiki/GCC_optimization/pt-br)
fast_OPTS = -fno-backtrace -march=native -Ofast
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Lógica de seleção de modos de construção
#seleção
ifeq ($(mode),safe)
  FCOPTS = $(BASIC_OPTS) $(safe_OPTS)
  LINK_OPTS =
else ifeq ($(mode),fast)
  FCOPTS = $(BASIC_OPTS) $(fast_OPTS)
  LINK_OPTS = 
else
  $(error mode value - "mode=safe" (default) or "mode=fast")
endif

DASSL_BASIC_OPTIONS = -O0 -std=legacy
#DO NOT USE -Ofast WHEN COMPILING DASSL, IT BREAKS.
#IT MAY MALFUNCTION EVEN WITH -O2
#-fcheck-all SHOWS MEMORY ADRESSING ERRORS INTRINSIC TO THE CODE
#USING -O0 IS THE SAFEST OPTION.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


