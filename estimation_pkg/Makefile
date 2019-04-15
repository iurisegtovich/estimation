# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Instruções

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Makefile keywords

build: .FORCE
	make BUILDS/$(mode)/bin/main.elf mode=$(mode)

run: .FORCE
	make BUILDS/$(mode)/bin/main.elf mode=$(mode)
	(export PATH=$$PATH:gsl_lib; echo $$PATH; \
	BUILDS/$(mode)/bin/main.elf)

debug: .FORCE
	make BUILDS/debug/bin/main.elf mode=debug
	(export PATH=$$PATH:gsl_lib; echo $$PATH; \
	cgdb BUILDS/debug/bin/main.elf)

memcheck: .FORCE
	make BUILDS/debug/bin/main.elf mode=debug
	(export PATH=$$PATH:gsl_lib; echo $$PATH; \
	valgrind --gen-suppressions=yes --leak-check=full --track-origins=yes --show-leak-kinds=all BUILDS/debug/bin/main.elf)

version: .FORCE
	git log -1 --pretty=format:"commit %H%n" > BUILDS/$(mode)/bin/version.txt #hash
	git log -1 --pretty=format:"Date: %ad%n" >> BUILDS/$(mode)/bin/version.txt #date
	git status -sb >> BUILDS/$(mode)/bin/version.txt #status

clean: .FORCE
	rm -f BUILDS/$(mode)/bin/*.elf
	rm -f BUILDS/$(mode)/obj/*.o
	rm -f BUILDS/$(mode)/obj/*.mod
	rm -f BUILDS/$(mode)/bin/version.txt

# <<<<<<<<<<<<<<<<<<<<<bj<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# O programa make:

# Para executar as receitas e construir o programa basta digitar
## make

# Opção recompilar do make:

# Para forçar recompilar todos os arquivos do projeto,
# independentemente da verificação de hora de modificação
## make -B

# Funções preparadas nesse arquivo:

# Para apagar todos os arquivos objeto temporários e programa final
## make clean

# Para (re-)compilar e rodar o programa
## make run

# Para (re-)compilar e rodar o programa no modo de verificação de memória
## make memcheck

# Para (re-)compilar e rodar o programa no modo de debug step-by-step
## make debug

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# mode: pode mudar aqui para fast ou trap se quiser usar os modos de código acelerado (e sem rastreamento de erros) ou modo trap (com detecção de infinity, nan, etc,...)

## make run mode=debug
## make run mode=trap
## make run mode=fast

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Arquivos e receitas do projeto:

## Receita para o programa final:
BUILDS/$(mode)/bin/main.elf: .FORCE
	#dassl
	$(COMPILER) $(FOR_BASIC_OPTIONS) -JBUILDS/$(mode)/obj -c dassl_src/dlinpk.f -o BUILDS/$(mode)/obj/dlinpk.o
	$(COMPILER) $(FOR_BASIC_OPTIONS) -JBUILDS/$(mode)/obj -c dassl_src/ddassl.f -o BUILDS/$(mode)/obj/ddassl.o
	#random
	$(COMPILER) $(FCOPTS) -Wno-conversion -JBUILDS/$(mode)/obj -c ziggurat_src/ziggurat.f90 -o BUILDS/$(mode)/obj/ziggurat.o
	#estimation
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/global.f90 -o BUILDS/$(mode)/obj/global.o
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/WLSobjF.f90 -o BUILDS/$(mode)/obj/WLSobjF.o
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/Swarm.f90 -o BUILDS/$(mode)/obj/Swarm.o
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/GaussNewton.f90 -o BUILDS/$(mode)/obj/GaussNewton.o
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c estimation_src/estimation.f90 -o BUILDS/$(mode)/obj/estimation.o
	#principal, modelo
	$(COMPILER) $(FCOPTS) -JBUILDS/$(mode)/obj -c MODEL_src/main.f90 -o BUILDS/$(mode)/obj/main.o
	#version
	make version
	#link
	$(LINKER) $(LINK_OPTS) BUILDS/$(mode)/obj/*.o -o $@ -Lgsl_lib -lgsl -lgslcblas

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Configurações globais do Makefile:

#Compiler and linker
COMPILER = gfortran
LINKER = gfortran

#flags for each mode
BASIC_OPTS = -cpp -fmax-errors=1 -ffree-line-length-0 -Wall -Wextra -fimplicit-none -g -pedantic -std=f2008ts -fall-intrinsics

debug_OPTS = -O0 -fbacktrace -fcheck=bounds -fcheck=array-temps -fcheck=do -fcheck=mem
###as flags -fcheck=pointer e -fcheck=recursive (inclusas no -fcheck=all) estavam gerando problemas no gdb

#trap
trap_OPTS = -ffpe-trap=invalid,zero,overflow,underflow,denormal
##> ‘invalid’ (invalid floating point operation, such as SQRT(-1.0)),
##> ‘zero’ (division by zero),
##> ‘overflow’ (overflow in a floating point operation),
##> ‘underflow’ (underflow in a floating point operation),
##> ‘inexact’ (loss of precision during operation), and
##> ‘denormal’ (operation performed on a denormal value). 
## The first three exceptions (‘invalid’, ‘zero’, and ‘overflow’) often indicate serious errors, and unless the program has provisions for dealing with these exceptions, enabling traps for these three exceptions is probably a good idea. 
### (https://gcc.gnu.org/onlinedocs/gfortran/Debugging-Options.html)

#fast
fast_OPTS = -march=native -Ofast -fno-backtrace
### (https://wiki.gentoo.org/wiki/GCC_optimization/pt-br)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Lógica de seleção de modos de construção
mode ?= debug
#selection
ifeq ($(mode),debug)
  FCOPTS = $(BASIC_OPTS) $(debug_OPTS)
  LINK_OPTS = 
else ifeq ($(mode),trap)
  FCOPTS = $(BASIC_OPTS) $(debug_OPTS) $(trap_OPTS)
  LINK_OPTS = 
else ifeq ($(mode),fast)
  FCOPTS = $(BASIC_OPTS) $(fast_OPTS)
  LINK_OPTS = 
else ifeq ($(mode),release)
  FCOPTS = $(BASIC_OPTS)
  LINK_OPTS = -static
else
  $(error mode value - "mode=debug" or "mode=fast" or "mode=trap" or "mode=release")
endif

FOR_BASIC_OPTIONS = -O0 -std=legacy
#DO NOT USE -Ofast WHEN COMPILING DASSL, IT BREAKS.
#IT MAY MALFUNCTION EVEN WITH -O2
#-fcheck-all SHOWS MEMORY ADRESSING ERRORS INTRINSIC TO THE CODE
#USING -O0 IS THE SAFEST OPTION.

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Configurações globais para as regras de construção

#default target keyword
.DEFAULT_GOAL := build

# phony target .FORCE to force executing keyword recipes ignoring like-named files
.PHONY: .FORCE

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
