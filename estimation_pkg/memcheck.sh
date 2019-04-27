#!/usr/bin/env bash

#roda o executavel através do programa valgring para detectar erros de memoria

#dependencia valgrind
#ubuntu:
#sudo apt install valgrind
#windows/msys2/mingw:
#não está disponivel

make build mode=safe
valgrind --gen-suppressions=yes --leak-check=full --track-origins=yes --show-leak-kinds=all BUILDS/safe/bin/main.elf
