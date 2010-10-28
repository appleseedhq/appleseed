
# Modify your compiler here. (icpc supported)
CC=g++

ifeq ($(CC),g++)
	FLAGS = -O3 -Wall -pedantic -ansi -Wextra -fopenmp -Wconversion
else ifeq ($(CC),sunCC)
	# Sun's compiler. Has some issues for now
	FLAGS = -O3 -library=stlport4 
else 
	# icpc 
	FLAGS = -O3 -openmp -ansi
endif

all:
	@echo "*--------------------------------*"
	@echo "| Welcome to STANN!  See         |"
	@echo "| README.txt for usage           |"
	@echo "| instructions or visit us at:   |"
	@echo "| http://www.compgeom.com/~stann |"
	@echo "|                                |"
	@echo "| use 'make test' to run tests   |"
	@echo "| use 'make docs' to generate    |"
	@echo "| doxygen html in docs dir       |"
	@echo "*--------------------------------*"

.PHONY: test docs

test:
	$(CC) $(FLAGS) -o stann_test test.cpp -I./include
	./stann_test
	@rm stann_test*

docs:
	cd docs; doxygen Doxyfile
