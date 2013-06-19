# -*-Makefile-*- for FLISP
##
FC     = gfortran
FFLAGS = -Wall -g -ffree-form
TESTS = expr_module_test

all: $(TESTS)

expr_module_test: expr_module.o expr_module_test.o
	$(FC) $(FFLAGS) -o $@ $^

flisp: flisp.o expr_module.o
	$(FC) $(FFLAGS) -o $@ $^

flisp.o expr_module_test.o: expr_module.o

clean:
	rm -f flisp $(TESTS) *.o *.mod
