# -*-Makefile-*- for FLISP
##
FC     = gfortran
FFLAGS = -Wall -g -ffree-form

flisp: flisp.o expr_module.o
	$(FC) $(FFLAGS) -o $@ $^

flisp.o: expr_module.o

clean:
	rm -f flisp *.o *.mod
