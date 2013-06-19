! FLISP: LISP written in Fortran
!!
program flisp
  use expr_module
  implicit none
  type(expr_type), pointer :: f, t, a, b, p

  f => nil
  call show(f)        !=> ()

  t => true
  call show(t)        !=> T

  call show(logical_not(f))
                      !=> T
  call show(logical_not(t))
                      !=> ()

  write(6,'(a)') '-----------------------------------------'

  a => new_atom_integer(1)
  call show(a)        !=> 1

  b => new_atom_integer(2)
  call show(b)        !=> 2

  write(6,'(a)') '-----------------------------------------'

  p => new_pair(a, b)

  call show(car(p))   !=> 1
  call show(cdr(p))   !=> 2

  call show(p)        !=> <unknown>, but it should be pretty print.

  a => cdr(a)         ! ERROR

  write(6,'(a)') '-----------------------------------------'

  a => new_atom_float(0.1d0)
  call show(a)        !=> 0.1

  b => new_atom_float(0.2d0)
  call show(b)        !=> 0.2
end program flisp
