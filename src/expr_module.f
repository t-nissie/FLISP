! expr_module.f -*-f90-*-
! Author: Takeshi NISHIMATSU
!!
module expr_module
  use, intrinsic :: iso_c_binding
  implicit none
  enum, bind(c)
    enumerator :: TYPE_NIL
    enumerator :: TYPE_TRUE
    enumerator :: TYPE_PAIR
    enumerator :: TYPE_INTEGER
    enumerator :: TYPE_STRING
    enumerator :: TYPE_SYMBOL
    enumerator :: TYPE_CLOSURE
    enumerator :: TYPE_PRIMITIVE
    enumerator :: TYPE_INTERNAL
    enumerator :: TYPE_FLOAT
    enumerator :: TYPE_COMPLEX
  end enum

  type expr_type
     integer :: type
     type(c_ptr)              :: data
     type(expr_type), pointer :: next
  end type expr_type

  type(expr_type), target :: nil   = expr_type(TYPE_NIL,  C_NULL_PTR, null())
  type(expr_type), target :: true  = expr_type(TYPE_TRUE, C_NULL_PTR, null())

  ! type closure_type
  !    type(expr_type),        pointer :: param
  !    type(expr_type),        pointer :: body
  !    type(environment_type), pointer :: env
  ! end type closure_type

  ! type environment_type
  !    !type(hash_table),       pointer :: table
  !    type(environment_type), pointer :: outer
  ! end type environment_type

contains
  function new_atom(type, data) result(a)
    implicit none
    integer, intent(in)      :: type
    type(c_ptr), intent(in)  :: data
    type(expr_type), pointer :: a
    allocate(a)
    a%type = type
    a%data = data
  end function new_atom

  function new_atom_integer(i) result(a)
    implicit none
    integer, intent(in)      :: i
    type(expr_type), pointer :: a
    integer, pointer         :: p
    allocate(a)
    allocate(p)
    a%type = TYPE_INTEGER
    p = i
    a%data = c_loc(p)
  end function new_atom_integer

  function new_atom_float(f) result(a)
    implicit none
    real*8, intent(in)       :: f
    type(expr_type), pointer :: a
    real*8, pointer          :: p
    allocate(a)
    allocate(p)
    a%type = TYPE_FLOAT
    p = f
    a%data = c_loc(p)
  end function new_atom_float

  function new_pair(head, tail) result(p)
    implicit none
    type(expr_type), pointer, intent(in) :: head
    type(expr_type), pointer, intent(in) :: tail
    type(expr_type), pointer             :: p
    allocate(p)
    p%type = TYPE_PAIR
    p%data = c_loc(head)
    p%next => tail
  end function new_pair

! (a) is (a . ())

  ! function new_closure(params, body, env) result(a)
  !   implicit none
  !   type(expr_type),        pointer, intent(in) :: params
  !   type(expr_type),        pointer, intent(in) :: body
  !   type(environment_type), pointer, intent(in) :: env
  !   type(expr_type), pointer                    :: a
  !   integer*8                                   :: data(4)
  !   type(closure_type),     pointer             :: c
  !   allocate(c)
  !   c%param => params
  !   c%body  => body
  !   c%env   => env
  !   data = transfer(c, data)
  !   a => new_atom(TYPE_CLOSURE, data)
  ! end function new_closure

  ! function list(n, ary) result(l)
  !   implicit none
  !   integer                              :: n
  !   type(expr_type), pointer, intent(in) :: ary
  !   type(expr_type), pointer             :: l
  !   if (n.eq.0) then
  !      l => nil
  !   else
  !      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !   end if
  ! end function list

  function logical_not(expr) result(n)
    implicit none
    type(expr_type), pointer, intent(in) :: expr
    type(expr_type), pointer             :: n
    allocate(n)
    select case(expr%type)
    case(TYPE_NIL)
       n => true
    case default
       n => nil
    end select
  end function logical_not

  function cdr(expr) result(n)
    implicit none
    type(expr_type), pointer, intent(in) :: expr
    type(expr_type), pointer             :: n
    if (expr%type.eq.TYPE_PAIR) then
       n => expr%next
    else
       write(6,'(a)') '*** ERROR: pair required, but got something else'
    end if
  end function cdr

  function car(expr) result(r)
    implicit none
    type(expr_type), pointer, intent(in) :: expr
    type(expr_type), pointer             :: r
    if (expr%type.eq.TYPE_PAIR) then
       call c_f_pointer(expr%data,r)
    else
       r => nil
    end if
  end function car

  subroutine show(expr)
    implicit none
    type(expr_type), pointer, intent(in) :: expr
    integer,    pointer :: p_i
    real*8,     pointer :: p_r
    complex*16, pointer :: p_c
    select case(expr%type)
    case(TYPE_NIL)
       write(6,'(a)') '()'
    case(TYPE_TRUE)
       write(6,'(a)') 'T'
    case(TYPE_INTEGER)
       call c_f_pointer(expr%data, p_i)
       write(6,'(i10)') p_i
    case(TYPE_FLOAT)
       call c_f_pointer(expr%data, p_r)
       write(6,'(e22.15)') p_r
    case(TYPE_COMPLEX)
       call c_f_pointer(expr%data, p_c)
       write(6,'(a,e22.15,e23.15,a)') '(complex ', p_c, ')'
    case(TYPE_CLOSURE, TYPE_PRIMITIVE, TYPE_INTERNAL)
       write(6,'(a)') '<procedure>'
    case default
       write(6,'(a)') '<unknown>'
    end select
  end subroutine show
end module expr_module
