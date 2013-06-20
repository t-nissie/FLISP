! parse_module.f -*-f90-*-
! Author: Takeshi NISHIMATSU
!!
module parse_module
  implicit none
  enum, bind(c)
    enumerator :: sentinel
    enumerator :: terminal
    enumerator :: l_paren
    enumerator :: r_paren
    enumerator :: quote
    enumerator :: quasiquote
    enumerator :: unquote
    enumerator :: unquote_splicing
    enumerator :: number
    enumerator :: string
    enumerator :: symbol
  end enum

  integer,   private :: token
  integer,   private :: ex_token = sentinel
  character, private, pointer :: value
  character, private, pointer :: ex_value
end module parse_module
