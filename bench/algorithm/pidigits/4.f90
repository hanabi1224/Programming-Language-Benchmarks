! The Computer Language Benchmarks Game
! https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
!
! Contributed by Tyler Funnell
!   Based heavily on C version by Mr Ledrug, and Fortran version by
!   Andrei Jirnyi
! compilation: ifort -O2 -xHost -ipo pidigits.f90 -lgmp

module gmp_mod
  ! declaring the GMP functions...
  use iso_c_binding
  type, bind(C) :: mpz_t 
     private
     integer :: mp_alloc
     integer :: mp_size
     type(c_ptr) :: mp_d  ! a pointer
  end type mpz_t
  
  interface
     ! int mpz_cmp (mpz_t op1, mpz_t op2)
     integer(c_int) function  mpz_cmp(op1, op2) bind(C, name='__gmpz_cmp')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
     end function mpz_cmp

     ! void mpz_init (mpz_t integer)
     subroutine mpz_init(op) bind(C,name='__gmpz_init')
       import
       type(mpz_t), intent(inout) :: op
     end subroutine mpz_init

     ! void mpz_init_set_ui (mpz_t rop, unsigned long int op)
     subroutine mpz_init_set_ui(op, N) bind(C, name='__gmpz_init_set_ui')
       import
       type(mpz_t), intent(inout) :: op
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_init_set_ui

     ! unsigned long int mpz_get_ui (mpz_t op)
     integer function  mpz_get_ui(op1) bind(C, name='__gmpz_get_ui')
       import
       type(mpz_t), intent(inout) :: op1
     end function mpz_get_ui

     ! void mpz_add (mpz_t rop, mpz_t op1, mpz_t op2)
     subroutine mpz_add(op1, op2, op3) bind(C, name='__gmpz_add')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       type(mpz_t), intent(inout) :: op3
     end subroutine mpz_add

     ! void mpz_mul_ui (mpz_t rop, mpz_t op1, unsigned long int op2)
     subroutine mpz_mul_ui(op1, op2, N) bind(C, name='__gmpz_mul_ui')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_mul_ui

     ! void mpz_submul_ui (mpz_t rop, mpz_t op1, unsigned long int op2)
     subroutine mpz_submul_ui(op1, op2, N) bind(C, name='__gmpz_submul_ui')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_submul_ui

     ! void mpz_addmul_ui (mpz_t rop, mpz_t op1, unsigned long int op2)     
     subroutine mpz_addmul_ui(op1, op2, N) bind(C, name='__gmpz_addmul_ui')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       integer(c_long), value, intent(in) :: N
     end subroutine mpz_addmul_ui

     ! void mpz_tdiv_q (mpz_t rop, mpz_t op1, mpz_t op2)     
     subroutine mpz_tdiv_q(op1, op2, op3) bind(C, name='__gmpz_tdiv_q')
       import
       type(mpz_t), intent(inout) :: op1
       type(mpz_t), intent(inout) :: op2
       type(mpz_t), intent(inout) :: op3
     end subroutine mpz_tdiv_q
  end interface
end module gmp_mod

program pidigits
    use iso_c_binding
    use gmp_mod
    implicit none

    type(mpz_t) :: tmp1, tmp2, acc, den, num
    integer(c_long) :: d, k = 0, i = 0, j = 0, n = 10000
    character(len=15) :: fmtstr
    integer(8) :: intout = 0
    character(len=10) :: argv, strout

    call getarg(1, argv)
    read(argv, *) n

    call mpz_init(tmp1)
    call mpz_init(tmp2)

    call mpz_init_set_ui(acc, 0_c_long)
    call mpz_init_set_ui(den, 1_c_long)
    call mpz_init_set_ui(num, 1_c_long)

    do
        k = k + 1
        call next_term(k)

        if (mpz_cmp(num, acc) > 0) cycle

        d = extract_digit(3_c_long)
        if (d /= extract_digit(4_c_long)) cycle

        i = i + 1
        j = mod(i, 10)
        intout = intout * 10 + d
        if (j == 0) then
            write(*, '(i0.10, a, i0)') intout, achar(9)//':', i
            intout = 0
        end if

        if (i >= n) exit

        call eliminate_digit(d)
    end do

    if (j /= 0) then
        write(fmtstr, '(a, i0, a)') '(i0.', j, ', a, i0)'
        write(strout, fmtstr) intout
        write(*, '(2a, i0)') adjustl(strout), achar(9)//':', i
    end if

contains
    integer function extract_digit(nth)
        integer(c_long) nth

        call mpz_mul_ui(tmp1, num, nth)
        call mpz_add(tmp2, tmp1, acc)
        call mpz_tdiv_q(tmp1, tmp2, den)
        extract_digit = mpz_get_ui(tmp1)
        return 
    end function extract_digit

    subroutine next_term(k)
        integer(c_long) k
        integer(c_long) k2

        k2 = k * 2 + 1

        call mpz_addmul_ui(acc, num, 2_c_long)
        call mpz_mul_ui(acc, acc, k2)
        call mpz_mul_ui(den, den, k2)
        call mpz_mul_ui(num, num, k)
    end subroutine next_term

    subroutine eliminate_digit(d)
        integer(c_long) d

        call mpz_submul_ui(acc, den, d)
        call mpz_mul_ui(acc, acc, 10_c_long)
        call mpz_mul_ui(num, num, 10_c_long)
    end subroutine eliminate_digit
end program pidigits