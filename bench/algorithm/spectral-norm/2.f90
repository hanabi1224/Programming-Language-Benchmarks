! The Computer Language Benchmarks Game
! https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
!
! contributed by Steve Decker based on the version by Simon Geard
! compilation:
!   g95 -O1 -fomit-frame-pointer -funroll-loops spectral_norm.f90
!   ifort -ipo -O3 -static spectral_norm.f90
module norm_subs
  implicit none

  integer, parameter :: dp = selected_real_kind(12)

contains

  ! Return element i,j of infinite matrix A
  pure real(dp) function A(i, j)
    integer, intent(in) :: i, j

    a = 1.d0 / ((i+j-2) * (i+j-1)/2 + i)
  end function A

  ! Multiply v by A
  pure function MultiplyAv(v) result (Av)
    real(dp), dimension(:), intent(in) :: v
    real(dp), dimension(size(v))       :: Av
    
    integer :: n, i, j

    n = size(v)
    Av = 0.d0
    do i = 1, n
       do j = 1, n
          Av(i) = Av(i) + A(i,j) * v(j)
       end do
    end do
  end function MultiplyAv
       
  ! Multiply v by A transpose
  pure function MultiplyAtv(v) result (Atv)
    real(dp), dimension(:), intent(in) :: v
    real(dp), dimension(size(v))       :: Atv

    integer :: n, i, j

    n = size(v)
    Atv = 0.d0
    do i = 1, n
       do j = 1, n
          Atv(i) = Atv(i) + A(j,i) * v(j)
       end do
    end do
  end function MultiplyAtv

  ! Multiply v by A and then by A transpose
  pure function MultiplyAtAv(v) result (AtAv)
    real(dp), dimension(:), intent(in) :: v
    real(dp), dimension(size(v))       :: AtAv
    
    AtAv = MultiplyAtv(MultiplyAv(v))
  end function MultiplyAtAv
end module norm_subs

program spectral_norm
  use norm_subs
  implicit none

  real(dp), dimension(:), allocatable :: u, v
  integer          :: i, n
  character(len=6) :: argv

  call get_command_argument(1, argv)
  read(argv, *) n

  allocate(u(n), v(n))
  u = 1.d0
  do i = 1, 10
     v = MultiplyAtAv(u)
     u = MultiplyAtAv(v)
  end do

  write(*, "(f0.9)") sqrt(dot_product(u,v) / dot_product(v,v))
  deallocate(u, v)
end program spectral_norm