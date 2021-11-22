! The Computer Language Benchmarks Game
! https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
!
! Original C contributed by Sebastien Loisel
! Conversion to C++ by Jon Harrop
! OpenMP parallelize by The Anh Tran
! Add SSE by The Anh Tran
! Reconversion into C by Dan Farina
! Conversion to Fortran by Brian Taylor

program main
  !$ use omp_lib
  implicit none

  character(len=6) :: argv
  integer :: n
  real*8, allocatable :: u(:), v(:), tmp(:)
  integer :: n2, r_begin, r_end
  real*8 uv, vv
  integer :: i, tid, tcount, chunk, ite

  call get_command_argument(1, argv)
  read (argv, *) n

  n2 = n / 2

  allocate(u(0:n-1), v(0:n-1), tmp(0:n-1))

  uv = 0.d0
  vv = 0.d0

  !$omp parallel default(shared) private(i,tid,tcount,chunk,r_begin,r_end)

  !$omp do schedule(static)
  do i = 0, n - 1
     u(i) = 1.d0
  end do

  tid = omp_get_thread_num()
  tcount = omp_get_num_threads()
  chunk = n / tcount

  r_begin = tid * chunk
  if (tid < tcount - 1) then
     r_end = r_begin + chunk - 1
  else
     r_end = n - 1
  end if

  do i = 1, 10
     call eval_AtA_times_u(r_begin, r_end, u, v)
     call eval_AtA_times_u(r_begin, r_end, v, u)
  end do

  !$omp do schedule(static) reduction(+:uv) reduction(+:vv)
  do i = 0, n - 1
     uv = uv + u(i) * v(i)
     vv = vv + v(i) * v(i)
  end do
  !$omp end do nowait

  !$omp end parallel

write (*, "(f0.9)") sqrt(uv / vv)

contains

! Return element (i,j) of matrix A
pure function eval_A(i, j)
real*8 :: eval_A
integer, intent(in) :: i, j
real*8 :: di, dj
integer :: d
di = real(i,8)
dj = real(j,8)
eval_A = 1.d0 / (0.5d0 * ((di + dj) * (di + dj + 1.d0)) + di + 1.d0)
end function


subroutine eval_A_times_u(r_begin, r_end, src, dest)
integer, intent(in) :: r_begin, r_end
real*8, intent(in) :: src(0:)
real*8, intent(out) :: dest(0:)
real*8 sum1
integer :: i, j
do i = r_begin, r_end
  sum1 = 0.d0
  do j = 0, n - 1
    sum1 = sum1 + src(j) * eval_A(i, j)
  end do
  dest(i) = sum1
end do
end subroutine


subroutine eval_At_times_u(r_begin, r_end, src, dest)
integer, intent(in) :: r_begin, r_end
real*8, intent(in) :: src(0:)
real*8, intent(out) :: dest(0:)
real*8 sum1
integer :: i, j
do i = r_begin, r_end
  sum1 = 0.d0
  do j = 0, n - 1
    sum1 = sum1 + src(j) * eval_A(j, i)
  end do
  dest(i) = sum1
end do
end subroutine


subroutine eval_AtA_times_u(r_begin, r_end, src, dest)
integer, intent(in) :: r_begin, r_end
real*8, intent(in) :: src(0:)
real*8, intent(out) :: dest(0:)
call eval_A_times_u(r_begin, r_end, src, tmp)
!$omp barrier
call eval_At_times_u(r_begin, r_end, tmp, dest)
!$omp barrier
end subroutine

end program main
