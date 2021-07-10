! The Computer Language Benchmarks Game
! https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
!
!   contributed by Simon Geard, translated from  Mark C. Williams nbody.java
!   modified by Brian Taylor
!
! ifort -fast -o nbody nbody.f90
!

program nbody
  implicit none

  real*8, parameter :: tstep = 0.01d0
  real*8, parameter ::  PI = 3.141592653589793d0
  real*8, parameter ::  SOLAR_MASS = 4 * PI * PI
  real*8, parameter ::  DAYS_PER_YEAR = 365.24d0

  type body
     real*8 :: x, y, z, vx, vy, vz, mass
  end type body

  type(body), parameter :: jupiter = body( &
       4.84143144246472090d0,    -1.16032004402742839d0, &
       -1.03622044471123109d-01, 1.66007664274403694d-03 * DAYS_PER_YEAR, &
       7.69901118419740425d-03 * DAYS_PER_YEAR, &
       -6.90460016972063023d-05 * DAYS_PER_YEAR, &
       9.54791938424326609d-04 * SOLAR_MASS)

  type(body), parameter :: saturn = body( &
       8.34336671824457987d+00, &
       4.12479856412430479d+00, &
       -4.03523417114321381d-01, &
       -2.76742510726862411d-03 * DAYS_PER_YEAR, &
       4.99852801234917238d-03 * DAYS_PER_YEAR, &
       2.30417297573763929d-05 * DAYS_PER_YEAR, &
       2.85885980666130812d-04 * SOLAR_MASS)

  type(body), parameter :: uranus = body( &
       1.28943695621391310d+01, &
       -1.51111514016986312d+01, &
       -2.23307578892655734d-01, &
       2.96460137564761618d-03 * DAYS_PER_YEAR, &
       2.37847173959480950d-03 * DAYS_PER_YEAR, &
       -2.96589568540237556d-05 * DAYS_PER_YEAR, &
       4.36624404335156298d-05 * SOLAR_MASS )

  type(body), parameter :: neptune = body( &
       1.53796971148509165d+01, &
       -2.59193146099879641d+01, &
       1.79258772950371181d-01, &
       2.68067772490389322d-03 * DAYS_PER_YEAR, &
       1.62824170038242295d-03 * DAYS_PER_YEAR, &
       -9.51592254519715870d-05 * DAYS_PER_YEAR, &
       5.15138902046611451d-05 * SOLAR_MASS)

  type(body), parameter :: sun = body(0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
        0.0d0, SOLAR_MASS)

  integer, parameter :: nb = 5

  real*8, parameter :: mass(nb) = (/ sun%mass, jupiter%mass, saturn%mass, &
        uranus%mass, neptune%mass /)

  integer :: num, i
  character(len=8) :: argv

  real*8 :: e, x(3,nb), v(3,nb)

  x(:,1) = (/ sun%x, sun%y, sun%z /)
  x(:,2) = (/ jupiter%x, jupiter%y, jupiter%z /)
  x(:,3) = (/ saturn%x, saturn%y, saturn%z /)
  x(:,4) = (/ uranus%x, uranus%y, uranus%z /)
  x(:,5) = (/ neptune%x, neptune%y, neptune%z /)

  v(:,1) = (/ sun%vx, sun%vy, sun%vz /)
  v(:,2) = (/ jupiter%vx, jupiter%vy, jupiter%vz /)
  v(:,3) = (/ saturn%vx, saturn%vy, saturn%vz /)
  v(:,4) = (/ uranus%vx, uranus%vy, uranus%vz /)
  v(:,5) = (/ neptune%vx, neptune%vy, neptune%vz /)

  call getarg(1, argv)
  read (argv,*) num

  call offsetMomentum(1, v, mass)
  e = energy(x, v, mass)
  write (*,'(f12.9)') e
  do i = 1, num
     call advance(tstep, x, v, mass)
  end do
  e = energy(x, v, mass)
  write (*,'(f12.9)') e

contains

  pure subroutine offsetMomentum(k, v, mass)
    integer, intent(in) :: k
    real*8, dimension(3,nb), intent(inout) :: v
    real*8, dimension(nb), intent(in) :: mass
    real*8 :: px, py, pz
    integer :: i
    px = 0.0d0
    py = 0.0d0
    pz = 0.0d0
    do i = 1, nb
       px = px + v(1,i) * mass(i)
       py = py + v(2,i) * mass(i)
       pz = pz + v(3,i) * mass(i)
    end do
    v(1,k) = -px / SOLAR_MASS
    v(2,k) = -py / SOLAR_MASS
    v(3,k) = -pz / SOLAR_MASS
  end subroutine offsetMomentum


  pure subroutine advance(tstep, x, v, mass)
  real*8, intent(in) :: tstep
  real*8, dimension(3,nb), intent(inout) :: x, v
  real*8, dimension(nb), intent(in) :: mass

  real*8 :: dx, dy, dz, distance, mag
  integer :: i, j

  do i = 1, nb
     do j = i + 1, nb
        dx = x(1,i) - x(1,j)
        dy = x(2,i) - x(2,j)
        dz = x(3,i) - x(3,j)

        distance = sqrt(dx**2 + dy**2 + dz**2)
        mag = tstep / distance**3

        v(1,i) = v(1,i) - dx * mass(j) * mag
        v(2,i) = v(2,i) - dy * mass(j) * mag
        v(3,i) = v(3,i) - dz * mass(j) * mag

        v(1,j) = v(1,j) + dx * mass(i) * mag
        v(2,j) = v(2,j) + dy * mass(i) * mag
        v(3,j) = v(3,j) + dz * mass(i) * mag
     end do
  end do

  do i = 1, nb
     x(1,i) = x(1,i) + tstep * v(1,i)
     x(2,i) = x(2,i) + tstep * v(2,i)
     x(3,i) = x(3,i) + tstep * v(3,i)
  end do
  end subroutine advance


  pure function energy(x, v, mass)
    real*8 :: energy
    real*8, dimension(3,nb), intent(in) :: x, v
    real*8, dimension(nb), intent(in) :: mass

    real*8 :: dx, dy, dz, distance
    integer :: i, j

    energy = 0.0d0
    do i = 1, nb
       energy = energy + 0.5d0 * mass(i) * (v(1,i)**2 + v(2,i)**2 + v(3,i)**2)
       do j = i + 1, nb
          dx = x(1,i) - x(1,j)
          dy = x(2,i) - x(2,j)
          dz = x(3,i) - x(3,j)
          distance = sqrt(dx**2 + dy**2 + dz**2)
          energy = energy - (mass(i) * mass(j)) / distance;
       end do
    end do
  end function energy

end program nbody