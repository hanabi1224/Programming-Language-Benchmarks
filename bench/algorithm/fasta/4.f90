! The Computer Language Benchmarks Game
! https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
!
! fasta implementation - translated from the lua program
! contributed by Simon Geard, 18/1/05
! modified by Andrei Jirnyi
!
! Building info.
! ==============
!
! Linux  - using the Intel Fortran90 compiler:
!
!          ifort -fast -opt-streaming-stores always fasta3.f90
!          time ./a.out 25000000 > /dev/null


module line_by_line
  interface
     function puts(str) bind(C)
       use, intrinsic :: ISO_C_BINDING
       integer(kind=c_int) :: puts
       character(kind=c_char), dimension(*) :: str
     end function puts
  end interface
end module line_by_line

program fasta
  use iso_fortran_env
  use line_by_line

 implicit none
  integer num
  character(len=8) argv
  integer, parameter :: IM = 139968, IA = 3877, IC = 29573
  integer, parameter :: LW=60
  character(len=*), parameter :: alu = &
'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' // &
'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' // &
'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' // &
'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' // &
'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' // &
'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' // &
'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA'
  character(len=1), parameter :: EOL = achar(10)

  type pair
     character(len=1),dimension(:),allocatable :: c
     real,dimension(:),allocatable :: p
  end type pair
  
  type(pair) :: homosapiens
  type(pair) :: iub
  
  iub = mkpair(15, c='acgtBDHKMNRSVWY',&
       &       p=[.27,.12,.12,.27,(.02,num=1,11)])
  homosapiens = mkpair(4, c='acgt',&
                     & p = [0.3029549426680, 0.1979883004921, &
                            0.1975473066391, 0.3015094502008])

  call getarg(1,argv)
  read(argv,*) num
 
  call makeRepeatFasta('ONE','Homo sapiens alu',alu,num*2)

  call makeRandomFasta('TWO','IUB ambiguity codes',iub,num*3)

  call makeRandomFasta('THREE','Homo sapiens frequency',homosapiens,num*5)

     
contains

  type(pair) function mkpair(n,c,p)
    integer, intent(in) :: n
    character(len=n) :: c
    real :: p(n), z
    integer :: i,k
    allocate(mkpair%c(0:n-1))
    allocate(mkpair%p(n-1))
    z = 0
    k = 1
    do i=1,n-1
       mkpair%c(i-1) = c(i:i)
       mkpair%p(i) = z+p(i)
       z = z+p(i)
    end do
    mkpair%c(n-1) = c(n:n)
    mkpair%p = mkpair%p 
  end function mkpair
  
  real function getRandom (max)
    real :: max
    integer, save :: last = 42
    last = mod(last * IA + IC, IM)
    getRandom = real(last)*max/IM
  end function getRandom

  subroutine makeRandomFasta(id,desc,a,n)
     character(len=*), intent(in) :: id
     character(len=*), intent(in) :: desc
     type(pair), intent(inout) :: a
     integer, intent(in) :: n
     character(len=LW) :: title
     character(len=1) :: line(LW+1) = achar(0)
     equivalence(title, line)
     integer :: j,l,istat
     character(1), dimension(0:size(a%c)-1) :: chars
     real, dimension(size(a%p)) :: probs
     
     probs = a%p * IM
     chars = a%c

     write(title,'(4a)') '>',id,' ',desc
     line(len(trim(title))+1) = achar(0)
     istat = puts(line)
     j = 0
     l = 0
     do
        j = j+1
        l = l+1
        line(j) = chars(count(getRandom(IM*1.0) >= probs))
        if(l == n) then  ! last line, exit
           line(j+1) = achar(0)
           istat = puts(line)
           exit
        end if
        if(j == LW) then ! write another line
           j = 0
           istat = puts(line)
        end if
     end do

  end subroutine makeRandomFasta

  subroutine makeRepeatFasta(id,desc,s,n)
     character(len=*), intent(in) :: id
     character(len=*), intent(in) :: desc
     character(len=*), intent(in) :: s
     integer, intent(in) :: n
     integer :: j, k, l, kn, istat
     integer, parameter :: length = 60
     character(len=LW) :: title
     character(len=1) :: line(LW+1) = achar(0)
     equivalence(title, line)
     intrinsic len

     write(title,'(4a)') '>',id,' ',desc
     line(len(trim(title))+1) = achar(0)
     istat = puts(line)

     k = 1; kn = len(s)
     
     j = 0 ! in output line
     k = 0 ! in repeat seq
     l = 0 ! generated count
     do
        j = j+1
        k = k+1
        l = l+1
        if(k>kn) k=1
        line(j) = s(k:k)
        if(l == n) then
           line(j+1) = achar(0)
           istat = puts(line)
           exit
        end if
        if(j == LW) then
           j = 0
           istat = puts(line)
        end if
     end do

  end subroutine makeRepeatFasta

end program fasta