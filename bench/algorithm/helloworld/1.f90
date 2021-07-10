program hello
    implicit none

    character(len=64)             :: argv

    call get_command_argument(1, argv)
    write(*, '(a)', advance='no') 'Hello world '
    write(*, '(a)', advance='no') trim(argv)
    write(*, '(a)', advance='no') '!'
    print *
end program hello
