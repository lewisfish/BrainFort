program brainfuckintrp

    !still breaks on hello world evil test...

    use stack
    use iso_fortran_env, only : stdout => output_unit

    implicit none
    
    character(len=8), parameter   :: chars = '+-<>[],.'!brainfuck character set
    character(len=256)            :: arg
    character(len=:), allocatable :: string, filename
    character(len=1)              :: code

    type(stack_var) :: braket
               !30000 due to original specification
    integer :: array(0:30000), iptr, io, u, dptr, filesize, i

    logical :: debug


    debug = .false.
    io = command_argument_count()

    if(io == 0)then
        print*,'Usage: ./brainfuck [file] [options]'
        print*,' '
        print*,'Options: any option is currently treated as a debug flag'
        stop
    end if

    if(io == 2)debug=.true.

    do i = 1, io !get cmd line arguments

        call get_command_argument(i, arg)

        if(len_trim(arg) == 0)then
            stop 'No input file'
        end if
        if(i == 1)then
            filename = trim(arg)    
        end if
    end do

    open(newunit=u,file=filename, status='old', iostat=io)
    if(io /= 0)then
        stop 'No such file'
    end if

    inquire(file=filename, size=filesize)
    allocate(character(len=filesize) :: string)!read file into string. caveat that file has to be on one line...
    read(u,'(A)',iostat=io)string
    close(u)

    iptr = 1
    dptr = 0

    do
        if(iptr == len(string) + 1)exit
        code = string(iptr:iptr)

        if(scan(code, chars) == 0)then!if character not in brainfuck set the skip
            iptr = iptr + 1
            cycle
        end if

        if(debug)then!debug print statments
            print*,iptr,code,dptr,array(dptr)
            read(*,*)
        end if

        !do brainfuck interpret
        if(code == '>')then
            dptr = dptr + 1
            iptr = iptr + 1
        end if

        if(code == '<')then
            dptr = dptr - 1
            iptr = iptr + 1
        end if

        if(code == '+')then
            array(dptr) = array(dptr) + 1
            iptr = iptr + 1
        end if

        if(code == '-')then
            array(dptr) = array(dptr) - 1
            iptr = iptr + 1
        end if

        if(code == '.')then
            write(stdout,'(A)',advance='no')char(array(dptr))
            iptr = iptr + 1
        end if  

        if(code == ',')then
            print*,'enter:',code
            read(*,*)io
            array(dptr) = array(dptr) + io
            iptr = iptr + 1
        end if

        if(code == '[')then
            if(array(dptr) /= 0)then
                call braket%push(iptr)
                iptr = iptr + 1
                continue
            else
                !jump
                call braket%push(iptr)
                call find_braket(string, iptr, io)
                iptr = io
                iptr = iptr + scan(string(iptr:), ']')-1
            end if
        end if

        if(code == ']')then
            if(array(dptr) == 0)then
                iptr = iptr + 1
                io = braket%pop()
                continue
            else
                !jump
                iptr = braket%pop()
            end if
        end if
    end do

    contains

    subroutine find_braket(string, oldiptr, newiptr)
    !
    !   get matching brackets for [ ] jumps
    !
        implicit none

        character(*), intent(IN) :: string
        integer :: oldiptr, newiptr
        integer :: i, left
        character(len=1) :: char
        character(len=:), allocatable :: tmp

        tmp = string(oldiptr:)
        left = 0

        do i = 2, len(tmp)
            char = tmp(i:i)
            if(char == '[')left = left + 1
            if(char == ']')then
                if(left > 0)then
                    left = left - 1
                elseif(left == 0)then
                    newiptr = i + oldiptr
                    exit
                end if
            end if
        end do
    end subroutine find_braket
end program brainfuckintrp