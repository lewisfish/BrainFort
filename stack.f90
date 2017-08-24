module stack

    implicit none 

    ! Define the data-structure to hold the data
    type :: stack_var
        integer, allocatable :: data(:)
        integer              :: size = 0
    contains
        procedure :: pop   => pop_fn
        procedure :: push  => push_sub
        procedure :: peek  => peek_fn
        procedure :: show  => show_sub
        procedure :: empty => empty_fn
        procedure :: sizeof  => size_fn
    end type stack_var

    ! Set the size of allocated memory blocks
    integer, parameter :: block_size = 10
    public
    private :: pop_fn, push_sub, peek_fn, empty_fn, show_sub, size_fn

contains
 
    subroutine push_sub(this, e)

        implicit none

        class(stack_var)     :: this
        integer, intent(IN)  :: e
        integer, allocatable :: wk(:)

        if(.not. allocated(this%data))then
            ! Allocate space if not yet done
            allocate(this%data(block_size))
        elseif(this%size == size(this%data))then
            ! Grow the allocated space
            allocate(wk(size(this%data)+block_size))
            wk(1:this%size) = this%data
            call move_alloc(wk,this%data)
        end if

        ! Store the data in the stack
        this%size = this%size + 1
        this%data(this%size) = e
    end subroutine push_sub
 

    integer function pop_fn(this)

        implicit none

        class(stack_var) :: this

        if(this%size == 0 .or. .not. allocated(this%data))then
            pop_fn = 0
            return
        end if
        pop_fn = this%data(this%size)
        this%size = this%size - 1
    end function pop_fn
 

    integer function peek_fn(this)
        
        implicit none

        class(stack_var) :: this
        if (this%size == 0 .or. .not. allocated(this%data)) then
            peek_fn = 0
            return
        end if
        peek_fn = this%data(this%size)
    end function peek_fn
 

    subroutine show_sub(this)

        implicit none

        class(stack_var) :: this
        integer :: i

        if(this%empty())then
            print*,'empty'
            return
        end if
        
        do i = this%size, 1, -1
            print*,this%data(i)
        end do
    end subroutine show_sub


    integer function size_fn(this)

        implicit none

        class(stack_var) :: this

        size_fn = this%size

    end function size_fn

    logical function empty_fn(this)

        implicit none

        class(stack_var) :: this
        empty_fn = (this%size == 0 .or. .not. allocated(this%data))
    end function empty_fn
end module stack