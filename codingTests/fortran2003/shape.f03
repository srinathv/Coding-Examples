        module shape_mod
        private    ! hide the type-bound procedure implementation procedures
        public :: shape, constructor   ! allow access to shape & constructor procedure
        type shape
           private               ! hide the underlying details
                integer :: color
                logical :: filled
                integer :: x
                integer :: y
           contains
           private                 ! hide the type bound procedures by default
           procedure :: initShape  ! private type-bound procedure
                procedure, public :: isFilled ! allow access to isFilled type-bound procedure
                procedure, public :: print ! allow access to print type-bound procedure
        end type shape
        contains

        logical function isFilled(this)
        class(shape) :: this

        isFilled = this%filled

        end function
         
        function constructor(color, filled, x, y)
        type(shape) :: constructor
        integer :: color
        logical :: filled
        integer :: x
        integer :: y
         call constructor%initShape(color, filled, x, y)
        end function
        subroutine initShape(this, color, filled, x, y)
        ! initialize shape objects
        class(shape) :: this
        integer :: color
        logical :: filled
        integer :: x
        integer :: y

        this%color = color
        this%filled = filled
        this%x = x
        this%y = y
        end subroutine

        subroutine print(this)
        class(shape) :: this
        print *, this%color, this%filled, this%x, this%y
          
        end subroutine
        end module
