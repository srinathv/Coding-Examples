
!  the traditional 'Hello, world!' first program
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all -fcoarray=single Hello_World.f90 -o Hello_World
!  executes with
!  ./Hello_World

!  give the main program a name
!  this name must match the name on the end program statement
!  matching names makes error reporting more informative

program hello_world

!  separate declarations from executables
!  (no declarations here but there might have been)

continue

!  writes the string to the output unit
!  * is a special unit designator
!  when used in a write statement it means the default output unit
!  (when used in a read statement it means the default input unit)
!  * is a special format designator
!  it is called 'list directed' and means the format is set by the list item
!  the string 'Hello, world!' is the list item that is written

   write( unit= *, fmt= *) 'Image ', this_image(), ' says "Hello, world!"'

!  stop the program execution
!  the end statement would have stopped execution anyway
!  but it's nice to leave a message that all is well

stop 'normal exit'

!  ends the program
!  the name must match the name on the program statement

end program hello_world
