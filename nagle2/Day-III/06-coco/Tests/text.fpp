?? logical :: debug = .false.
?? integer :: counter = 0
??text :: foo
?? counter = counter + 1
?? message 'this is appearance: ', counter
! ?counter?
?? if( debug )then
! what? me worry!
?? else
! just do it
?? end if
?? end text foo
?? document
!  copy this
?? copy :: foo
?? debug = .true.
?? copy :: foo
?? copy :: foo
??symbols
