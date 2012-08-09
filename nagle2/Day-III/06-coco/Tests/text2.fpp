?? logical :: debug = .false.
?? integer :: counter = 0
?? macro :: fubar( x) = f?x?cked up beyond ALL recognition
??cmdline
! ?fubar?(A)
??text :: foo(word)
?? counter = counter + 1
?? message 'copy number ', counter, counter > 1
! ?counter?
WHAT IS THE ?WORD? WORD
?? if( debug )then
! what? me worry!
?? else
! just do it
?? end if
?? end text foo
?? document
!  copy this
?? copy :: foo(what)
?? debug = .true.
?? copy :: foo(ME)
?? copy :: foo(worry)
??symbols
?? report
