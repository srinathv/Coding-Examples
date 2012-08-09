!?>?? logical :: debug = .false.
!?>?? integer :: counter = 0
!?>?? macro :: fubar( x) = f?x?cked up beyond ALL recognition
!?>??cmdline
! fAcked up beyond ALL recognition
!?>??text :: foo(word)
!?>?? counter = counter + 1
!?>?? message 'copy number ', counter, counter > 1
!?>! ?counter?
!?>WHAT IS THE ?WORD? WORD
!?>?? if( debug )then
!?>! what? me worry!
!?>?? else
!?>! just do it
!?>?? end if
!?>?? end text foo
!?>?? document
!
! Preprocessor executed: 2012/04/07 07:07:14.034
!
! Preprocessor command line: ../coco text2
! Preprocessor set file: <no set file>
! Preprocessor log file: <stderr>
! Preprocessor version: $Id: coco.f90,v 2.8 2011/12/23 20:39:40 dan Exp dan $
!
! Source file: text2.fpp line: 17
! Compile file: text2.f90
! Include path: .
!
! User: dan
! Current directory: /Users/dan/Projects/Preprocessor/Tests
!
!  copy this
!?>?? copy :: foo(what)
!?>?? ! text foo
!?>?? counter = counter + 1
!?>?? message 'copy number ', counter, counter > 1
!?> copy number 1 .false.
! 1
WHAT IS THE what WORD
!?>?? if( debug )then
!?>! what? me worry!
!?>?? else
! just do it
!?>?? end if
!?>?? ! end text foo
!?>?? debug = .true.
!?>?? copy :: foo(ME)
!?>?? ! text foo
!?>?? counter = counter + 1
!?>?? message 'copy number ', counter, counter > 1
!?> copy number 2 .true.
! 2
WHAT IS THE ME WORD
!?>?? if( debug )then
! what? me worry!
!?>?? else
!?>! just do it
!?>?? end if
!?>?? ! end text foo
!?>?? copy :: foo(worry)
!?>?? ! text foo
!?>?? counter = counter + 1
!?>?? message 'copy number ', counter, counter > 1
!?> copy number 3 .true.
! 3
WHAT IS THE worry WORD
!?>?? if( debug )then
! what? me worry!
!?>?? else
!?>! just do it
!?>?? end if
!?>?? ! end text foo
!?>??symbols
!?>?? report
!?>?? This was produced using the following SET file
