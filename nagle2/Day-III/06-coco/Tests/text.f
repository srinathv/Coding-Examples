!?>?? logical :: debug = .false.
!?>?? integer :: counter = 0
!?>??text :: foo
!?>?? counter = counter + 1
!?>?? message 'this is appearance: ', counter
!?>! ?counter?
!?>?? if( debug )then
!?>! what? me worry!
!?>?? else
!?>! just do it
!?>?? end if
!?>?? end text foo
!?>?? document
!
! Preprocessor executed: 2012/04/07 07:06:46.188
!
! Preprocessor command line: ../coco -F text
! Preprocessor set file: <no set file>
! Preprocessor log file: <stderr>
! Preprocessor version: $Id: coco.f90,v 2.8 2011/12/23 20:39:40 dan Exp dan $
!
! Source file: text.fpp line: 13
! Compile file: text.f
! Include path: .
!
! User: dan
! Current directory: /Users/dan/Projects/Preprocessor/Tests
!
!  copy this
!?>?? copy :: foo
!?>?? ! text foo
!?>?? counter = counter + 1
!?>?? message 'this is appearance: ', counter
!?> this is appearance: 1
! 1
!?>?? if( debug )then
!?>! what? me worry!
!?>?? else
! just do it
!?>?? end if
!?>?? ! end text foo
!?>?? debug = .true.
!?>?? copy :: foo
!?>?? ! text foo
!?>?? counter = counter + 1
!?>?? message 'this is appearance: ', counter
!?> this is appearance: 2
! 2
!?>?? if( debug )then
! what? me worry!
!?>?? else
!?>! just do it
!?>?? end if
!?>?? ! end text foo
!?>?? copy :: foo
!?>?? ! text foo
!?>?? counter = counter + 1
!?>?? message 'this is appearance: ', counter
!?> this is appearance: 3
! 3
!?>?? if( debug )then
! what? me worry!
!?>?? else
!?>! just do it
!?>?? end if
!?>?? ! end text foo
!?>??symbols
!?>?? This was produced using the following SET file
