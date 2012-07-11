
!  exercise pFUnit with a simple test
!  version: $Id$
!  compiles with
!  gfortran -std=f2008 -Wall -fcheck=all -I$PFUNIT Test.f90 -o Test
!  executes with
!  ./Test

program test_driver

!  use the following names from the pfunit module

use :: pfunit, only: TestSuite_type, TestResult_type, pFUnit_init, &
                     TestSuite, add, newTestResult, Run, clean, pFUnit_finalize, &
                     MODE_USE_STDOUT, testcase1step, summary

!  some functions to test

use :: test_math_funcs, only: test_cbrt, test_qurt

implicit none

!  declare a variable defining a test suite

type( testsuite_type) :: suite

!  declare a variable defining the results of a test suite

type( testresult_type) :: result

!  capture the test suite result report string

character( len= 100) :: summary_statement

!  begin execution

continue

!  start pFUnit

   call pfunit_init()

!  initialize the suite variable

   suite = testsuite( 'test it')

!  add the procedure to test cbrt to the test suite

   call add( suite, testcase1step( 'cbrt', test_cbrt))

!  add the procedure to test qurt to the test suite

   call add( suite, testcase1step( 'qurt', test_qurt))

!  initialize the result variable

   result = newtestresult( mode= MODE_USE_STDOUT)

!  run the test suite with the results to go to the result variable

   call run( suite, result)

!  get the summary statement from the result variable

   summary_statement = summary( result)

!  and write it to stdout

   write( unit= *, fmt= *) trim( summary_statement)

!  clean the suite variable

   call clean( suite)

!  clean the result variable

   call clean( result)

!  stop pFUnit

   call pfunit_finalize()

!  stop noting expected end

stop 'normal completion'

end program test_driver
