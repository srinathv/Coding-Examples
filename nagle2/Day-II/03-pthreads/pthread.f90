! bof
! **********************************************************************
! Fortran 2003 module pthreads

! **********************************************************************
! Source Control Strings

! $Id: pthread.f03 1.1 2005/03/24 17:10:07Z Dan Exp $

! **********************************************************************
!  Copyright 2012 Dan Nagle

!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Library General Public
!   License as published by the Free Software Foundation; either
!   version 2 of the License, or (at your option) any later version.

!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Library General Public License for more details.

!   You should have received a copy of the GNU Library General Public
!   License along with this library; if not, write to the Free
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

! To report bugs, suggest enhancements, etc. to the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 2820 Lafayette Dr
!                                             Boulder CO 80305 USA

! **********************************************************************
! pthreads description

! **********************************************************************

!  pthreads uses

!     intrinsic iso_c_binding

!  pthreads constants

!  pthreads types

!     pthread_attr_t
!     pthread_t
!     pthread_condattr_t
!     pthread_cond_t
!     pthread_mutex_t
!     sched_param_t
!     pthread_key_t
!     pthread_mutexattr_t
!     pthread_mutex_t
!     pthread_once_t
!     sigset_t

!  pthreads data

!  pthreads library

!     pthread_atfork()
!     pthread_attr_destroy()
!     pthread_attr_getdetachstate()
!     pthread_attr_getinheritsched()
!     pthread_attr_getschedparam()
!     pthread_attr_getschedpolicy()
!     pthread_attr_getscope()
!     pthread_attr_getstackaddr()
!     pthread_attr_getstacksize()
!     pthread_attr_init()
!     pthread_attr_setdetachstate()
!     pthread_attr_setinheritsched()
!     pthread_attr_setschedparam()
!     pthread_attr_setschedpolicy()
!     pthread_attr_setscope()
!     pthread_attr_setstackaddr()
!     pthread_attr_setstacksize()
!     pthread_cancel()
!     pthread_cleanup_pop()
!     pthread_cleanup_push()
!     pthread_condattr_destroy()
!     pthread_condattr_getpshared()
!     pthread_condattr_init()
!     pthread_condattr_setpshared()
!     pthread_cond_broadcast()
!     pthread_cond_destroy()
!     pthread_cond_init()
!     pthread_cond_signal()
!     pthread_cond_timedwait()
!     pthread_cond_wait()
!     pthread_create()
!     pthread_detach()
!     pthread_equal()
!     pthread_exit()
!     pthread_getschedparam()
!     pthread_getspecific()
!     pthread_join()
!     pthread_key_create()
!     pthread_key_delete()
!     pthread_kill()
!     pthread_mutexattr_destroy()
!     pthread_mutexattr_getprioceiling()
!     pthread_mutexattr_getprotocol()
!     pthread_mutexattr_getpshared()
!     pthread_mutexattr_init()
!     pthread_mutexattr_setprioceiling()
!     pthread_mutexattr_setprotocol()
!     pthread_mutexattr_setpshared()
!     pthread_mutex_destroy()
!     pthread_mutex_init()
!     pthread_mutex_lock()
!     pthread_mutex_trylock()
!     pthread_mutex_unlock()
!     pthread_once()
!     pthread_self()
!     pthread_setcancelstate()
!     pthread_setcanceltype()
!     pthread_setschedparam()
!     pthread_setspecific()
!     pthread_sigmask()
!     pthread_testcancel()

! **********************************************************************

!  pthreads

!  Note that, since the pthreads standard only specifies what works
!  when pthread.h is included and the C source file is run through
!  the C preprocessor, not everything may be specified with certainty.
!  That is, you may have to change the values of some constants,
!  or the definition of some types, in order to have a working program.

!  For more on pthreads, consult the POSIX standard, or
!  Title: _Pthreads Programming_
!  Authors: Nichols, Buttlar, Farrell
!  Publisher: O'Reilly & Associates, Inc.
!  Year: 1996
!  ISBN: 1-56592-115-1

! **********************************************************************

module pthreads

! **********************************************************************

!  pthreads uses modules

use, intrinsic :: iso_c_binding

! **********************************************************************

!  explicit names

implicit none

! **********************************************************************

!  explicit exports

private

! **********************************************************************

!  pthreads RCS strings

! **********************************************************************

!  module source filename supplied by RCS

character( len= *), public, parameter :: pthreads_rcs_id = &
   '$Id: pthread.f03 1.1 2005/03/24 17:10:07Z Dan Exp $'

! **********************************************************************

!  pthreads constants

! **********************************************************************

!  The values of these constants *may* be right for your pthreads
!  implementation.  Check the pthread.h file for the correct values
!  to use.

! ----------------------------------------------------------------------

!  Since typealias has been removed from the draft, a new way
!  of making the types pthreads requires must be found.  The way
!  I have chosen is to make a derived type with a single private
!  component which is an integer array.  This is the size of that array.
!  You may need to enlarge the array for your system, check
!  the pthreads.h file for your system to find the value needed.

!  #include <pthread.h>
!  int main()
!  {
!  printf( "int: %d\n", sizeof( int));
!  printf( "pthread: %d\n", sizeof(pthread_t));
!  printf( "pthread key: %d\n", sizeof(pthread_key_t));
!  return 0;
!  }

integer, parameter :: size_of_types = 2

! ----------------------------------------------------------------------

!  A pthread is joinable if another thread can wait for it
!  via a call to pthread_join() and retrieve its exit value.
!  A pthread is detached if it isn't joinable.

integer, public, parameter :: pthread_create_detached = 0

! default

integer, public, parameter :: pthread_create_joinable = 1

!  A pthread has system scope if it is to be scheduled as an equal
!  from among all the threads within the system.  A pthread has
!  process scope if it is to be scheduled within the process only.

! default

integer, public, parameter :: pthread_scope_system = 0

integer, public, parameter :: pthread_scope_process = 1

!  A pthread may inherit its scheduling properties from its creator
!  or explicitly declare them.

! default

integer, public, parameter :: pthread_inherit_sched = 0

integer, public, parameter :: pthread_explicit_sched = 1

!  The defined scheduling policies are below.

integer, public, parameter :: sched_other = 0

integer, public, parameter :: sched_fifo = 1

integer, public, parameter :: sched_rr = 2

!  A mutex or condition variable may be known outside the process
!  of the creating thread or not.  (Some systems do not support shared.)
!  Most Fortran uses will use only process anyway.

integer, public, parameter :: pthread_process_shared = 0

! default

integer, public, parameter :: pthread_process_private = 1

!  A pthread is cancelable is another pthread can cause its exit
!  via a call to pthread_cancel().  A pthread may need to be unstoppable
!  for a time to allow it to atomically complete some code, or it may need
!  to defer acceptance of the cancel signal until it reaches
!  a cancelation point.

integer, public, parameter :: pthread_cancel_enable = 0

integer, public, parameter :: pthread_cancel_disable = 1

! default

integer, public, parameter :: pthread_cancel_deferred = 2

integer, public, parameter :: pthread_cancel_asynchronous = 3

integer, public, parameter :: pthread_canceled = 0

!  Priority policies of a mutex.  These may not be defined by your pthreads.

integer, public, parameter :: pthread_prio_inherit = 0

integer, public, parameter :: pthread_prio_protect = 1

integer, public, parameter :: pthread_prio_none = 2

! **********************************************************************

!  pthreads types

! **********************************************************************

!  These types *may* be valid for your pthreads,
!  or you may have to fix them.  Check the pthread.h file
!  for the C compiler on your target system.

!  Since your Fortran program need not manipulate variables
!  of these types, the strategy is to make a type whose
!  only component is a private integer array of sufficient size.
!  You must glean the size from the pthread.h file.
!  The bind( c) and the kind is c_int to allay alignment issues.

! ----------------------------------------------------------------------

type, bind( c) :: pthread_t

   private

   integer( kind= c_int), dimension( size_of_types) :: secret

end type pthread_t

type, bind( c) :: pthread_key_t

   private

   integer( kind= c_int), dimension( size_of_types) :: secret

end type pthread_key_t

!  This one should be right.  (It's required by the C standard
!  to be some kind of integer, c_size_t is the Fortran name of that kind.)

type, bind( c) :: size_t

   private

   integer( kind= c_size_t) :: secret

end type size_t

! ----------------------------------------------------------------------

!  The using program will need these.

public :: pthread_t
public :: pthread_key_t
public :: size_t

! ----------------------------------------------------------------------

!  When examining the pthread.h file, you may notice other types,
!  such as pthread_attr_t, etc.  Since pthreads only passes pointers
!  to these types, you should always use the Fortran c_ptr type here.
!  Variables of these types are not manipulated by the program
!  using pthreads.

!  Also, the static initializers for these types are not defined.
!  You must use calls to the init routines.

! **********************************************************************

!  pthreads data

! **********************************************************************

! **********************************************************************

!  pthread library

! **********************************************************************

!  The program will use these.

public :: pthread_atfork
public :: pthread_attr_destroy
public :: pthread_attr_getdetachstate
public :: pthread_attr_getinheritsched
public :: pthread_attr_getschedparam
public :: pthread_attr_getschedpolicy
public :: pthread_attr_getscope
public :: pthread_attr_getstackaddr
public :: pthread_attr_getstacksize
public :: pthread_attr_init
public :: pthread_attr_setdetachstate
public :: pthread_attr_setinheritsched
public :: pthread_attr_setschedparam
public :: pthread_attr_setschedpolicy
public :: pthread_attr_setscope
public :: pthread_attr_setstackaddr
public :: pthread_attr_setstacksize
public :: pthread_cancel
public :: pthread_cleanup_pop
public :: pthread_cleanup_push
public :: pthread_condattr_destroy
public :: pthread_condattr_getpshared
public :: pthread_condattr_init
public :: pthread_condattr_setpshared
public :: pthread_cond_broadcast
public :: pthread_cond_destroy
public :: pthread_cond_init
public :: pthread_cond_signal
public :: pthread_cond_timedwait
public :: pthread_cond_wait
public :: pthread_create
public :: pthread_detach
public :: pthread_equal
public :: pthread_exit
public :: pthread_getschedparam
public :: pthread_getspecific
public :: pthread_join
public :: pthread_key_create
public :: pthread_key_delete
public :: pthread_kill
public :: pthread_mutexattr_destroy
public :: pthread_mutexattr_getprioceiling
public :: pthread_mutexattr_getprotocol
public :: pthread_mutexattr_getpshared
public :: pthread_mutexattr_init
public :: pthread_mutexattr_setprioceiling
public :: pthread_mutexattr_setprotocol
public :: pthread_mutexattr_setpshared
public :: pthread_mutex_destroy
public :: pthread_mutex_init
public :: pthread_mutex_lock
public :: pthread_mutex_trylock
public :: pthread_mutex_unlock
public :: pthread_once
public :: pthread_self
public :: pthread_setcancelstate
public :: pthread_setcanceltype
public :: pthread_setschedparam
public :: pthread_setspecific
public :: pthread_sigmask
public :: pthread_testcancel

! **********************************************************************

!  Note that, in a multithreaded environment, the single C variable errno
!  cannot be used to return status information, so many of these functions
!  return the status of their (attempted) action as their return value.

! ----------------------------------------------------------------------

!  These interfaces *may* be valid for your pthreads,
!  or some names may be implemented as C macros.  Check the pthread.h file
!  for the C compiler on your target system.

!  If some of these functions are macros on your target system,
!  you must place a wrapper routine (which rewrites the macro in Fortran)
!  below the contains of this file, and replace the interface here
!  to reflect the change.

! ----------------------------------------------------------------------

interface

   integer( kind= c_int) function pthread_atfork( prepare, parent, child) bind( c)
      import :: c_funptr, c_int
      type( c_funptr), value :: prepare
      type( c_funptr), value :: parent
      type( c_funptr), value :: child
   end function pthread_atfork

   integer( kind= c_int) function pthread_attr_destroy( attr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
   end function pthread_attr_destroy

   integer( kind= c_int) function pthread_attr_getdetachstate( attr, detachstate) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: detachstate
   end function pthread_attr_getdetachstate

   integer( kind= c_int) function pthread_attr_getinheritsched( attr, inheritsched) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: inheritsched
   end function pthread_attr_getinheritsched

   integer( kind= c_int) function pthread_attr_getscope( attr, param) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: param
   end function pthread_attr_getscope

   integer( kind= c_int) function pthread_attr_getschedpolicy( attr, policy) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: policy
   end function pthread_attr_getschedpolicy

   integer( kind= c_int) function pthread_attr_getschedparam( attr, param) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: param
   end function pthread_attr_getschedparam

   integer( kind= c_int) function pthread_attr_getstackaddr( attr, stackaddr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr) :: stackaddr
   end function pthread_attr_getstackaddr

   integer( kind= c_int) function pthread_attr_getstacksize( attr, stacksize) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: stacksize
   end function pthread_attr_getstacksize

   integer( kind= c_int) function pthread_attr_init( attr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
   end function pthread_attr_init

   integer( kind= c_int) function pthread_attr_setdetachstate( attr, detachstate) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: detachstate
   end function pthread_attr_setdetachstate

   integer( kind= c_int) function pthread_attr_setinheritsched( attr, inherit) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: inherit
   end function pthread_attr_setinheritsched

   integer( kind= c_int) function pthread_attr_setschedparam( attr, param) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: param
   end function pthread_attr_setschedparam

   integer( kind= c_int) function pthread_attr_setschedpolicy( attr, policy) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: policy
   end function pthread_attr_setschedpolicy

   integer( kind= c_int) function pthread_attr_setscope( attr, scope) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: scope
   end function pthread_attr_setscope

   integer( kind= c_int) function pthread_attr_setstackaddr( attr, stackaddr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: stackaddr
   end function pthread_attr_setstackaddr

   integer( kind= c_int) function pthread_attr_setstacksize( attr, stacksize) bind( c)
      import :: c_ptr, c_int, size_t
      type( c_ptr), value :: attr
      type( size_t), value :: stacksize
   end function pthread_attr_setstacksize

   integer( kind= c_int) function pthread_cancel( thread) bind( c)
      import :: pthread_t, c_int
      type( pthread_t), value :: thread
   end function pthread_cancel

   integer( kind= c_int) function pthread_cleanup_pop( execute) bind( c)
      import :: c_int
      integer( kind= c_int), value :: execute
   end function pthread_cleanup_pop

   integer( kind= c_int) function pthread_cleanup_push( routine, arg) bind( c)
      import :: c_ptr, c_funptr, c_int
      type( c_funptr), value :: routine
      type( c_ptr), value :: arg
   end function pthread_cleanup_push

   integer( kind= c_int) function pthread_condattr_destroy( attr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
   end function pthread_condattr_destroy

   integer( kind= c_int) function pthread_condattr_getpshared( attr, pshared) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: pshared
   end function pthread_condattr_getpshared

   integer( kind= c_int) function pthread_condattr_init( attr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
   end function pthread_condattr_init

   integer( kind= c_int) function pthread_condattr_setpshared( attr, pshared) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: pshared
   end function pthread_condattr_setpshared

   integer( kind= c_int) function pthread_cond_broadcast( cond) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: cond
   end function pthread_cond_broadcast

   integer( kind= c_int) function pthread_cond_destroy( cond) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: cond
   end function pthread_cond_destroy

   integer( kind= c_int) function pthread_cond_init( cond, attr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: cond
      type( c_ptr), value :: attr
   end function pthread_cond_init

   integer( kind= c_int) function pthread_cond_signal( cond) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: cond
   end function pthread_cond_signal

   integer( kind= c_int) function pthread_cond_timedwait( cond, mutex, abstime) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: cond
      type( c_ptr), value :: mutex
      type( c_ptr), value :: abstime
   end function pthread_cond_timedwait

   integer( kind= c_int) function pthread_cond_wait( cond, mutex) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: cond
      type( c_ptr), value :: mutex
   end function pthread_cond_wait

   integer( kind= c_int) function pthread_create( thread, attr, start_routine, arg) bind( c)
      import :: c_ptr, c_funptr, c_int
      type( c_ptr), value :: thread
      type( c_ptr), value :: attr
      type( c_funptr), value :: start_routine
      type( c_ptr), value :: arg
   end function pthread_create

   integer( kind= c_int) function pthread_detach( thread) bind( c)
      import :: pthread_t, c_int
      type( pthread_t), value :: thread
   end function pthread_detach

   integer( kind= c_int) function pthread_equal( t1, t2) bind( c)
      import :: pthread_t, c_int
      type( pthread_t), value :: t1
      type( pthread_t), value :: t2
   end function pthread_equal

   subroutine pthread_exit( value) bind( c)
      import :: c_ptr
      type( c_ptr), value :: value
   end subroutine pthread_exit

   integer( kind= c_int) function pthread_getschedparam( thread, policy, param) bind( c)
      import :: c_ptr, c_int, pthread_t
      type( pthread_t), value :: thread
      type( c_ptr), value :: policy
      type( c_ptr), value :: param
   end function pthread_getschedparam

   integer( kind= c_int) function pthread_getspecific( key) bind( c)
      import :: pthread_key_t, c_int
      type( pthread_key_t), value :: key
   end function pthread_getspecific

   integer( kind= c_int) function pthread_join( thread, value_ptr) bind( c)
      import :: c_ptr, c_int, pthread_t
      type( pthread_t), value :: thread
      type( c_ptr) :: value_ptr
   end function pthread_join

   integer( kind= c_int) function pthread_key_create( key, destructor) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: key
      type( c_ptr), value :: destructor
   end function pthread_key_create

   integer( kind= c_int) function pthread_key_delete( key) bind( c)
      import :: pthread_key_t, c_int
      type( pthread_key_t), value :: key
   end function pthread_key_delete

   integer( kind= c_int) function pthread_kill( thread, sig) bind( c)
      import :: pthread_t, c_int
      type( pthread_t), value :: thread
      integer( c_int), value :: sig
   end function pthread_kill

   integer( kind= c_int) function pthread_mutexattr_destroy( attr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
   end function pthread_mutexattr_destroy

   integer( kind= c_int) function pthread_mutexattr_getprioceiling( attr, prioceiling) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: prioceiling
   end function pthread_mutexattr_getprioceiling

   integer( kind= c_int) function pthread_mutexattr_getprotocol( attr, protocol) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: protocol
   end function pthread_mutexattr_getprotocol

   integer( kind= c_int) function pthread_mutexattr_getpshared( attr, pshared) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      type( c_ptr), value :: pshared
   end function pthread_mutexattr_getpshared

   integer( kind= c_int) function pthread_mutexattr_init( attr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
   end function pthread_mutexattr_init

   integer( kind= c_int) function pthread_mutexattr_setprioceiling( attr, prioceiling) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: prioceiling
   end function pthread_mutexattr_setprioceiling

   integer( kind= c_int) function pthread_mutexattr_setprotocol( attr, protocol) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: protocol
   end function pthread_mutexattr_setprotocol

   integer( kind= c_int) function pthread_mutexattr_setpshared( attr, pshared) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: attr
      integer( kind= c_int), value :: pshared
   end function pthread_mutexattr_setpshared

   integer( kind= c_int) function pthread_mutex_destroy( mutex) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: mutex
   end function pthread_mutex_destroy

   integer( kind= c_int) function pthread_mutex_init( mutex, attr) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: mutex
      type( c_ptr), value :: attr
   end function pthread_mutex_init

   integer( kind= c_int) function pthread_mutex_lock( mutex) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: mutex
   end function pthread_mutex_lock

   integer( kind= c_int) function pthread_mutex_trylock( mutex) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: mutex
   end function pthread_mutex_trylock

   integer( kind= c_int) function pthread_mutex_unlock( mutex) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: mutex
   end function pthread_mutex_unlock

   integer( kind= c_int) function pthread_once( once_block, init_routine) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: once_block
      type( c_ptr), value :: init_routine
   end function pthread_once

   type( pthread_t) function pthread_self( ) bind( c)
      import :: pthread_t
   end function pthread_self

   integer( kind= c_int) function pthread_setcancelstate( state, oldstate) bind( c)
      import :: c_ptr, c_int
      integer( kind= c_int), value :: state
      type( c_ptr), value :: oldstate
   end function pthread_setcancelstate

   integer( kind= c_int) function pthread_setcanceltype( type, oldtype) bind( c)
      import :: c_ptr, c_int
      integer( kind= c_int), value :: type
      type( c_ptr), value :: oldtype
   end function pthread_setcanceltype

   integer( kind= c_int) function pthread_setschedparam( thread, policy, param) bind( c)
      import :: c_ptr, c_int
      type( c_ptr), value :: thread
      integer( kind= c_int), value :: policy
      type( c_ptr), value :: param
   end function pthread_setschedparam

   integer( kind= c_int) function pthread_setspecific( key, value) bind( c)
      import :: c_ptr, c_int, pthread_key_t
      type( pthread_key_t), value :: key
      type( c_ptr), value :: value
   end function pthread_setspecific

   integer( kind= c_int) function pthread_sigmask( how, set, oset) bind( c)
      import :: c_ptr, c_int
      integer( kind= c_int), value :: how
      type( c_ptr), value :: set
      type( c_ptr), value :: oset
   end function pthread_sigmask

   type( pthread_t) function pthread_testcancel( ) bind( c)
      import :: pthread_t
   end function pthread_testcancel

end interface

! **********************************************************************

!  module procedures

! **********************************************************************

!  If any of the functions whose interfaces are defined above
!  are implemented on your system as C macros, uncomment the following
!  contains statement and write a function below to implement
!  the C macro.  Change the interface above to reflect the change.

contains

! **********************************************************************

!  pthreads

! $Id: pthread.f03 1.1 2005/03/24 17:10:07Z Dan Exp $
! **********************************************************************

end module pthreads
