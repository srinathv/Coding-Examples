! bof
! **********************************************************************
! Fortran 95 module f_df

! ----------------------------------------------------------------------
! Source Control Strings

! $Id$

! ----------------------------------------------------------------------
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

! To report bugs, suggest enhancements, or contact the Authors,
! Contact:
!    Dan Nagle
!                               send email to dannagle@verizon.net
!                                  or mail to 2820 Lafayette Dr.
!                                             Boulder CO 80305 USA

! ----------------------------------------------------------------------
! f_df description

! ----------------------------------------------------------------------

!  f_df uses

!     <none>

!  f_df includes

!     <none>

!  f_df constants

!  f_df types

!  f_df data

!  f_df library

! **********************************************************************

!  f_df

! ----------------------------------------------------------------------

module f_df

! ----------------------------------------------------------------------

!  f_df uses modules

use, intrinsic :: iso_fortran_env, only: real64

! ----------------------------------------------------------------------

!  no implicit typing: require explicit declarations

implicit none

! ----------------------------------------------------------------------

!  all names are private: require explicit exports

private

! ----------------------------------------------------------------------

!  f_df RCS strings

! ----------------------------------------------------------------------

!  module source filename supplied by RCS

character( len= *), public, parameter :: f_df_rcs_id = &
   '$Id$'

! ----------------------------------------------------------------------

!  f_df constants

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  f_df types

! ----------------------------------------------------------------------

type, public :: f_df_t

   real( real64) :: f
   real( real64) :: df

end type f_df_t

! ----------------------------------------------------------------------

!  f_df data

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

!  f_df library

! ----------------------------------------------------------------------

public :: operator( +)

interface operator( +)

   module procedure f_df_add
   module procedure f_df_add_r

end interface operator( +)

public :: operator( -)

interface operator( -)

   module procedure f_df_sub
   module procedure f_df_sub_r

end interface operator( -)

public :: operator( *)

interface operator( *)

   module procedure f_df_mul

end interface operator( *)

public :: operator( /)

interface operator( /)

   module procedure f_df_div

end interface operator( /)

intrinsic :: cos

public :: cos

interface cos

   module procedure f_df_cos

end interface cos

intrinsic :: sin

public :: sin

interface sin

   module procedure f_df_sin

end interface sin

! ----------------------------------------------------------------------

!  module procedures

! ----------------------------------------------------------------------

contains

! ----------------------------------------------------------------------

!  f_df + f_df

function f_df_add( u, v) result( upv)

type( f_df_t), intent( in) :: u
type( f_df_t), intent( in) :: v
type( f_df_t) :: upv

! ----------------------------------------------------------------------

continue

   upv% f = u% f + v% f
   upv% df = u% df + v% df

return

! ----------------------------------------------------------------------

end function f_df_add

! ----------------------------------------------------------------------

!  f_df + r

function f_df_add_r( u, v) result( upv)

type( f_df_t), intent( in) :: u
real( real64), intent( in) :: v
type( f_df_t) :: upv

! ----------------------------------------------------------------------

continue

   upv% f = u% f + v
   upv% df = u% df

return

! ----------------------------------------------------------------------

end function f_df_add_r

! ----------------------------------------------------------------------

!  f_df - f_df

function f_df_sub( u, v) result( umv)

type( f_df_t), intent( in) :: u
type( f_df_t), intent( in) :: v
type( f_df_t) :: umv

! ----------------------------------------------------------------------

continue

   umv% f = u% f - v% f
   umv% df = u% df - v% df

return

! ----------------------------------------------------------------------

end function f_df_sub

! ----------------------------------------------------------------------

!  f_df -r

function f_df_sub_r( u, v) result( umv)

type( f_df_t), intent( in) :: u
real( real64), intent( in) :: v
type( f_df_t) :: umv

! ----------------------------------------------------------------------

continue

   umv% f = u% f - v
   umv% df = u% df

return

! ----------------------------------------------------------------------

end function f_df_sub_r

! ----------------------------------------------------------------------

!  f_df * f_df

function f_df_mul( u, v) result( uxv)

type( f_df_t), intent( in) :: u
type( f_df_t), intent( in) :: v
type( f_df_t) :: uxv

! ----------------------------------------------------------------------

continue

   uxv% f = u% f * v% f
   uxv% df = u% df * v% f + v% df * u% f

return

! ----------------------------------------------------------------------

end function f_df_mul

! ----------------------------------------------------------------------

!  f_df / f_df

function f_df_div( u, v) result( uov)

type( f_df_t), intent( in) :: u
type( f_df_t), intent( in) :: v
type( f_df_t) :: uov

! ----------------------------------------------------------------------

continue

   uov% f = u% f / v% f
   uov% df = ( u% df * v% f - v% df * u% f) / ( v% f * v% f)

return

! ----------------------------------------------------------------------

end function f_df_div

! ----------------------------------------------------------------------

!  cos( f_df)

function f_df_cos( u) result( cu)

type( f_df_t), intent( in) :: u
type( f_df_t) :: cu

! ----------------------------------------------------------------------

continue

   cu% f = cos( u% f)
   cu% df = -sin( u% df)

return

! ----------------------------------------------------------------------

end function f_df_cos

! ----------------------------------------------------------------------

!  sin( f_df)

function f_df_sin( u) result( su)

type( f_df_t), intent( in) :: u
type( f_df_t) :: su

! ----------------------------------------------------------------------

continue

   su% f = sin( u% f)
   su% df = cos( u% df)

return

! ----------------------------------------------------------------------

end function f_df_sin

! ----------------------------------------------------------------------

!  f_df

! $Id$
! **********************************************************************
! eof
end module f_df


