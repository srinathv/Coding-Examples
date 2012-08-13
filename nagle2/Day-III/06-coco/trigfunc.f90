! bof
! **********************************************************************
! Fortran 95 module trig_functions

! **********************************************************************
! Source Control Strings

! $Id: trigfunc.fpp 1.3 2003/10/03 19:44:06Z Dan Release $

! **********************************************************************
! Copyright 2003 Purple Sage Computing Solutions, Inc.

! **********************************************************************
! Summary of License

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
!    Purple Sage Computing Solutions, Inc.
!                               send email to dnagle@@erols.com
!                                   or fax to 703 471 0684 (USA)
!                                  or mail to 12142 Purple Sage Ct.
!                                             Reston, VA 20194-5621 USA

! **********************************************************************
! trigonometric functions for real kinds

! **********************************************************************

!  trig_functions library

!     cot() cotangent function cot() for reals
!     acot() arccotangent functions acot(), acot2() for reals
!     acot2()

!     sec() secant, arcsecant functions sec(), asec() for reals
!     asec()
!     csc() cosecant, arccosecant functions csc(), acsc() for reals
!     acsc()

!     asinh() arc hyperbolic sine(), cosine() for reals
!     acosh()

!     atanh() arc hyperbolic tangent functions atanh(), atanh2() for reals
!     atanh2()
!     coth() hyperbolic cotangent function coth() for reals
!     acoth() arc hyperbolic cotangent functions acoth(), acoth2() for reals
!     acoth2()

!     sech() hyperbolic secant functions sech(), csch() for reals
!     csch()
!     asech() arc hyperbolic secant functions asech(), acsch() for reals
!     acsch()

!     sind() degrees: sind(), cosd(), tand(), cotd(), secd(), cscd() for reals
!     cosd()
!     tand()
!     cotd()
!     secd()
!     cscd()

!     asind() degrees: asind(), acosd(), asecd(), acscd() for reals
!     acosd()
!     asecd()
!     acscd()
!     atand() degrees: atand(), atan2d(), acotd(), acot2d() for reals
!     atan2d()
!     acotd()
!     acot2d()





! **********************************************************************

module trig_functions

! **********************************************************************
! use standard parameterization of processor dependencies

use standard_types

! **********************************************************************
! declare all variables

implicit none

! **********************************************************************
! export names

private

! **********************************************************************

!  RCS strings

! **********************************************************************

character( len= *), parameter :: trig_functions_rcs_id = &
   '$Id: trigfunc.fpp 1.3 2003/10/03 19:44:06Z Dan Release $'

! **********************************************************************

!  trig_functions library

! **********************************************************************

!  declare specific functions supporting generic function cot()

public :: cot

interface cot
   module procedure single_cot
   module procedure double_cot
   module procedure quad_cot
end interface

!  declare specific functions supporting generic function acot()

public :: acot

interface acot
   module procedure single_acot
   module procedure double_acot
   module procedure quad_acot
end interface

!  declare specific functions supporting generic function acot2()

interface acot2
   module procedure single_acot2
   module procedure double_acot2
   module procedure quad_acot2
end interface

!  declare specific functions supporting generic function sec()

public :: sec

interface sec
   module procedure single_sec
   module procedure double_sec
   module procedure quad_sec
end interface

!  declare specific functions supporting generic function asec()

public :: asec

interface asec
   module procedure single_asec
   module procedure double_asec
   module procedure quad_asec
end interface

!  declare specific functions supporting generic function csc()

public :: csc

interface csc
   module procedure single_csc
   module procedure double_csc
   module procedure quad_csc
end interface

!  declare specific functions supporting generic function acsc()

public :: acsc

interface acsc
   module procedure single_acsc
   module procedure double_acsc
   module procedure quad_acsc
end interface

!  declare specific functions supporting generic function asinh()

public :: asinh

interface asinh
   module procedure single_asinh
   module procedure double_asinh
   module procedure quad_asinh
end interface

!  declare specific functions supporting generic function acosh()

public :: acosh

interface acosh
   module procedure single_acosh
   module procedure double_acosh
   module procedure quad_acosh
end interface

!  declare specific functions supporting generic function atanh()

public :: atanh

interface atanh
   module procedure single_atanh
   module procedure double_atanh
   module procedure quad_atanh
end interface

!  declare specific functions supporting generic function atanh2()

public :: atanh2

interface atanh2
   module procedure single_atanh2
   module procedure double_atanh2
   module procedure quad_atanh2
end interface

!  declare specific functions supporting generic function coth()

public :: coth

interface coth
   module procedure single_coth
   module procedure double_coth
   module procedure quad_coth
end interface

!  declare specific functions supporting generic function acoth()

public :: acoth

interface acoth
   module procedure single_acoth
   module procedure double_acoth
   module procedure quad_acoth
end interface

!  declare specific functions supporting generic function acoth2()

public :: acoth2

interface acoth2
   module procedure single_acoth2
   module procedure double_acoth2
   module procedure quad_acoth2
end interface

!  declare specific functions supporting generic function sech()

public :: sech

interface sech
   module procedure single_sech
   module procedure double_sech
   module procedure quad_sech
end interface

!  declare specific functions supporting generic function asech()

public :: asech

interface asech
   module procedure single_asech
   module procedure double_asech
   module procedure quad_asech
end interface

!  declare specific functions supporting generic function csch()

public :: csch

interface csch
   module procedure single_csch
   module procedure double_csch
   module procedure quad_csch
end interface

!  declare specific functions supporting generic function acsch()

public :: acsch

interface acsch
   module procedure single_acsch
   module procedure double_acsch
   module procedure quad_acsch
end interface

!  declare specific functions supporting generic function sind()

public :: sind

interface sind
   module procedure single_sind
   module procedure double_sind
   module procedure quad_sind
end interface

!  declare specific functions supporting generic function asind()

public :: asind

interface asind
   module procedure single_asind
   module procedure double_asind
   module procedure quad_asind
end interface

!  declare specific functions supporting generic function cosd()

public :: cosd

interface cosd
   module procedure single_cosd
   module procedure double_cosd
   module procedure quad_cosd
end interface

!  declare specific functions supporting generic function acosd()

public :: acosd

interface acosd
   module procedure single_acosd
   module procedure double_acosd
   module procedure quad_acosd
end interface

!  declare specific functions supporting generic function tand()

public :: tand

interface tand
   module procedure single_tand
   module procedure double_tand
   module procedure quad_tand
end interface

!  declare specific functions supporting generic function atand()

public :: atand

interface atand
   module procedure single_atand
   module procedure double_atand
   module procedure quad_atand
end interface

!  declare specific functions supporting generic function atan2d()

public :: atan2d

interface atan2d
   module procedure single_atan2d
   module procedure double_atan2d
   module procedure quad_atan2d
end interface

!  declare specific functions supporting generic function cotd()

public :: cotd

interface cotd
   module procedure single_cotd
   module procedure double_cotd
   module procedure quad_cotd
end interface

!  declare specific functions supporting generic function acotd()

public :: acotd

interface acotd
   module procedure single_acotd
   module procedure double_acotd
   module procedure quad_acotd
end interface

!  declare specific functions supporting generic function acotd()

public :: acot2d

interface acot2d
   module procedure single_acot2d
   module procedure double_acot2d
   module procedure quad_acot2d
end interface

!  declare specific functions supporting generic function secd()

public :: secd

interface secd
   module procedure single_secd
   module procedure double_secd
   module procedure quad_secd
end interface

!  declare specific functions supporting generic function asecd()

public :: asecd

interface asecd
   module procedure single_asecd
   module procedure double_asecd
   module procedure quad_asecd
end interface

!  declare specific functions supporting generic function cscd()

public :: cscd

interface cscd
   module procedure single_cscd
   module procedure double_cscd
   module procedure quad_cscd
end interface

!  declare specific functions supporting generic function acscd()

public :: acscd

interface acscd
   module procedure single_acscd
   module procedure double_acscd
   module procedure quad_acscd
end interface

! **********************************************************************

!  trig_functions module procedures

! **********************************************************************

contains

! **********************************************************************

!  cotangent, arccotangent functions for real kinds

! **********************************************************************

!  ?kind_cot(): cot() for kind single

real( kind= single_k) function single_cot( a)

real( kind= single_k), intent( in) :: a

!  single_cot()

continue                                                             ! cot()

   single_cot = cos( a) / sin( a)

return                                                               ! cot()

!  single_cot()

end function single_cot

! **********************************************************************

!  ?kind_cot(): cot() for kind double

real( kind= double_k) function double_cot( a)

real( kind= double_k), intent( in) :: a

!  double_cot()

continue                                                             ! cot()

   double_cot = cos( a) / sin( a)

return                                                               ! cot()

!  double_cot()

end function double_cot

! **********************************************************************

!  ?kind_cot(): cot() for kind quad

real( kind= quad_k) function quad_cot( a)

real( kind= quad_k), intent( in) :: a

!  quad_cot()

continue                                                             ! cot()

   quad_cot = cos( a) / sin( a)

return                                                               ! cot()

!  quad_cot()

end function quad_cot

! **********************************************************************

!  single_acot(): acot() for kind single

real( kind= single_k) function single_acot( a)

real( kind= single_k), intent( in) :: a

!  single_acot()

continue                                                             ! acot()

   single_acot = atan( 1.0_single_k / a)

return                                                               ! acot()

!  single_acot()

end function single_acot

! **********************************************************************

!  double_acot(): acot() for kind double

real( kind= double_k) function double_acot( a)

real( kind= double_k), intent( in) :: a

!  double_acot()

continue                                                             ! acot()

   double_acot = atan( 1.0_double_k / a)

return                                                               ! acot()

!  double_acot()

end function double_acot

! **********************************************************************

!  quad_acot(): acot() for kind quad

real( kind= quad_k) function quad_acot( a)

real( kind= quad_k), intent( in) :: a

!  quad_acot()

continue                                                             ! acot()

   quad_acot = atan( 1.0_quad_k / a)

return                                                               ! acot()

!  quad_acot()

end function quad_acot

! **********************************************************************

!  single_acot2(): acot2() for kind single

real( kind= single_k) function single_acot2( a, b)

real( kind= single_k), intent( in) :: a, b

!  single_acot2

continue                                                             ! acot2()

   single_acot2 = atan2( b, a)

return                                                               ! acot2()

!  single_acot2()

end function single_acot2

! **********************************************************************

!  double_acot2(): acot2() for kind double

real( kind= double_k) function double_acot2( a, b)

real( kind= double_k), intent( in) :: a, b

!  double_acot2

continue                                                             ! acot2()

   double_acot2 = atan2( b, a)

return                                                               ! acot2()

!  double_acot2()

end function double_acot2

! **********************************************************************

!  quad_acot2(): acot2() for kind quad

real( kind= quad_k) function quad_acot2( a, b)

real( kind= quad_k), intent( in) :: a, b

!  quad_acot2

continue                                                             ! acot2()

   quad_acot2 = atan2( b, a)

return                                                               ! acot2()

!  quad_acot2()

end function quad_acot2

! **********************************************************************

!  secant, arcsecant, cosecant, arccosecant functions for real kinds

! **********************************************************************

!  single_sec(): sec() for kind single

real( kind= single_k) function single_sec( a)

real( kind= single_k), intent( in) :: a

!  single_sec()

continue                                                             ! sec()

   single_sec = 1.0_single_k / cos( a)

return                                                               ! sec()

!  single_sec()

end function single_sec

! **********************************************************************

!  double_sec(): sec() for kind double

real( kind= double_k) function double_sec( a)

real( kind= double_k), intent( in) :: a

!  double_sec()

continue                                                             ! sec()

   double_sec = 1.0_double_k / cos( a)

return                                                               ! sec()

!  double_sec()

end function double_sec

! **********************************************************************

!  quad_sec(): sec() for kind quad

real( kind= quad_k) function quad_sec( a)

real( kind= quad_k), intent( in) :: a

!  quad_sec()

continue                                                             ! sec()

   quad_sec = 1.0_quad_k / cos( a)

return                                                               ! sec()

!  quad_sec()

end function quad_sec

! **********************************************************************

!  single_asec(): asec() for kind single

real( kind= single_k) function single_asec( a)

real( kind= single_k), intent( in) :: a

!  single_asec()

continue                                                             ! asec()

   single_asec = acos( 1.0_single_k / a)

return                                                               ! asec()

!  single_asec()

end function single_asec

! **********************************************************************

!  double_asec(): asec() for kind double

real( kind= double_k) function double_asec( a)

real( kind= double_k), intent( in) :: a

!  double_asec()

continue                                                             ! asec()

   double_asec = acos( 1.0_double_k / a)

return                                                               ! asec()

!  double_asec()

end function double_asec

! **********************************************************************

!  quad_asec(): asec() for kind quad

real( kind= quad_k) function quad_asec( a)

real( kind= quad_k), intent( in) :: a

!  quad_asec()

continue                                                             ! asec()

   quad_asec = acos( 1.0_quad_k / a)

return                                                               ! asec()

!  quad_asec()

end function quad_asec

! **********************************************************************

!  single_csc(): csc() for kind single

real( kind= single_k) function single_csc( a)

real( kind= single_k), intent( in) :: a

!  single_csc()

continue                                                             ! csc()

   single_csc = 1.0_single_k / sin( a)

return                                                               ! csc()

!  single_csc()

end function single_csc

! **********************************************************************

!  double_csc(): csc() for kind double

real( kind= double_k) function double_csc( a)

real( kind= double_k), intent( in) :: a

!  double_csc()

continue                                                             ! csc()

   double_csc = 1.0_double_k / sin( a)

return                                                               ! csc()

!  double_csc()

end function double_csc

! **********************************************************************

!  quad_csc(): csc() for kind quad

real( kind= quad_k) function quad_csc( a)

real( kind= quad_k), intent( in) :: a

!  quad_csc()

continue                                                             ! csc()

   quad_csc = 1.0_quad_k / sin( a)

return                                                               ! csc()

!  quad_csc()

end function quad_csc

! **********************************************************************

!  single_acsc(): acsc() for kind single

real( kind= single_k) function single_acsc( a)

real( kind= single_k), intent( in) :: a

!  single_acsc()

continue                                                             ! acsc()

   single_acsc = asin( 1.0_single_k /  a)

return                                                               ! acsc()

!  single_acsc()

end function single_acsc

! **********************************************************************

!  double_acsc(): acsc() for kind double

real( kind= double_k) function double_acsc( a)

real( kind= double_k), intent( in) :: a

!  double_acsc()

continue                                                             ! acsc()

   double_acsc = asin( 1.0_double_k /  a)

return                                                               ! acsc()

!  double_acsc()

end function double_acsc

! **********************************************************************

!  quad_acsc(): acsc() for kind quad

real( kind= quad_k) function quad_acsc( a)

real( kind= quad_k), intent( in) :: a

!  quad_acsc()

continue                                                             ! acsc()

   quad_acsc = asin( 1.0_quad_k /  a)

return                                                               ! acsc()

!  quad_acsc()

end function quad_acsc

! **********************************************************************

!  hyperbolic arcsine, arccosine functions for real kinds

! **********************************************************************

!  single_asinh(): asinh() for kind single

real( kind= single_k) function single_asinh( a)

real( kind= single_k), intent( in) :: a

!  single_sinh()

continue                                                             ! asinh()

   single_asinh = log( a + sqrt( a*a + 1.0_single_k) )

return                                                               ! asinh()

!  single_asinh()

end function single_asinh

! **********************************************************************

!  double_asinh(): asinh() for kind double

real( kind= double_k) function double_asinh( a)

real( kind= double_k), intent( in) :: a

!  double_sinh()

continue                                                             ! asinh()

   double_asinh = log( a + sqrt( a*a + 1.0_double_k) )

return                                                               ! asinh()

!  double_asinh()

end function double_asinh

! **********************************************************************

!  quad_asinh(): asinh() for kind quad

real( kind= quad_k) function quad_asinh( a)

real( kind= quad_k), intent( in) :: a

!  quad_sinh()

continue                                                             ! asinh()

   quad_asinh = log( a + sqrt( a*a + 1.0_quad_k) )

return                                                               ! asinh()

!  quad_asinh()

end function quad_asinh

! **********************************************************************

!  single_acosh(): acosh() for kind single

real( kind= single_k) function single_acosh( a)

real( kind= single_k), intent( in) :: a

!  single_acosh()

continue                                                             ! acosh()

   single_acosh = log( a + sqrt( a*a - 1.0_single_k) )

return                                                               ! acosh()

!  single_acosh()

end function single_acosh

! **********************************************************************

!  double_acosh(): acosh() for kind double

real( kind= double_k) function double_acosh( a)

real( kind= double_k), intent( in) :: a

!  double_acosh()

continue                                                             ! acosh()

   double_acosh = log( a + sqrt( a*a - 1.0_double_k) )

return                                                               ! acosh()

!  double_acosh()

end function double_acosh

! **********************************************************************

!  quad_acosh(): acosh() for kind quad

real( kind= quad_k) function quad_acosh( a)

real( kind= quad_k), intent( in) :: a

!  quad_acosh()

continue                                                             ! acosh()

   quad_acosh = log( a + sqrt( a*a - 1.0_quad_k) )

return                                                               ! acosh()

!  quad_acosh()

end function quad_acosh

! **********************************************************************

!  hyperbolic arctangent functions for real kinds

! **********************************************************************

!  single_atanh(): atanh() for kind single

real( kind= single_k) function single_atanh( a)

real( kind= single_k), intent( in) :: a

!  single_atanh()

continue                                                             ! atanh()

   single_atanh = 0.5_single_k * log( ( a + 1.0_single_k) / ( a - 1.0_single_k) )

return                                                               ! atanh()

!  single_atanh()

end function single_atanh

! **********************************************************************

!  double_atanh(): atanh() for kind double

real( kind= double_k) function double_atanh( a)

real( kind= double_k), intent( in) :: a

!  double_atanh()

continue                                                             ! atanh()

   double_atanh = 0.5_double_k * log( ( a + 1.0_double_k) / ( a - 1.0_double_k) )

return                                                               ! atanh()

!  double_atanh()

end function double_atanh

! **********************************************************************

!  quad_atanh(): atanh() for kind quad

real( kind= quad_k) function quad_atanh( a)

real( kind= quad_k), intent( in) :: a

!  quad_atanh()

continue                                                             ! atanh()

   quad_atanh = 0.5_quad_k * log( ( a + 1.0_quad_k) / ( a - 1.0_quad_k) )

return                                                               ! atanh()

!  quad_atanh()

end function quad_atanh

! **********************************************************************

!  single_atanh2(): atanh2() for kind single

real( kind= single_k) function single_atanh2( a, b)

real( kind= single_k), intent( in) :: a, b

   real( kind= single_k) :: r

!  single_atanh2()

continue                                                             ! atanh2()

   r = a / b

   single_atanh2 = 0.5_single_k * log( ( r + 1.0_single_k) / ( r - 1.0_single_k) )

return                                                               ! atanh2()

!  single_atanh2()

end function single_atanh2

! **********************************************************************

!  double_atanh2(): atanh2() for kind double

real( kind= double_k) function double_atanh2( a, b)

real( kind= double_k), intent( in) :: a, b

   real( kind= double_k) :: r

!  double_atanh2()

continue                                                             ! atanh2()

   r = a / b

   double_atanh2 = 0.5_double_k * log( ( r + 1.0_double_k) / ( r - 1.0_double_k) )

return                                                               ! atanh2()

!  double_atanh2()

end function double_atanh2

! **********************************************************************

!  quad_atanh2(): atanh2() for kind quad

real( kind= quad_k) function quad_atanh2( a, b)

real( kind= quad_k), intent( in) :: a, b

   real( kind= quad_k) :: r

!  quad_atanh2()

continue                                                             ! atanh2()

   r = a / b

   quad_atanh2 = 0.5_quad_k * log( ( r + 1.0_quad_k) / ( r - 1.0_quad_k) )

return                                                               ! atanh2()

!  quad_atanh2()

end function quad_atanh2

! **********************************************************************

!  hyperbolic cotangent, arccotangent functions for real kinds

! **********************************************************************

!  single_coth(): coth() for kind single

real( kind= single_k) function single_coth( a)

real( kind= single_k), intent( in) :: a

   real( kind= single_k) :: ep, em

!  single_coth()

continue                                                             ! coth()

   ep = exp( a)
   em = exp( -a)

   single_coth = ( ep + em) / ( ep - em)

return                                                               ! coth()

!  single_coth()

end function single_coth

! **********************************************************************

!  double_coth(): coth() for kind double

real( kind= double_k) function double_coth( a)

real( kind= double_k), intent( in) :: a

   real( kind= double_k) :: ep, em

!  double_coth()

continue                                                             ! coth()

   ep = exp( a)
   em = exp( -a)

   double_coth = ( ep + em) / ( ep - em)

return                                                               ! coth()

!  double_coth()

end function double_coth

! **********************************************************************

!  quad_coth(): coth() for kind quad

real( kind= quad_k) function quad_coth( a)

real( kind= quad_k), intent( in) :: a

   real( kind= quad_k) :: ep, em

!  quad_coth()

continue                                                             ! coth()

   ep = exp( a)
   em = exp( -a)

   quad_coth = ( ep + em) / ( ep - em)

return                                                               ! coth()

!  quad_coth()

end function quad_coth

! **********************************************************************

!  hyperbolic arccotangent functions for real kinds

! **********************************************************************

!  single_acoth(): acoth() for kind single

real( kind= single_k) function single_acoth( a)

real( kind= single_k), intent( in) :: a

!  single_acoth()

continue                                                             ! acoth()

   single_acoth = 0.5_single_k * log( ( a - 1.0_single_k) / ( a + 1.0_single_k) )

return                                                               ! acoth()

!  single_acoth()

end function single_acoth

! **********************************************************************

!  double_acoth(): acoth() for kind double

real( kind= double_k) function double_acoth( a)

real( kind= double_k), intent( in) :: a

!  double_acoth()

continue                                                             ! acoth()

   double_acoth = 0.5_double_k * log( ( a - 1.0_double_k) / ( a + 1.0_double_k) )

return                                                               ! acoth()

!  double_acoth()

end function double_acoth

! **********************************************************************

!  quad_acoth(): acoth() for kind quad

real( kind= quad_k) function quad_acoth( a)

real( kind= quad_k), intent( in) :: a

!  quad_acoth()

continue                                                             ! acoth()

   quad_acoth = 0.5_quad_k * log( ( a - 1.0_quad_k) / ( a + 1.0_quad_k) )

return                                                               ! acoth()

!  quad_acoth()

end function quad_acoth

! **********************************************************************

!  single_acoth2(): acoth2() for kind single

real( kind= single_k) function single_acoth2( a, b)

real( kind= single_k), intent( in) :: a, b

   real( kind= single_k) :: r

!  single_acoth2()

continue                                                             ! acoth2()

   r = a / b

   single_acoth2 = 0.5_single_k * log( ( r - 1.0_single_k) / ( r + 1.0_single_k) )

return                                                               ! acoth2()

!  single_acoth2()

end function single_acoth2

! **********************************************************************

!  double_acoth2(): acoth2() for kind double

real( kind= double_k) function double_acoth2( a, b)

real( kind= double_k), intent( in) :: a, b

   real( kind= double_k) :: r

!  double_acoth2()

continue                                                             ! acoth2()

   r = a / b

   double_acoth2 = 0.5_double_k * log( ( r - 1.0_double_k) / ( r + 1.0_double_k) )

return                                                               ! acoth2()

!  double_acoth2()

end function double_acoth2

! **********************************************************************

!  quad_acoth2(): acoth2() for kind quad

real( kind= quad_k) function quad_acoth2( a, b)

real( kind= quad_k), intent( in) :: a, b

   real( kind= quad_k) :: r

!  quad_acoth2()

continue                                                             ! acoth2()

   r = a / b

   quad_acoth2 = 0.5_quad_k * log( ( r - 1.0_quad_k) / ( r + 1.0_quad_k) )

return                                                               ! acoth2()

!  quad_acoth2()

end function quad_acoth2

! **********************************************************************

!  hyperbolic secant, cosecant functions for real kinds

! **********************************************************************

!  single_sech(): sech() for kind single

real( kind= single_k) function single_sech( a)

real( kind= single_k), intent( in) :: a

!  single_sech()

continue                                                             ! sech()

   single_sech = 1.0_single_k / cosh( a)

return                                                               ! sech()

!  single_sech()

end function single_sech

! **********************************************************************

!  double_sech(): sech() for kind double

real( kind= double_k) function double_sech( a)

real( kind= double_k), intent( in) :: a

!  double_sech()

continue                                                             ! sech()

   double_sech = 1.0_double_k / cosh( a)

return                                                               ! sech()

!  double_sech()

end function double_sech

! **********************************************************************

!  quad_sech(): sech() for kind quad

real( kind= quad_k) function quad_sech( a)

real( kind= quad_k), intent( in) :: a

!  quad_sech()

continue                                                             ! sech()

   quad_sech = 1.0_quad_k / cosh( a)

return                                                               ! sech()

!  quad_sech()

end function quad_sech

! **********************************************************************

!  single_csch(): csch() for kind single

real( kind= single_k) function single_csch( a)

real( kind= single_k), intent( in) :: a

!  single_csch()

continue                                                             ! csch()

   single_csch = 1.0_single_k / sinh( a)

return                                                               ! csch()

!  single_csch()

end function single_csch

! **********************************************************************

!  double_csch(): csch() for kind double

real( kind= double_k) function double_csch( a)

real( kind= double_k), intent( in) :: a

!  double_csch()

continue                                                             ! csch()

   double_csch = 1.0_double_k / sinh( a)

return                                                               ! csch()

!  double_csch()

end function double_csch

! **********************************************************************

!  quad_csch(): csch() for kind quad

real( kind= quad_k) function quad_csch( a)

real( kind= quad_k), intent( in) :: a

!  quad_csch()

continue                                                             ! csch()

   quad_csch = 1.0_quad_k / sinh( a)

return                                                               ! csch()

!  quad_csch()

end function quad_csch

! **********************************************************************

!  hyperbolic arcsecant, arccosecant functions for real kinds

! **********************************************************************

!  single_asech(): asech() for kind single

real( kind= single_k) function single_asech( a)

real( kind= single_k), intent( in) :: a

   real( kind= single_k) :: ai

!  single_asech()

continue                                                             ! asech()

   ai = 1.0_single_k / a

   single_asech = log( ai + sqrt( ai*ai - 1.0_single_k) )

return                                                               ! asech()

!  single_asech()

end function single_asech

! **********************************************************************

!  double_asech(): asech() for kind double

real( kind= double_k) function double_asech( a)

real( kind= double_k), intent( in) :: a

   real( kind= double_k) :: ai

!  double_asech()

continue                                                             ! asech()

   ai = 1.0_double_k / a

   double_asech = log( ai + sqrt( ai*ai - 1.0_double_k) )

return                                                               ! asech()

!  double_asech()

end function double_asech

! **********************************************************************

!  quad_asech(): asech() for kind quad

real( kind= quad_k) function quad_asech( a)

real( kind= quad_k), intent( in) :: a

   real( kind= quad_k) :: ai

!  quad_asech()

continue                                                             ! asech()

   ai = 1.0_quad_k / a

   quad_asech = log( ai + sqrt( ai*ai - 1.0_quad_k) )

return                                                               ! asech()

!  quad_asech()

end function quad_asech

! **********************************************************************

!  single_acsch(): acsch() for kind single

real( kind= single_k) function single_acsch( a)

real( kind= single_k), intent( in) :: a

   real( kind= single_k) :: ai

!  single_acsch()

continue                                                             ! acsch()

   ai = 1.0_single_k / a

   single_acsch = log( ai + sqrt( ai*ai + 1.0_single_k) )

return                                                               ! acsch()

!  single_acsch()

end function single_acsch

! **********************************************************************

!  double_acsch(): acsch() for kind double

real( kind= double_k) function double_acsch( a)

real( kind= double_k), intent( in) :: a

   real( kind= double_k) :: ai

!  double_acsch()

continue                                                             ! acsch()

   ai = 1.0_double_k / a

   double_acsch = log( ai + sqrt( ai*ai + 1.0_double_k) )

return                                                               ! acsch()

!  double_acsch()

end function double_acsch

! **********************************************************************

!  quad_acsch(): acsch() for kind quad

real( kind= quad_k) function quad_acsch( a)

real( kind= quad_k), intent( in) :: a

   real( kind= quad_k) :: ai

!  quad_acsch()

continue                                                             ! acsch()

   ai = 1.0_quad_k / a

   quad_acsch = log( ai + sqrt( ai*ai + 1.0_quad_k) )

return                                                               ! acsch()

!  quad_acsch()

end function quad_acsch

! **********************************************************************

!  sind(), cosd(), tand(), cotd(), secd(), cscd()- arcs in degrees

! **********************************************************************

!  single_sind(): sind() for kind single

real( kind= single_k) function single_sind( a)

real( kind= single_k), intent( in) :: a

!  single_sind()

continue                                                             ! sind()

   single_sind = sin( 0.017453292519943295769236907684886125_single_k * a)

return                                                               ! sind()

!  single_sind()

end function single_sind

! **********************************************************************

!  double_sind(): sind() for kind double

real( kind= double_k) function double_sind( a)

real( kind= double_k), intent( in) :: a

!  double_sind()

continue                                                             ! sind()

   double_sind = sin( 0.017453292519943295769236907684886125_double_k * a)

return                                                               ! sind()

!  double_sind()

end function double_sind

! **********************************************************************

!  quad_sind(): sind() for kind quad

real( kind= quad_k) function quad_sind( a)

real( kind= quad_k), intent( in) :: a

!  quad_sind()

continue                                                             ! sind()

   quad_sind = sin( 0.017453292519943295769236907684886125_quad_k * a)

return                                                               ! sind()

!  quad_sind()

end function quad_sind

! **********************************************************************

!  single_cosd(): cosd() for kind single

real( kind= single_k) function single_cosd( a)

real( kind= single_k), intent( in) :: a

!  single_cosd()

continue                                                             ! cosd()

   single_cosd = cos( 0.017453292519943295769236907684886125_single_k * a)

return                                                               ! cosd()

!  single_cosd()

end function single_cosd

! **********************************************************************

!  double_cosd(): cosd() for kind double

real( kind= double_k) function double_cosd( a)

real( kind= double_k), intent( in) :: a

!  double_cosd()

continue                                                             ! cosd()

   double_cosd = cos( 0.017453292519943295769236907684886125_double_k * a)

return                                                               ! cosd()

!  double_cosd()

end function double_cosd

! **********************************************************************

!  quad_cosd(): cosd() for kind quad

real( kind= quad_k) function quad_cosd( a)

real( kind= quad_k), intent( in) :: a

!  quad_cosd()

continue                                                             ! cosd()

   quad_cosd = cos( 0.017453292519943295769236907684886125_quad_k * a)

return                                                               ! cosd()

!  quad_cosd()

end function quad_cosd

! **********************************************************************

!  single_tand(): tand() for kind single

real( kind= single_k) function single_tand( a)

real( kind= single_k), intent( in) :: a

!  single_tand()

continue                                                             ! tand()

   single_tand = tan( 0.017453292519943295769236907684886125_single_k * a)

return                                                               ! tand()

!  single_tand()

end function single_tand

! **********************************************************************

!  double_tand(): tand() for kind double

real( kind= double_k) function double_tand( a)

real( kind= double_k), intent( in) :: a

!  double_tand()

continue                                                             ! tand()

   double_tand = tan( 0.017453292519943295769236907684886125_double_k * a)

return                                                               ! tand()

!  double_tand()

end function double_tand

! **********************************************************************

!  quad_tand(): tand() for kind quad

real( kind= quad_k) function quad_tand( a)

real( kind= quad_k), intent( in) :: a

!  quad_tand()

continue                                                             ! tand()

   quad_tand = tan( 0.017453292519943295769236907684886125_quad_k * a)

return                                                               ! tand()

!  quad_tand()

end function quad_tand

! **********************************************************************

!  single_cotd(): cotd() for kind single

real( kind= single_k) function single_cotd( a)

real( kind= single_k), intent( in) :: a

!  single_cotd()

continue                                                             ! cotd()

   single_cotd = 1.0_single_k / tan( 0.017453292519943295769236907684886125_single_k * a)

return                                                               ! cotd()

!  single_cotd()

end function single_cotd

! **********************************************************************

!  double_cotd(): cotd() for kind double

real( kind= double_k) function double_cotd( a)

real( kind= double_k), intent( in) :: a

!  double_cotd()

continue                                                             ! cotd()

   double_cotd = 1.0_double_k / tan( 0.017453292519943295769236907684886125_double_k * a)

return                                                               ! cotd()

!  double_cotd()

end function double_cotd

! **********************************************************************

!  quad_cotd(): cotd() for kind quad

real( kind= quad_k) function quad_cotd( a)

real( kind= quad_k), intent( in) :: a

!  quad_cotd()

continue                                                             ! cotd()

   quad_cotd = 1.0_quad_k / tan( 0.017453292519943295769236907684886125_quad_k * a)

return                                                               ! cotd()

!  quad_cotd()

end function quad_cotd

! **********************************************************************

!  single_secd(): secd() for kind single

real( kind= single_k) function single_secd( a)

real( kind= single_k), intent( in) :: a

!  single_secd()

continue                                                             ! secd()

   single_secd = 1.0_single_k / cos( 0.017453292519943295769236907684886125_single_k * a)

return                                                               ! secd()

!  single_secd()

end function single_secd

! **********************************************************************

!  double_secd(): secd() for kind double

real( kind= double_k) function double_secd( a)

real( kind= double_k), intent( in) :: a

!  double_secd()

continue                                                             ! secd()

   double_secd = 1.0_double_k / cos( 0.017453292519943295769236907684886125_double_k * a)

return                                                               ! secd()

!  double_secd()

end function double_secd

! **********************************************************************

!  quad_secd(): secd() for kind quad

real( kind= quad_k) function quad_secd( a)

real( kind= quad_k), intent( in) :: a

!  quad_secd()

continue                                                             ! secd()

   quad_secd = 1.0_quad_k / cos( 0.017453292519943295769236907684886125_quad_k * a)

return                                                               ! secd()

!  quad_secd()

end function quad_secd

! **********************************************************************

!  single_cscd(): cscd() for kind single

real( kind= single_k) function single_cscd( a)

real( kind= single_k), intent( in) :: a

!  single_cscd()

continue                                                             ! cscd()

   single_cscd = 1.0_single_k / sin( 0.017453292519943295769236907684886125_single_k * a)

return                                                               ! cscd()

!  single_cscd()

end function single_cscd

! **********************************************************************

!  double_cscd(): cscd() for kind double

real( kind= double_k) function double_cscd( a)

real( kind= double_k), intent( in) :: a

!  double_cscd()

continue                                                             ! cscd()

   double_cscd = 1.0_double_k / sin( 0.017453292519943295769236907684886125_double_k * a)

return                                                               ! cscd()

!  double_cscd()

end function double_cscd

! **********************************************************************

!  quad_cscd(): cscd() for kind quad

real( kind= quad_k) function quad_cscd( a)

real( kind= quad_k), intent( in) :: a

!  quad_cscd()

continue                                                             ! cscd()

   quad_cscd = 1.0_quad_k / sin( 0.017453292519943295769236907684886125_quad_k * a)

return                                                               ! cscd()

!  quad_cscd()

end function quad_cscd

! **********************************************************************

!  asind(), acosd(), atand(), acotd(), asecd(), acscd()- return degrees

! **********************************************************************

!  single_asind(): asind() for kind single

real( kind= single_k) function single_asind( a)

real( kind= single_k), intent( in) :: a

!  single_asind()

continue                                                             ! asind()

   single_asind = 57.2957795130823208767981548141052_single_k * asin( a)

return                                                               ! asind()

!  single_asind()

end function single_asind

! **********************************************************************

!  double_asind(): asind() for kind double

real( kind= double_k) function double_asind( a)

real( kind= double_k), intent( in) :: a

!  double_asind()

continue                                                             ! asind()

   double_asind = 57.2957795130823208767981548141052_double_k * asin( a)

return                                                               ! asind()

!  double_asind()

end function double_asind

! **********************************************************************

!  quad_asind(): asind() for kind quad

real( kind= quad_k) function quad_asind( a)

real( kind= quad_k), intent( in) :: a

!  quad_asind()

continue                                                             ! asind()

   quad_asind = 57.2957795130823208767981548141052_quad_k * asin( a)

return                                                               ! asind()

!  quad_asind()

end function quad_asind

! **********************************************************************

!  single_acosd(): acosd() for kind single

real( kind= single_k) function single_acosd( a)

real( kind= single_k), intent( in) :: a

!  single_acosd()

continue                                                             ! acosd()

   single_acosd = 57.2957795130823208767981548141052_single_k * acos( a)

return                                                               ! acosd()

!  single_acosd()

end function single_acosd

! **********************************************************************

!  double_acosd(): acosd() for kind double

real( kind= double_k) function double_acosd( a)

real( kind= double_k), intent( in) :: a

!  double_acosd()

continue                                                             ! acosd()

   double_acosd = 57.2957795130823208767981548141052_double_k * acos( a)

return                                                               ! acosd()

!  double_acosd()

end function double_acosd

! **********************************************************************

!  quad_acosd(): acosd() for kind quad

real( kind= quad_k) function quad_acosd( a)

real( kind= quad_k), intent( in) :: a

!  quad_acosd()

continue                                                             ! acosd()

   quad_acosd = 57.2957795130823208767981548141052_quad_k * acos( a)

return                                                               ! acosd()

!  quad_acosd()

end function quad_acosd

! **********************************************************************

!  single_atand(): atand() for kind single

real( kind= single_k) function single_atand( a)

real( kind= single_k), intent( in) :: a

!  single_atand()

continue                                                             ! atand()

   single_atand = 57.2957795130823208767981548141052_single_k * atan( a)

return                                                               ! atand()

!  single_atand()

end function single_atand

! **********************************************************************

!  double_atand(): atand() for kind double

real( kind= double_k) function double_atand( a)

real( kind= double_k), intent( in) :: a

!  double_atand()

continue                                                             ! atand()

   double_atand = 57.2957795130823208767981548141052_double_k * atan( a)

return                                                               ! atand()

!  double_atand()

end function double_atand

! **********************************************************************

!  quad_atand(): atand() for kind quad

real( kind= quad_k) function quad_atand( a)

real( kind= quad_k), intent( in) :: a

!  quad_atand()

continue                                                             ! atand()

   quad_atand = 57.2957795130823208767981548141052_quad_k * atan( a)

return                                                               ! atand()

!  quad_atand()

end function quad_atand

! **********************************************************************

!  single_atan2d(): atan2d() for kind single

real( kind= single_k) function single_atan2d( a, b)

real( kind= single_k), intent( in) :: a, b

!  single_atan2d()

continue                                                             ! atan2d()

   single_atan2d = 57.2957795130823208767981548141052_single_k * atan2( a, b)

return                                                               ! atan2d()

!  single_atan2d()

end function single_atan2d

! **********************************************************************

!  double_atan2d(): atan2d() for kind double

real( kind= double_k) function double_atan2d( a, b)

real( kind= double_k), intent( in) :: a, b

!  double_atan2d()

continue                                                             ! atan2d()

   double_atan2d = 57.2957795130823208767981548141052_double_k * atan2( a, b)

return                                                               ! atan2d()

!  double_atan2d()

end function double_atan2d

! **********************************************************************

!  quad_atan2d(): atan2d() for kind quad

real( kind= quad_k) function quad_atan2d( a, b)

real( kind= quad_k), intent( in) :: a, b

!  quad_atan2d()

continue                                                             ! atan2d()

   quad_atan2d = 57.2957795130823208767981548141052_quad_k * atan2( a, b)

return                                                               ! atan2d()

!  quad_atan2d()

end function quad_atan2d

! **********************************************************************

!  single_acotd(): acotd() for kind single

real( kind= single_k) function single_acotd( a)

real( kind= single_k), intent( in) :: a

!  single_acotd()

continue                                                             ! acotd()

   single_acotd = 57.2957795130823208767981548141052_single_k * atan( 1.0_single_k / a)

return                                                               ! acotd()

!  single_acotd()

end function single_acotd

! **********************************************************************

!  double_acotd(): acotd() for kind double

real( kind= double_k) function double_acotd( a)

real( kind= double_k), intent( in) :: a

!  double_acotd()

continue                                                             ! acotd()

   double_acotd = 57.2957795130823208767981548141052_double_k * atan( 1.0_double_k / a)

return                                                               ! acotd()

!  double_acotd()

end function double_acotd

! **********************************************************************

!  quad_acotd(): acotd() for kind quad

real( kind= quad_k) function quad_acotd( a)

real( kind= quad_k), intent( in) :: a

!  quad_acotd()

continue                                                             ! acotd()

   quad_acotd = 57.2957795130823208767981548141052_quad_k * atan( 1.0_quad_k / a)

return                                                               ! acotd()

!  quad_acotd()

end function quad_acotd

! **********************************************************************

!  single_acot2d(): acot2d() for kind single

real( kind= single_k) function single_acot2d( a, b)

real( kind= single_k), intent( in) :: a, b

!  single_acot2d()

continue                                                             ! acot2d()

   single_acot2d = 57.2957795130823208767981548141052_single_k * atan2( b, a)

return                                                               ! acot2d()

!  single_acot2d()

end function single_acot2d

! **********************************************************************

!  double_acot2d(): acot2d() for kind double

real( kind= double_k) function double_acot2d( a, b)

real( kind= double_k), intent( in) :: a, b

!  double_acot2d()

continue                                                             ! acot2d()

   double_acot2d = 57.2957795130823208767981548141052_double_k * atan2( b, a)

return                                                               ! acot2d()

!  double_acot2d()

end function double_acot2d

! **********************************************************************

!  quad_acot2d(): acot2d() for kind quad

real( kind= quad_k) function quad_acot2d( a, b)

real( kind= quad_k), intent( in) :: a, b

!  quad_acot2d()

continue                                                             ! acot2d()

   quad_acot2d = 57.2957795130823208767981548141052_quad_k * atan2( b, a)

return                                                               ! acot2d()

!  quad_acot2d()

end function quad_acot2d

! **********************************************************************

!  single_asecd(): asecd() for kind single

real( kind= single_k) function single_asecd( a)

real( kind= single_k), intent( in) :: a

!  single_asecd()

continue                                                             ! asecd()

   single_asecd = 57.2957795130823208767981548141052_single_k * acos( 1.0_single_k / a)

return                                                               ! asecd()

!  single_asecd()

end function single_asecd

! **********************************************************************

!  double_asecd(): asecd() for kind double

real( kind= double_k) function double_asecd( a)

real( kind= double_k), intent( in) :: a

!  double_asecd()

continue                                                             ! asecd()

   double_asecd = 57.2957795130823208767981548141052_double_k * acos( 1.0_double_k / a)

return                                                               ! asecd()

!  double_asecd()

end function double_asecd

! **********************************************************************

!  quad_asecd(): asecd() for kind quad

real( kind= quad_k) function quad_asecd( a)

real( kind= quad_k), intent( in) :: a

!  quad_asecd()

continue                                                             ! asecd()

   quad_asecd = 57.2957795130823208767981548141052_quad_k * acos( 1.0_quad_k / a)

return                                                               ! asecd()

!  quad_asecd()

end function quad_asecd

! **********************************************************************

!  single_acscd(): acscd() for kind single

real( kind= single_k) function single_acscd( a)

real( kind= single_k), intent( in) :: a

!  single_acscd()

continue                                                             ! acscd()

   single_acscd = 57.2957795130823208767981548141052_single_k * asin( 1.0_single_k / a)

return                                                               ! acscd()

!  single_acscd()

end function single_acscd

! **********************************************************************

!  double_acscd(): acscd() for kind double

real( kind= double_k) function double_acscd( a)

real( kind= double_k), intent( in) :: a

!  double_acscd()

continue                                                             ! acscd()

   double_acscd = 57.2957795130823208767981548141052_double_k * asin( 1.0_double_k / a)

return                                                               ! acscd()

!  double_acscd()

end function double_acscd

! **********************************************************************

!  quad_acscd(): acscd() for kind quad

real( kind= quad_k) function quad_acscd( a)

real( kind= quad_k), intent( in) :: a

!  quad_acscd()

continue                                                             ! acscd()

   quad_acscd = 57.2957795130823208767981548141052_quad_k * asin( 1.0_quad_k / a)

return                                                               ! acscd()

!  quad_acscd()

end function quad_acscd

! **********************************************************************

!  trig_functions

! $Id: trigfunc.fpp 1.3 2003/10/03 19:44:06Z Dan Release $
! **********************************************************************

end module trig_functions                                  ! eof
