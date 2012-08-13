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

?? ! *******************************************************************

?? ! preprocessor definitions

?? include 'coco.inc'

?? ! *******************************************************************

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
?? if( single_k )then
   module procedure single_cot
?? endif
?? if( double_k )then
   module procedure double_cot
?? endif
?? if( quad_k )then
   module procedure quad_cot
?? endif
end interface

!  declare specific functions supporting generic function acot()

public :: acot

interface acot
?? if( single_k )then
   module procedure single_acot
?? endif
?? if( double_k )then
   module procedure double_acot
?? endif
?? if( quad_k )then
   module procedure quad_acot
?? endif
end interface

!  declare specific functions supporting generic function acot2()

interface acot2
?? if( single_k )then
   module procedure single_acot2
?? endif
?? if( double_k )then
   module procedure double_acot2
?? endif
?? if( quad_k )then
   module procedure quad_acot2
?? endif
end interface

!  declare specific functions supporting generic function sec()

public :: sec

interface sec
?? if( single_k )then
   module procedure single_sec
?? endif
?? if( double_k )then
   module procedure double_sec
?? endif
?? if( quad_k )then
   module procedure quad_sec
?? endif
end interface

!  declare specific functions supporting generic function asec()

public :: asec

interface asec
?? if( single_k )then
   module procedure single_asec
?? endif
?? if( double_k )then
   module procedure double_asec
?? endif
?? if( quad_k )then
   module procedure quad_asec
?? endif
end interface

!  declare specific functions supporting generic function csc()

public :: csc

interface csc
?? if( single_k )then
   module procedure single_csc
?? endif
?? if( double_k )then
   module procedure double_csc
?? endif
?? if( quad_k )then
   module procedure quad_csc
?? endif
end interface

!  declare specific functions supporting generic function acsc()

public :: acsc

interface acsc
?? if( single_k )then
   module procedure single_acsc
?? endif
?? if( double_k )then
   module procedure double_acsc
?? endif
?? if( quad_k )then
   module procedure quad_acsc
?? endif
end interface

!  declare specific functions supporting generic function asinh()

public :: asinh

interface asinh
?? if( single_k )then
   module procedure single_asinh
?? endif
?? if( double_k )then
   module procedure double_asinh
?? endif
?? if( quad_k )then
   module procedure quad_asinh
?? endif
end interface

!  declare specific functions supporting generic function acosh()

public :: acosh

interface acosh
?? if( single_k )then
   module procedure single_acosh
?? endif
?? if( double_k )then
   module procedure double_acosh
?? endif
?? if( quad_k )then
   module procedure quad_acosh
?? endif
end interface

!  declare specific functions supporting generic function atanh()

public :: atanh

interface atanh
?? if( single_k )then
   module procedure single_atanh
?? endif
?? if( double_k )then
   module procedure double_atanh
?? endif
?? if( quad_k )then
   module procedure quad_atanh
?? endif
end interface

!  declare specific functions supporting generic function atanh2()

public :: atanh2

interface atanh2
?? if( single_k )then
   module procedure single_atanh2
?? endif
?? if( double_k )then
   module procedure double_atanh2
?? endif
?? if( quad_k )then
   module procedure quad_atanh2
?? endif
end interface

!  declare specific functions supporting generic function coth()

public :: coth

interface coth
?? if( single_k )then
   module procedure single_coth
?? endif
?? if( double_k )then
   module procedure double_coth
?? endif
?? if( quad_k )then
   module procedure quad_coth
?? endif
end interface

!  declare specific functions supporting generic function acoth()

public :: acoth

interface acoth
?? if( single_k )then
   module procedure single_acoth
?? endif
?? if( double_k )then
   module procedure double_acoth
?? endif
?? if( quad_k )then
   module procedure quad_acoth
?? endif
end interface

!  declare specific functions supporting generic function acoth2()

public :: acoth2

interface acoth2
?? if( single_k )then
   module procedure single_acoth2
?? endif
?? if( double_k )then
   module procedure double_acoth2
?? endif
?? if( quad_k )then
   module procedure quad_acoth2
?? endif
end interface

!  declare specific functions supporting generic function sech()

public :: sech

interface sech
?? if( single_k )then
   module procedure single_sech
?? endif
?? if( double_k )then
   module procedure double_sech
?? endif
?? if( quad_k )then
   module procedure quad_sech
?? endif
end interface

!  declare specific functions supporting generic function asech()

public :: asech

interface asech
?? if( single_k )then
   module procedure single_asech
?? endif
?? if( double_k )then
   module procedure double_asech
?? endif
?? if( quad_k )then
   module procedure quad_asech
?? endif
end interface

!  declare specific functions supporting generic function csch()

public :: csch

interface csch
?? if( single_k )then
   module procedure single_csch
?? endif
?? if( double_k )then
   module procedure double_csch
?? endif
?? if( quad_k )then
   module procedure quad_csch
?? endif
end interface

!  declare specific functions supporting generic function acsch()

public :: acsch

interface acsch
?? if( single_k )then
   module procedure single_acsch
?? endif
?? if( double_k )then
   module procedure double_acsch
?? endif
?? if( quad_k )then
   module procedure quad_acsch
?? endif
end interface

!  declare specific functions supporting generic function sind()

public :: sind

interface sind
?? if( single_k )then
   module procedure single_sind
?? endif
?? if( double_k )then
   module procedure double_sind
?? endif
?? if( quad_k )then
   module procedure quad_sind
?? endif
end interface

!  declare specific functions supporting generic function asind()

public :: asind

interface asind
?? if( single_k )then
   module procedure single_asind
?? endif
?? if( double_k )then
   module procedure double_asind
?? endif
?? if( quad_k )then
   module procedure quad_asind
?? endif
end interface

!  declare specific functions supporting generic function cosd()

public :: cosd

interface cosd
?? if( single_k )then
   module procedure single_cosd
?? endif
?? if( double_k )then
   module procedure double_cosd
?? endif
?? if( quad_k )then
   module procedure quad_cosd
?? endif
end interface

!  declare specific functions supporting generic function acosd()

public :: acosd

interface acosd
?? if( single_k )then
   module procedure single_acosd
?? endif
?? if( double_k )then
   module procedure double_acosd
?? endif
?? if( quad_k )then
   module procedure quad_acosd
?? endif
end interface

!  declare specific functions supporting generic function tand()

public :: tand

interface tand
?? if( single_k )then
   module procedure single_tand
?? endif
?? if( double_k )then
   module procedure double_tand
?? endif
?? if( quad_k )then
   module procedure quad_tand
?? endif
end interface

!  declare specific functions supporting generic function atand()

public :: atand

interface atand
?? if( single_k )then
   module procedure single_atand
?? endif
?? if( double_k )then
   module procedure double_atand
?? endif
?? if( quad_k )then
   module procedure quad_atand
?? endif
end interface

!  declare specific functions supporting generic function atan2d()

public :: atan2d

interface atan2d
?? if( single_k )then
   module procedure single_atan2d
?? endif
?? if( double_k )then
   module procedure double_atan2d
?? endif
?? if( quad_k )then
   module procedure quad_atan2d
?? endif
end interface

!  declare specific functions supporting generic function cotd()

public :: cotd

interface cotd
?? if( single_k )then
   module procedure single_cotd
?? endif
?? if( double_k )then
   module procedure double_cotd
?? endif
?? if( quad_k )then
   module procedure quad_cotd
?? endif
end interface

!  declare specific functions supporting generic function acotd()

public :: acotd

interface acotd
?? if( single_k )then
   module procedure single_acotd
?? endif
?? if( double_k )then
   module procedure double_acotd
?? endif
?? if( quad_k )then
   module procedure quad_acotd
?? endif
end interface

!  declare specific functions supporting generic function acotd()

public :: acot2d

interface acot2d
?? if( single_k )then
   module procedure single_acot2d
?? endif
?? if( double_k )then
   module procedure double_acot2d
?? endif
?? if( quad_k )then
   module procedure quad_acot2d
?? endif
end interface

!  declare specific functions supporting generic function secd()

public :: secd

interface secd
?? if( single_k )then
   module procedure single_secd
?? endif
?? if( double_k )then
   module procedure double_secd
?? endif
?? if( quad_k )then
   module procedure quad_secd
?? endif
end interface

!  declare specific functions supporting generic function asecd()

public :: asecd

interface asecd
?? if( single_k )then
   module procedure single_asecd
?? endif
?? if( double_k )then
   module procedure double_asecd
?? endif
?? if( quad_k )then
   module procedure quad_asecd
?? endif
end interface

!  declare specific functions supporting generic function cscd()

public :: cscd

interface cscd
?? if( single_k )then
   module procedure single_cscd
?? endif
?? if( double_k )then
   module procedure double_cscd
?? endif
?? if( quad_k )then
   module procedure quad_cscd
?? endif
end interface

!  declare specific functions supporting generic function acscd()

public :: acscd

interface acscd
?? if( single_k )then
   module procedure single_acscd
?? endif
?? if( double_k )then
   module procedure double_acscd
?? endif
?? if( quad_k )then
   module procedure quad_acscd
?? endif
end interface

! **********************************************************************

!  trig_functions module procedures

! **********************************************************************

contains

! **********************************************************************

!  cotangent, arccotangent functions for real kinds

?? text :: cot( kind)
! **********************************************************************

!  ?kind_cot(): cot() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_cot( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_cot()

continue                                                             ! cot()

   ?kind?_cot = cos( a) / sin( a)

return                                                               ! cot()

!  ?kind?_cot()

end function ?kind?_cot

?? end text cot
?? if( single_k )then
?? copy :: cot( single)
?? endif
?? if( double_k )then
?? copy :: cot( double)
?? endif
?? if( quad_k )then
?? copy :: cot( quad)
?? endif
?? text :: acot( kind)
! **********************************************************************

!  ?kind?_acot(): acot() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acot( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_acot()

continue                                                             ! acot()

   ?kind?_acot = atan( 1.0_?kind?_k / a)

return                                                               ! acot()

!  ?kind?_acot()

end function ?kind?_acot

?? end text acot
?? if( single_k )then
?? copy :: acot( single)
?? endif
?? if( double_k )then
?? copy :: acot( double)
?? endif
?? if( quad_k )then
?? copy :: acot( quad)
?? endif
?? text :: acot2( kind)
! **********************************************************************

!  ?kind?_acot2(): acot2() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acot2( a, b)

real( kind= ?kind?_k), intent( in) :: a, b

!  ?kind?_acot2

continue                                                             ! acot2()

   ?kind?_acot2 = atan2( b, a)

return                                                               ! acot2()

!  ?kind?_acot2()

end function ?kind?_acot2

?? end text acot2
?? if( single_k )then
?? copy :: acot2( single)
?? endif
?? if( double_k )then
?? copy :: acot2( double)
?? endif
?? if( quad_k )then
?? copy :: acot2( quad)
?? endif
! **********************************************************************

!  secant, arcsecant, cosecant, arccosecant functions for real kinds

?? text :: sec( kind)
! **********************************************************************

!  ?kind?_sec(): sec() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_sec( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_sec()

continue                                                             ! sec()

   ?kind?_sec = 1.0_?kind?_k / cos( a)

return                                                               ! sec()

!  ?kind?_sec()

end function ?kind?_sec

?? end text sec
?? if( single_k )then
?? copy :: sec( single)
?? endif
?? if( double_k )then
?? copy :: sec( double)
?? endif
?? if( quad_k )then
?? copy :: sec( quad)
?? endif
?? text :: asec( kind)
! **********************************************************************

!  ?kind?_asec(): asec() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_asec( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_asec()

continue                                                             ! asec()

   ?kind?_asec = acos( 1.0_?kind?_k / a)

return                                                               ! asec()

!  ?kind?_asec()

end function ?kind?_asec

?? end text asec
?? if( single_k )then
?? copy :: asec( single)
?? endif
?? if( double_k )then
?? copy :: asec( double)
?? endif
?? if( quad_k )then
?? copy :: asec( quad)
?? endif
?? text :: csc( kind)
! **********************************************************************

!  ?kind?_csc(): csc() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_csc( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_csc()

continue                                                             ! csc()

   ?kind?_csc = 1.0_?kind?_k / sin( a)

return                                                               ! csc()

!  ?kind?_csc()

end function ?kind?_csc

?? end text csc
?? if( single_k )then
?? copy :: csc( single)
?? endif
?? if( double_k )then
?? copy :: csc( double)
?? endif
?? if( quad_k )then
?? copy :: csc( quad)
?? endif
?? text :: acsc( kind)
! **********************************************************************

!  ?kind?_acsc(): acsc() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acsc( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_acsc()

continue                                                             ! acsc()

   ?kind?_acsc = asin( 1.0_?kind?_k /  a)

return                                                               ! acsc()

!  ?kind?_acsc()

end function ?kind?_acsc

?? end text acsc
?? if( single_k )then
?? copy :: acsc( single)
?? endif
?? if( double_k )then
?? copy :: acsc( double)
?? endif
?? if( quad_k )then
?? copy :: acsc( quad)
?? endif
! **********************************************************************

!  hyperbolic arcsine, arccosine functions for real kinds

?? text :: asinh( kind)
! **********************************************************************

!  ?kind?_asinh(): asinh() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_asinh( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_sinh()

continue                                                             ! asinh()

   ?kind?_asinh = log( a + sqrt( a*a + 1.0_?kind?_k) )

return                                                               ! asinh()

!  ?kind?_asinh()

end function ?kind?_asinh

?? end text asinh
?? if( single_k )then
?? copy :: asinh( single)
?? endif
?? if( double_k )then
?? copy :: asinh( double)
?? endif
?? if( quad_k )then
?? copy :: asinh( quad)
?? endif
?? text :: acosh( kind)
! **********************************************************************

!  ?kind?_acosh(): acosh() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acosh( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_acosh()

continue                                                             ! acosh()

   ?kind?_acosh = log( a + sqrt( a*a - 1.0_?kind?_k) )

return                                                               ! acosh()

!  ?kind?_acosh()

end function ?kind?_acosh

?? end text acosh
?? if( single_k )then
?? copy :: acosh( single)
?? endif
?? if( double_k )then
?? copy :: acosh( double)
?? endif
?? if( quad_k )then
?? copy :: acosh( quad)
?? endif
! **********************************************************************

!  hyperbolic arctangent functions for real kinds

?? text :: atanh( kind)
! **********************************************************************

!  ?kind?_atanh(): atanh() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_atanh( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_atanh()

continue                                                             ! atanh()

   ?kind?_atanh = 0.5_?kind?_k * log( ( a + 1.0_?kind?_k) / ( a - 1.0_?kind?_k) )

return                                                               ! atanh()

!  ?kind?_atanh()

end function ?kind?_atanh

?? end text atanh
?? if( single_k )then
?? copy :: atanh( single)
?? endif
?? if( double_k )then
?? copy :: atanh( double)
?? endif
?? if( quad_k )then
?? copy :: atanh( quad)
?? endif
?? text :: atanh2( kind)
! **********************************************************************

!  ?kind?_atanh2(): atanh2() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_atanh2( a, b)

real( kind= ?kind?_k), intent( in) :: a, b

   real( kind= ?kind?_k) :: r

!  ?kind?_atanh2()

continue                                                             ! atanh2()

   r = a / b

   ?kind?_atanh2 = 0.5_?kind?_k * log( ( r + 1.0_?kind?_k) / ( r - 1.0_?kind?_k) )

return                                                               ! atanh2()

!  ?kind?_atanh2()

end function ?kind?_atanh2

?? end text atanh2
?? if( single_k )then
?? copy :: atanh2( single)
?? endif
?? if( double_k )then
?? copy :: atanh2( double)
?? endif
?? if( quad_k )then
?? copy :: atanh2( quad)
?? endif
! **********************************************************************

!  hyperbolic cotangent, arccotangent functions for real kinds

?? text :: coth( kind)
! **********************************************************************

!  ?kind?_coth(): coth() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_coth( a)

real( kind= ?kind?_k), intent( in) :: a

   real( kind= ?kind?_k) :: ep, em

!  ?kind?_coth()

continue                                                             ! coth()

   ep = exp( a)
   em = exp( -a)

   ?kind?_coth = ( ep + em) / ( ep - em)

return                                                               ! coth()

!  ?kind?_coth()

end function ?kind?_coth

?? end text coth
?? if( single_k )then
?? copy :: coth( single)
?? endif
?? if( double_k )then
?? copy :: coth( double)
?? endif
?? if( quad_k )then
?? copy :: coth( quad)
?? endif
! **********************************************************************

!  hyperbolic arccotangent functions for real kinds

?? text :: acoth( kind)
! **********************************************************************

!  ?kind?_acoth(): acoth() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acoth( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_acoth()

continue                                                             ! acoth()

   ?kind?_acoth = 0.5_?kind?_k * log( ( a - 1.0_?kind?_k) / ( a + 1.0_?kind?_k) )

return                                                               ! acoth()

!  ?kind?_acoth()

end function ?kind?_acoth

?? end text acoth
?? if( single_k )then
?? copy :: acoth( single)
?? endif
?? if( double_k )then
?? copy :: acoth( double)
?? endif
?? if( quad_k )then
?? copy :: acoth( quad)
?? endif
?? text :: acoth2( kind)
! **********************************************************************

!  ?kind?_acoth2(): acoth2() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acoth2( a, b)

real( kind= ?kind?_k), intent( in) :: a, b

   real( kind= ?kind?_k) :: r

!  ?kind?_acoth2()

continue                                                             ! acoth2()

   r = a / b

   ?kind?_acoth2 = 0.5_?kind?_k * log( ( r - 1.0_?kind?_k) / ( r + 1.0_?kind?_k) )

return                                                               ! acoth2()

!  ?kind?_acoth2()

end function ?kind?_acoth2

?? end text acoth2
?? if( single_k )then
?? copy :: acoth2( single)
?? endif
?? if( double_k )then
?? copy :: acoth2( double)
?? endif
?? if( quad_k )then
?? copy :: acoth2( quad)
?? endif
! **********************************************************************

!  hyperbolic secant, cosecant functions for real kinds

?? text :: sech( kind)
! **********************************************************************

!  ?kind?_sech(): sech() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_sech( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_sech()

continue                                                             ! sech()

   ?kind?_sech = 1.0_?kind?_k / cosh( a)

return                                                               ! sech()

!  ?kind?_sech()

end function ?kind?_sech

?? end text sech
?? if( single_k )then
?? copy :: sech( single)
?? endif
?? if( double_k )then
?? copy :: sech( double)
?? endif
?? if( quad_k )then
?? copy :: sech( quad)
?? endif
?? text :: csch( kind)
! **********************************************************************

!  ?kind?_csch(): csch() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_csch( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_csch()

continue                                                             ! csch()

   ?kind?_csch = 1.0_?kind?_k / sinh( a)

return                                                               ! csch()

!  ?kind?_csch()

end function ?kind?_csch

?? end text csch
?? if( single_k )then
?? copy :: csch( single)
?? endif
?? if( double_k )then
?? copy :: csch( double)
?? endif
?? if( quad_k )then
?? copy :: csch( quad)
?? endif
! **********************************************************************

!  hyperbolic arcsecant, arccosecant functions for real kinds

?? text :: asech( kind)
! **********************************************************************

!  ?kind?_asech(): asech() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_asech( a)

real( kind= ?kind?_k), intent( in) :: a

   real( kind= ?kind?_k) :: ai

!  ?kind?_asech()

continue                                                             ! asech()

   ai = 1.0_?kind?_k / a

   ?kind?_asech = log( ai + sqrt( ai*ai - 1.0_?kind?_k) )

return                                                               ! asech()

!  ?kind?_asech()

end function ?kind?_asech

?? end text asech
?? if( single_k )then
?? copy :: asech( single)
?? endif
?? if( double_k )then
?? copy :: asech( double)
?? endif
?? if( quad_k )then
?? copy :: asech( quad)
?? endif
?? text :: acsch( kind)
! **********************************************************************

!  ?kind?_acsch(): acsch() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acsch( a)

real( kind= ?kind?_k), intent( in) :: a

   real( kind= ?kind?_k) :: ai

!  ?kind?_acsch()

continue                                                             ! acsch()

   ai = 1.0_?kind?_k / a

   ?kind?_acsch = log( ai + sqrt( ai*ai + 1.0_?kind?_k) )

return                                                               ! acsch()

!  ?kind?_acsch()

end function ?kind?_acsch

?? end text acsch
?? if( single_k )then
?? copy :: acsch( single)
?? endif
?? if( double_k )then
?? copy :: acsch( double)
?? endif
?? if( quad_k )then
?? copy :: acsch( quad)
?? endif
! **********************************************************************

!  sind(), cosd(), tand(), cotd(), secd(), cscd()- arcs in degrees

?? text :: sind( kind)
! **********************************************************************

!  ?kind?_sind(): sind() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_sind( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_sind()

continue                                                             ! sind()

   ?kind?_sind = sin( 0.017453292519943295769236907684886125_?kind?_k * a)

return                                                               ! sind()

!  ?kind?_sind()

end function ?kind?_sind

?? end text sind
?? if( single_k )then
?? copy :: sind( single)
?? endif
?? if( double_k )then
?? copy :: sind( double)
?? endif
?? if( quad_k )then
?? copy :: sind( quad)
?? endif
?? text :: cosd( kind)
! **********************************************************************

!  ?kind?_cosd(): cosd() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_cosd( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_cosd()

continue                                                             ! cosd()

   ?kind?_cosd = cos( 0.017453292519943295769236907684886125_?kind?_k * a)

return                                                               ! cosd()

!  ?kind?_cosd()

end function ?kind?_cosd

?? end text cosd
?? if( single_k )then
?? copy :: cosd( single)
?? endif
?? if( double_k )then
?? copy :: cosd( double)
?? endif
?? if( quad_k )then
?? copy :: cosd( quad)
?? endif
?? text :: tand( kind)
! **********************************************************************

!  ?kind?_tand(): tand() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_tand( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_tand()

continue                                                             ! tand()

   ?kind?_tand = tan( 0.017453292519943295769236907684886125_?kind?_k * a)

return                                                               ! tand()

!  ?kind?_tand()

end function ?kind?_tand

?? end text tand
?? if( single_k )then
?? copy :: tand( single)
?? endif
?? if( double_k )then
?? copy :: tand( double)
?? endif
?? if( quad_k )then
?? copy :: tand( quad)
?? endif
?? text :: cotd( kind)
! **********************************************************************

!  ?kind?_cotd(): cotd() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_cotd( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_cotd()

continue                                                             ! cotd()

   ?kind?_cotd = 1.0_?kind?_k / tan( 0.017453292519943295769236907684886125_?kind?_k * a)

return                                                               ! cotd()

!  ?kind?_cotd()

end function ?kind?_cotd

?? end text cotd
?? if( single_k )then
?? copy :: cotd( single)
?? endif
?? if( double_k )then
?? copy :: cotd( double)
?? endif
?? if( quad_k )then
?? copy :: cotd( quad)
?? endif
?? text :: secd( kind)
! **********************************************************************

!  ?kind?_secd(): secd() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_secd( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_secd()

continue                                                             ! secd()

   ?kind?_secd = 1.0_?kind?_k / cos( 0.017453292519943295769236907684886125_?kind?_k * a)

return                                                               ! secd()

!  ?kind?_secd()

end function ?kind?_secd

?? end text secd
?? if( single_k )then
?? copy :: secd( single)
?? endif
?? if( double_k )then
?? copy :: secd( double)
?? endif
?? if( quad_k )then
?? copy :: secd( quad)
?? endif
?? text :: cscd( kind)
! **********************************************************************

!  ?kind?_cscd(): cscd() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_cscd( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_cscd()

continue                                                             ! cscd()

   ?kind?_cscd = 1.0_?kind?_k / sin( 0.017453292519943295769236907684886125_?kind?_k * a)

return                                                               ! cscd()

!  ?kind?_cscd()

end function ?kind?_cscd

?? end text cscd
?? if( single_k )then
?? copy :: cscd( single)
?? endif
?? if( double_k )then
?? copy :: cscd( double)
?? endif
?? if( quad_k )then
?? copy :: cscd( quad)
?? endif
! **********************************************************************

!  asind(), acosd(), atand(), acotd(), asecd(), acscd()- return degrees

?? text :: asind( kind)
! **********************************************************************

!  ?kind?_asind(): asind() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_asind( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_asind()

continue                                                             ! asind()

   ?kind?_asind = 57.2957795130823208767981548141052_?kind?_k * asin( a)

return                                                               ! asind()

!  ?kind?_asind()

end function ?kind?_asind

?? end text asind
?? if( single_k )then
?? copy :: asind( single)
?? endif
?? if( double_k )then
?? copy :: asind( double)
?? endif
?? if( quad_k )then
?? copy :: asind( quad)
?? endif
?? text :: acosd( kind)
! **********************************************************************

!  ?kind?_acosd(): acosd() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acosd( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_acosd()

continue                                                             ! acosd()

   ?kind?_acosd = 57.2957795130823208767981548141052_?kind?_k * acos( a)

return                                                               ! acosd()

!  ?kind?_acosd()

end function ?kind?_acosd

?? end text acosd
?? if( single_k )then
?? copy :: acosd( single)
?? endif
?? if( double_k )then
?? copy :: acosd( double)
?? endif
?? if( quad_k )then
?? copy :: acosd( quad)
?? endif
?? text :: atand( kind)
! **********************************************************************

!  ?kind?_atand(): atand() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_atand( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_atand()

continue                                                             ! atand()

   ?kind?_atand = 57.2957795130823208767981548141052_?kind?_k * atan( a)

return                                                               ! atand()

!  ?kind?_atand()

end function ?kind?_atand

?? end text atand
?? if( single_k )then
?? copy :: atand( single)
?? endif
?? if( double_k )then
?? copy :: atand( double)
?? endif
?? if( quad_k )then
?? copy :: atand( quad)
?? endif
?? text :: atan2d( kind)
! **********************************************************************

!  ?kind?_atan2d(): atan2d() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_atan2d( a, b)

real( kind= ?kind?_k), intent( in) :: a, b

!  ?kind?_atan2d()

continue                                                             ! atan2d()

   ?kind?_atan2d = 57.2957795130823208767981548141052_?kind?_k * atan2( a, b)

return                                                               ! atan2d()

!  ?kind?_atan2d()

end function ?kind?_atan2d

?? end text atan2d
?? if( single_k )then
?? copy :: atan2d( single)
?? endif
?? if( double_k )then
?? copy :: atan2d( double)
?? endif
?? if( quad_k )then
?? copy :: atan2d( quad)
?? endif
?? text :: acotd( kind)
! **********************************************************************

!  ?kind?_acotd(): acotd() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acotd( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_acotd()

continue                                                             ! acotd()

   ?kind?_acotd = 57.2957795130823208767981548141052_?kind?_k * atan( 1.0_?kind?_k / a)

return                                                               ! acotd()

!  ?kind?_acotd()

end function ?kind?_acotd

?? end text acotd
?? if( single_k )then
?? copy :: acotd( single)
?? endif
?? if( double_k )then
?? copy :: acotd( double)
?? endif
?? if( quad_k )then
?? copy :: acotd( quad)
?? endif
?? text :: acot2d( kind)
! **********************************************************************

!  ?kind?_acot2d(): acot2d() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acot2d( a, b)

real( kind= ?kind?_k), intent( in) :: a, b

!  ?kind?_acot2d()

continue                                                             ! acot2d()

   ?kind?_acot2d = 57.2957795130823208767981548141052_?kind?_k * atan2( b, a)

return                                                               ! acot2d()

!  ?kind?_acot2d()

end function ?kind?_acot2d

?? end text acot2d
?? if( single_k )then
?? copy :: acot2d( single)
?? endif
?? if( double_k )then
?? copy :: acot2d( double)
?? endif
?? if( quad_k )then
?? copy :: acot2d( quad)
?? endif
?? text :: asecd( kind)
! **********************************************************************

!  ?kind?_asecd(): asecd() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_asecd( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_asecd()

continue                                                             ! asecd()

   ?kind?_asecd = 57.2957795130823208767981548141052_?kind?_k * acos( 1.0_?kind?_k / a)

return                                                               ! asecd()

!  ?kind?_asecd()

end function ?kind?_asecd

?? end text asecd
?? if( single_k )then
?? copy :: asecd( single)
?? endif
?? if( double_k )then
?? copy :: asecd( double)
?? endif
?? if( quad_k )then
?? copy :: asecd( quad)
?? endif
?? text :: acscd( kind)
! **********************************************************************

!  ?kind?_acscd(): acscd() for kind ?kind?

real( kind= ?kind?_k) function ?kind?_acscd( a)

real( kind= ?kind?_k), intent( in) :: a

!  ?kind?_acscd()

continue                                                             ! acscd()

   ?kind?_acscd = 57.2957795130823208767981548141052_?kind?_k * asin( 1.0_?kind?_k / a)

return                                                               ! acscd()

!  ?kind?_acscd()

end function ?kind?_acscd

?? end text acscd
?? if( single_k )then
?? copy :: acscd( single)
?? endif
?? if( double_k )then
?? copy :: acscd( double)
?? endif
?? if( quad_k )then
?? copy :: acscd( quad)
?? endif
! **********************************************************************

!  trig_functions

! $Id: trigfunc.fpp 1.3 2003/10/03 19:44:06Z Dan Release $
! **********************************************************************

end module trig_functions                                  ! eof
