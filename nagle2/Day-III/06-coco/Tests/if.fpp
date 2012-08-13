?? logical :: debug = .false.
?? if( debug )then
! one
?? elseif( debug .or. .not. debug )then
! ok
?? else
! never
?? end if
?? symbols
