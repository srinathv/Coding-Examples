?? logical :: debug
?? integer :: counter = 42
?? macro :: goo = oobleck
?? macro, parens :: hypot( x, y) = sqrt( ?x?*?x? + ?y?*?y?)
?? integer :: i
!
?? include '../document.inc'
! ?file? ?line?
! ?date? ?time?
! ?time? ?date?
! ?date?
! ?date? ?coco?
! ?coco?
! ?setfile?
! ?logfile?
! ?output?
! ?cmdline?
! debug y/n ?debug?
! how many = ?counter?
poo = ?goo?
z = ?hypot?(a, b)
z = ?hypot?(x2-x1, y2-y1)
z = ?hypot?(a+b,(a-b))
z = ?hypot?((x2-x1), y2-y1)
?? symbols