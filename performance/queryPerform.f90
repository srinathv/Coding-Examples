SUBROUTINE use_QueryPerform (elapse)
!
! Returns the total elapsed time in seconds
! based on QueryPerformanceCounter
! This is the fastest and most accurate timing routine
!
real*8, intent (out) :: elapse
!
STDCALL QUERYPERFORMANCECOUNTER 'QueryPerformanceCounter' (REF):LOGICAL*4
STDCALL QUERYPERFORMANCEFREQUENCY 'QueryPerformanceFrequency' (REF):LOGICAL*4
!
real*8 :: freq = 1
logical*4 :: first = .true.
integer*8 :: start = 0
integer*8 :: num
logical*4 :: ll
!
! Calibrate this time using QueryPerformanceFrequency
if (first) then
num = 0
ll = QueryPerformanceFrequency (num)
freq = 1.0d0 / dble (num)
start = 0
ll = QueryPerformanceCounter (start)
first = .false.
end if
!
num = 0
ll = QueryPerformanceCounter (num)
elapse = dble (num-start) * freq
return
end
