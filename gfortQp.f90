module IFWIN

   use ISO_C_BINDING

   implicit none

   private

   public QueryPerformanceCounter

   interface

      function QueryPerformanceCounter(lpPerformanceCount) &

         bind(C, name='QueryPerformanceCounter')

         import

         implicit none

!gcc attributes stdcall :: QueryPerformanceCounter

         integer(C_INT) QueryPerformanceCounter

         integer(C_INT64_T) lpPerformanceCount

      end function QueryPerformanceCounter

   end interface

   public QueryPerformanceFrequency

   interface

      function QueryPerformanceFrequency(lpFrequency) &

         bind(C, name='QueryPerformanceFrequency')

         import

         implicit none

!gcc attributes stdcall :: QueryPerformanceFrequency

         integer(C_INT) QueryPerformanceFrequency

         integer(C_INT64_T) lpFrequency

      end function QueryPerformanceFrequency

   end interface

end module IFWIN
program test

   use IFWIN

   use ISO_C_BINDING

   implicit none

   integer(C_INT64_T) lpFrequency, lpPerformanceCount

   integer(C_INT) ll
   ll = QueryPerformanceFrequency(lpFrequency)

   ll = QueryPerformanceCounter(lpPerformanceCount)

   write(*,*) lpFrequency, lpPerformanceCount

end program test
