! ---------------------------------------------------------------------

program test_accumulator

use, intrinsic :: iso_fortran_env, only: output_unit, compiler_version, compiler_options

use :: normalized_accumulator_type, wk => acc_wk

implicit none

! ---------------------------------------------------------------------

real( wk), parameter :: zero = 0.0_wk

   real( wk), dimension( 1: 1000000)  :: addends

   real( wk), dimension( 1: 10000) :: junk

   real( wk), dimension( 1: 100) :: totals, total_accs

   real( wk) :: total_ave, total_sd, acc_ave, acc_sd, sd_proportion, ave_proportion, ulps

   real :: time_start, time_initialize, time_run, time_report, time_all

   integer :: i

! ---------------------------------------------------------------------

continue

   write( unit= output_unit, fmt= '( a)') compiler_version()
   write( unit= output_unit, fmt= '( a)') compiler_options()

   call cpu_time( time_start)

!  start the rng

   call warm_rng( junk, output_unit)

! ---------------------------------------------------------------------

!  get some numbers to add

   fill_gaussian: do i = lbound( addends, dim= 1), ubound( addends, dim= 1)

      addends( i) = gaussian()

   end do fill_gaussian

   call cpu_time( time_initialize)

!  run a bunch of sums

   fill_totals: do i = lbound( totals, dim= 1), ubound( totals, dim= 1)

!  compute sums

      call run_sums( addends, totals( i), total_accs( i))

      call reorder( addends)

   end do fill_totals

   call cpu_time( time_run)

! ---------------------------------------------------------------------

!  make simple statistics

   total_ave = sum( totals) / real( size( totals), wk)
   total_sd = sqrt( sum( (totals - total_ave)**2) / real( size( totals) - 1, wk))

   acc_ave = sum( total_accs) / real( size( total_accs), wk)
   acc_sd = sqrt( sum( (total_accs - acc_ave)**2) / real( size( total_accs) - 1, wk))

   ave_proportion = ( total_ave - acc_ave) / acc_ave
   ulps = abs( ave_proportion) / epsilon( ave_proportion)
   sd_proportion = ( total_sd - acc_sd) / acc_sd

! ---------------------------------------------------------------------

!  report

   write( unit= *, fmt= '( "sum average: ", es26.17/ "standard deviation: ", es26.17)') total_ave, total_sd

   write( unit= *, fmt= '( "accumulator average: ", es26.17/ "standard deviation: ", es26.17)') acc_ave, acc_sd

   write( unit= *, fmt= '( "proportional sum ave - accumulator ave: ", en26.17, a, en26.17, a)') ave_proportion, ' (', ulps, ')'
   write( unit= *, fmt= '( "proportional sum sd - accumulator sd: ", en26.17)') sd_proportion

   call cpu_time( time_report)

!  report time used

   time_all = time_report - time_start
   time_report = time_report - time_run
   time_run = time_run - time_initialize
   time_initialize = time_initialize - time_start

   write( unit= *, fmt= '( "cpu time"/ "initialize: ", f8.3/ "run: ", f8.3)') time_initialize, time_run
   write( unit= *, fmt= '( "report: ", f8.3/ "total: ", f8.3)') time_report, time_all

stop 'normal exit'

! ---------------------------------------------------------------------

!  end of execution-- internal procedures below

contains

! ---------------------------------------------------------------------

!  seed the rng from the clock and consume size( buffer) rns

subroutine warm_rng( buffer, log_unit)

real( wk), dimension( :), intent( out) :: buffer
integer, optional, intent( in) :: log_unit

integer, dimension( *), parameter :: p1 = &
   [ 1500007, 1500019, 1500041, 1500043, 1500047, 1500061, 1500071, 1500073, 1500101, 1500113, &
     1500127, 1500133, 1500139, 1500143, 1500151, 1500157, 1500181, 1500229, 1500241, 1500269, &
     1500277, 1500283, 1500293, 1500337, 1500341, 1500347, 1500349, 1500353, 1500371, 1500379, &
     1500397, 1500407, 1500409, 1500413, 1500419, 1500463, 1500467, 1500469, 1500479, 1500491, &
     1500503, 1500511, 1500517, 1500523, 1500529, 1500533, 1500593, 1500613, 1500619, 1500643, &
     1500647, 1500649, 1500691, 1500701, 1500703, 1500713, 1500731, 1500739, 1500761, 1500767, &
     1500769, 1500781, 1500787, 1500797, 1500799, 1500817, 1500823, 1500827, 1500833, 1500839, &
     1500847, 1500853, 1500857, 1500859, 1500871, 1500893, 1500899, 1500929, 1500931, 1500937, &
     1500973, 1500991, 1500997, 1501009, 1501021, 1501037, 1501043, 1501081, 1501139, 1501169, &
     1501177, 1501193, 1501207, 1501217, 1501223, 1501229, 1501261, 1501303, 1501307, 1501333, &
     1501343, 1501351, 1501363, 1501369, 1501411, 1501427, 1501429, 1501441, 1501447, 1501471, &
     1501481, 1501483, 1501499, 1501501, 1501523, 1501529, 1501541, 1501561, 1501573, 1501583, &
     1501597, 1501607, 1501613, 1501639, 1501663, 1501667, 1501673, 1501679, 1501681, 1501699, &
     1501723, 1501777, 1501781, 1501783, 1501807, 1501811, 1501837, 1501847, 1501849, 1501859, &
     1501873, 1501889, 1501897, 1501901, 1501909, 1501921, 1501937, 1501943, 1501949, 1501957, &
     1501961, 1501999, 1502021, 1502023, 1502041, 1502047, 1502057, 1502063, 1502093, 1502099, &
     1502101, 1502141, 1502143, 1502161, 1502183, 1502191, 1502201, 1502203, 1502209, 1502219, &
     1502227, 1502233, 1502269, 1502297, 1502309, 1502323, 1502327, 1502329, 1502381, 1502407, &
     1502419, 1502437, 1502467, 1502471, 1502503, 1502551, 1502563, 1502569, 1502581, 1502591, &
     1502621, 1502629, 1502639, 1502651, 1502687, 1502689, 1502717, 1502719, 1502723, 1502741 ]

integer, dimension( *), parameter :: p2 = &
   [ 1502747, 1502759, 1502771, 1502801, 1502819, 1502827, 1502861, 1502863, 1502869, 1502887, &
     1502909, 1502923, 1502929, 1502933, 1502939, 1502947, 1502959, 1502971, 1502989, 1502993, &
     1503017, 1503031, 1503037, 1503043, 1503049, 1503053, 1503059, 1503091, 1503113, 1503127, &
     1503137, 1503149, 1503163, 1503169, 1503181, 1503233, 1503241, 1503247, 1503253, 1503263, &
     1503269, 1503287, 1503311, 1503317, 1503319, 1503329, 1503353, 1503367, 1503371, 1503373, &
     1503377, 1503401, 1503419, 1503431, 1503461, 1503473, 1503479, 1503499, 1503503, 1503517, &
     1503521, 1503529, 1503583, 1503611, 1503613, 1503637, 1503647, 1503653, 1503659, 1503661, &
     1503683, 1503713, 1503721, 1503731, 1503739, 1503751, 1503767, 1503781, 1503787, 1503811, &
     1503823, 1503829, 1503847, 1503863, 1503881, 1503883, 1503899, 1503913, 1503919, 1503937, &
     1503941, 1503959, 1503961, 1503967, 1503989, 1504033, 1504037, 1504057, 1504067, 1504073, &
     1504093, 1504103, 1504117, 1504121, 1504147, 1504157, 1504171, 1504187, 1504231, 1504247, &
     1504267, 1504271, 1504289, 1504297, 1504319, 1504339, 1504379, 1504409, 1504411, 1504417, &
     1504421, 1504429, 1504463, 1504469, 1504471, 1504487, 1504493, 1504501, 1504513, 1504519, &
     1504537, 1504543, 1504571, 1504579, 1504583, 1504589, 1504609, 1504627, 1504631, 1504651, &
     1504661, 1504663, 1504669, 1504673, 1504681, 1504691, 1504693, 1504697, 1504717, 1504733, &
     1504739, 1504747, 1504757, 1504777, 1504793, 1504801, 1504813, 1504817, 1504831, 1504843, &
     1504847, 1504859, 1504861, 1504879, 1504903, 1504907, 1504949, 1504961, 1504967, 1504969, &
     1504981, 1504991, 1504999, 1505003, 1505011, 1505033, 1505083, 1505087, 1505089, 1505093, &
     1505099, 1505107, 1505111, 1505117, 1505131, 1505137, 1505167, 1505173, 1505177, 1505183, &
     1505191, 1505201, 1505209, 1505227, 1505243, 1505261, 1505279, 1505291, 1505293, 1505311 ]

integer, dimension( *), parameter :: p3 = &
   [ 1505323, 1505341, 1505353, 1505369, 1505381, 1505407, 1505411, 1505417, 1505431, 1505437, &
     1505443, 1505447, 1505459, 1505489, 1505507, 1505519, 1505521, 1505563, 1505587, 1505591, &
     1505599, 1505611, 1505657, 1505659, 1505681, 1505683, 1505687, 1505711, 1505723, 1505729, &
     1505737, 1505743, 1505747, 1505753, 1505761, 1505773, 1505797, 1505813, 1505831, 1505837, &
     1505849, 1505851, 1505873, 1505893, 1505899, 1505929, 1505953, 1505983, 1505993, 1506007, &
     1506023, 1506031, 1506059, 1506077, 1506079, 1506091, 1506103, 1506121, 1506137, 1506157, &
     1506163, 1506179, 1506191, 1506199, 1506203, 1506223, 1506229, 1506257, 1506269, 1506287, &
     1506317, 1506341, 1506359, 1506371, 1506389, 1506391, 1506413, 1506433, 1506443, 1506457, &
     1506473, 1506487, 1506493, 1506497, 1506499, 1506509, 1506511, 1506551, 1506553, 1506559, &
     1506563, 1506587, 1506607, 1506611, 1506613, 1506619, 1506623, 1506641, 1506649, 1506653, &
     1506689, 1506697, 1506721, 1506731, 1506733, 1506749, 1506779, 1506781, 1506797, 1506803, &
     1506809, 1506823, 1506839, 1506851, 1506877, 1506887, 1506889, 1506907, 1506917, 1506929, &
     1506943, 1506959, 1506977, 1506979, 1506997, 1507007, 1507019, 1507039, 1507057, 1507069, &
     1507073, 1507091, 1507097, 1507111, 1507123, 1507139, 1507141, 1507153, 1507171, 1507183, &
     1507211, 1507229, 1507291, 1507301, 1507321, 1507369, 1507379, 1507421, 1507423, 1507427, &
     1507439, 1507453, 1507469, 1507481, 1507483, 1507487, 1507501, 1507531, 1507559, 1507591, &
     1507603, 1507607, 1507609, 1507613, 1507637, 1507651, 1507657, 1507687, 1507697, 1507699, &
     1507729, 1507763, 1507769, 1507771, 1507789, 1507813, 1507837, 1507841, 1507853, 1507867, &
     1507879, 1507889, 1507907, 1507921, 1507993, 1507997, 1508033, 1508047, 1508051, 1508063, &
     1508077, 1508081, 1508093, 1508113, 1508131, 1508141, 1508147, 1508173, 1508197, 1508207 ]

integer, dimension( *), parameter :: p4 = &
   [ 1508219, 1508249, 1508251, 1508263, 1508279, 1508281, 1508293, 1508303, 1508321, 1508323, &
     1508383, 1508389, 1508401, 1508407, 1508413, 1508417, 1508449, 1508459, 1508471, 1508473, &
     1508489, 1508509, 1508519, 1508531, 1508561, 1508579, 1508587, 1508621, 1508623, 1508627, &
     1508629, 1508651, 1508659, 1508671, 1508687, 1508693, 1508707, 1508711, 1508719, 1508723, &
     1508729, 1508743, 1508753, 1508779, 1508789, 1508797, 1508803, 1508813, 1508833, 1508851, &
     1508867, 1508873, 1508879, 1508893, 1508909, 1508911, 1508921, 1508929, 1508933, 1508939, &
     1508951, 1508953, 1508959, 1508977, 1508981, 1508993, 1509019, 1509031, 1509059, 1509061, &
     1509071, 1509077, 1509097, 1509127, 1509133, 1509143, 1509163, 1509187, 1509197, 1509203, &
     1509229, 1509269, 1509289, 1509307, 1509331, 1509353, 1509367, 1509371, 1509377, 1509407, &
     1509427, 1509437, 1509439, 1509457, 1509463, 1509491, 1509509, 1509517, 1509523, 1509533, &
     1509551, 1509553, 1509581, 1509587, 1509589, 1509623, 1509631, 1509643, 1509659, 1509701, &
     1509727, 1509733, 1509737, 1509749, 1509757, 1509779, 1509841, 1509857, 1509863, 1509887, &
     1509899, 1509913, 1509919, 1509929, 1509941, 1509947, 1509953, 1509961, 1509967, 1509971, &
     1509997, 1510013, 1510021, 1510039, 1510043, 1510049, 1510057, 1510087, 1510109, 1510121, &
     1510141, 1510147, 1510163, 1510189, 1510199, 1510207, 1510213, 1510217, 1510219, 1510259, &
     1510273, 1510279, 1510307, 1510309, 1510319, 1510321, 1510337, 1510339, 1510343, 1510357, &
     1510361, 1510363, 1510373, 1510391, 1510393, 1510417, 1510423, 1510427, 1510429, 1510469, &
     1510477, 1510489, 1510493, 1510507, 1510511, 1510541, 1510573, 1510583, 1510591, 1510601, &
     1510643, 1510651, 1510669, 1510679, 1510681, 1510687, 1510693, 1510703, 1510741, 1510753, &
     1510757, 1510759, 1510763, 1510777, 1510781, 1510799, 1510819, 1510843, 1510853, 1510867 ]

integer, dimension( *), parameter :: p5 = &
   [ 1510877, 1510889, 1510897, 1510913, 1510921, 1510933, 1510961, 1510963, 1510967, 1510991, &
     1511017, 1511021, 1511047, 1511053, 1511099, 1511101, 1511119, 1511129, 1511143, 1511179, &
     1511201, 1511207, 1511227, 1511231, 1511233, 1511239, 1511243, 1511269, 1511273, 1511287, &
     1511291, 1511303, 1511327, 1511329, 1511371, 1511387, 1511423, 1511429, 1511441, 1511443, &
     1511449, 1511459, 1511527, 1511533, 1511539, 1511563, 1511569, 1511597, 1511599, 1511617, &
     1511633, 1511647, 1511651, 1511663, 1511669, 1511687, 1511689, 1511723, 1511737, 1511743, &
     1511747, 1511779, 1511791, 1511801, 1511819, 1511821, 1511863, 1511891, 1511897, 1511911, &
     1511921, 1511927, 1511933, 1511941, 1511947, 1511953, 1511971, 1511977, 1511999, 1512019, &
     1512023, 1512029, 1512041, 1512083, 1512097, 1512109, 1512113, 1512127, 1512169, 1512197, &
     1512209, 1512221, 1512223, 1512233, 1512241, 1512253, 1512281, 1512283, 1512289, 1512293, &
     1512299, 1512307, 1512311, 1512323, 1512331, 1512361, 1512383, 1512421, 1512431, 1512479, &
     1512481, 1512493, 1512517, 1512527, 1512547, 1512551, 1512557, 1512559, 1512569, 1512607, &
     1512619, 1512629, 1512661, 1512683, 1512689, 1512691, 1512703, 1512713, 1512751, 1512767, &
     1512773, 1512787, 1512809, 1512817, 1512827, 1512829, 1512857, 1512877, 1512923, 1512943, &
     1512947, 1512961, 1513013, 1513019, 1513021, 1513033, 1513037, 1513049, 1513067, 1513069, &
     1513073, 1513091, 1513093, 1513111, 1513117, 1513121, 1513123, 1513139, 1513151, 1513159, &
     1513163, 1513199, 1513207, 1513219, 1513229, 1513271, 1513273, 1513277, 1513319, 1513321, &
     1513361, 1513367, 1513381, 1513387, 1513397, 1513399, 1513417, 1513427, 1513429, 1513441, &
     1513453, 1513487, 1513489, 1513511, 1513517, 1513529, 1513531, 1513537, 1513543, 1513553, &
     1513573, 1513583, 1513591, 1513601, 1513609, 1513619, 1513621, 1513651, 1513657, 1513661 ]

integer, dimension( *), parameter :: primes = [ p5, p4, p3, p2, p1 ]

   integer, dimension( :), allocatable :: seed

   integer :: seed_size, astat, rtc, scr_unit

   integer, dimension( 8) :: dt_values

continue

!  get the seed size and allocate the seed array

   call random_seed( size= seed_size)

   allocate( seed( seed_size), stat= astat)

   allocate_error: if( astat > 0 )then

      stop 'allocate rng seed error'

   end if allocate_error

!  get the date and time values array to fudge a seed

   call date_and_time( values= dt_values)

   dt_values = ( dt_values( 1: 8) + dt_values( 8: 1: -1))**2

!  use the real time clock to further mix the seed

   call system_clock( count= rtc)

   even_odd: if( iand( rtc, 1) == 1 )then

      dt_values( 1: 8: 2) = dt_values( 1: 8: 2) + rtc

   else even_odd

      dt_values( 2: 8: 2) = dt_values( 2: 8: 2) + rtc

   end if even_odd

!  setting the seed depends on whether the seed array is larger than the time array

   dt_v_seed_size: if( size( seed) > size( dt_values) )then

      seed( 1: size( dt_values)) = dt_values
      seed( size( dt_values) + 1: ) = primes( dt_values( 7) + size( dt_values) + 1: dt_values( 7) + size( seed) )

   else dt_v_seed_size

      seed = dt_values( 1: size( seed))

   end if dt_v_seed_size

!  log the seed if asked

   log_seed: if( present( log_unit) )then

      write( unit= log_unit, fmt= '( a, i0, a/ (6i12))') 'seed (size ', seed_size, ')', seed

   end if log_seed

!  set the seed

   call random_seed( put= seed)

   deallocate( seed)

!  throw away a bunch of rns to get things warmed

   call random_number( harvest= buffer)

   open( newunit= scr_unit, status= 'scratch', form= 'unformatted')

   write( unit= scr_unit) buffer

   close( unit= scr_unit, status= 'delete')

return

end subroutine warm_rng

! ---------------------------------------------------------------------

!  sum the addends via addition and the accumulator variable

pure subroutine run_sums( addends, total, total_acc)

real( wk), dimension( :), intent( in) :: addends
real( wk), intent( out) :: total, total_acc

   type( normalized_accumulator_t) :: acc

   integer :: i

continue

!  initialize the accumulators

   total = zero
   call initialize( acc)

!  loop through the array adding numbers

   sum_total: do i = lbound( addends, dim= 1), ubound( addends, dim= 1)

      total = total + addends( i)
      acc = acc + addends( i)

   end do sum_total

   total_acc = acc

return

end subroutine run_sums

! ---------------------------------------------------------------------

!  gaussian() is based on the gasdev procedure of Numerical Recipes

function gaussian( mu, sigma) result( g)
real( wk) :: g

real( wk), optional :: mu
real( wk), optional :: sigma

real( wk), parameter :: zero = 0.0_wk
real( wk), parameter :: one = 1.0_wk
real( wk), parameter :: two = 2.0_wk

   real( wk), dimension( 1: 2) :: v
   real( wk) :: fac, rsq

   real( wk), save :: g_save
   logical, save :: make_more = .true.

continue

!  gaussian numbers are made two at a time

   need_make_more: if( make_more )then

!  must have a pair in ( 0, 1)

      in_zero_one: do

         call random_number( harvest= v)
         v = two * v - one
         rsq = v( 1)**2 + v( 2)**2

         if( rsq > zero .and. rsq < one ) exit in_zero_one

      end do in_zero_one

      fac = sqrt( -two * log( rsq) / rsq)

!  save one for next time and use the other

      g_save = v( 1) * fac
      g = v( 2) * fac

      make_more = .false.

   else need_make_more

!  use the one saved last time

      g = g_save

      make_more = .true.

   end if need_make_more

!  if have standard deviation, use it

   set_sigma: if( present( sigma) )then

      g = g * sigma

   end if set_sigma

!  if have mean, use it

   set_mu: if( present( mu) )then

      g = g + mu

   end if set_mu

return

end function gaussian

! ---------------------------------------------------------------------

!  use the rng to reorder the addends

subroutine reorder( a)

real( wk), dimension( :), intent( in out) :: a

   real( wk), dimension( 1: size( a)) :: rns

   integer, dimension( 1: size( a)) :: r_idx

   integer :: i

continue

!  make a random index set for the index of a

   call random_number( harvest= rns)

   r_idx = int( rns * real( size( a), wk)) + 1

!  swap elements of a- it doesn't matter if some are swapped more than once

   reorder_swap: do i = lbound( a, dim= 1), ubound( a, dim= 1)

      in_bounds: if( r_idx( i) >= lbound( a, dim= 1) .and. r_idx( i) <= ubound( a, dim= 1) )then

         call swap( a( i), a( r_idx( i)) )

      end if in_bounds

   end do reorder_swap

return

end subroutine reorder

! ---------------------------------------------------------------------

!  swap a and b

pure subroutine swap( a, b)
real( wk), intent( in out) :: a, b

   real( wk) :: t

continue

   t = a
   a = b
   b = t

return

end subroutine swap

! ---------------------------------------------------------------------

end program test_accumulator
