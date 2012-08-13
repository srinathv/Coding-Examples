 
program NumericModel
 
  use Fortran_Kind_Module
  implicit none 
 
! LOCAL VARIABLES:
  integer(kind=  Int_01), parameter ::   Int_01_var = 1_Int_01  
  integer(kind=  Int_02), parameter ::   Int_02_var = 1_Int_02  
  integer(kind= Int_Def), parameter ::  Int_Def_var = 1_Int_Def 
  integer(kind=  Int_08), parameter ::   Int_08_var = 1_Int_08  
  integer(kind=  Int_16), parameter ::   Int_16_var = 1_Int_16  
 
  real(kind=  Single),    parameter ::   Single_var = 1.0_Single  
  real(kind=  Double),    parameter ::   Double_var = 1.0_Double  
  real(kind=  Extnd1),    parameter ::   Extnd1_var = 1.0_Extnd1  
  real(kind=  Extnd2),    parameter ::   Extnd2_var = 1.0_Extnd2  
 
! EXECUION:
 
  write(unit=*,fmt="(TR1,A)") &
        " " &
       ,"            The Numerical Models of" &
       ,"         Integer & Real/Complex Types" &
       ," " &
       ,"     available with this Computer/Compiler" &
       ," " &
       ," " &
       ," Author: Werner W Schulz, (C) 1998" &
       ," " &
       ," This Programme was generated automatically by KindFinder," &
       ," Author: Werner W Schulz, (C) 1998" &
       ," The Names used below are those used in KindFinder" &
       ," "
 
 
  ! IntegerModel:
  write(unit=*,fmt="(//TR1,A)") &
        "INTEGER MODEL:"
 
  write(unit=*,fmt="(TR1,A,5A20)") &
        " Name:       " &
       ,"  Int_01" &  
       ,"  Int_02" &  
       ," Int_Def" &  
       ,"  Int_08" &  
       ,"  Int_16"    
 
 
  write(unit=*,fmt="(TR1,A,5I20)") &
        " KIND:       " &
        ,kind(  Int_01_var) &  
        ,kind(  Int_02_var) &  
        ,kind( Int_Def_var) &  
        ,kind(  Int_08_var) &  
        ,kind(  Int_16_var)    
 
 
  write(unit=*,fmt="(TR1,A,5I20)") &
        " DIGITS:     " &
        ,digits(  Int_01_var) &  
        ,digits(  Int_02_var) &  
        ,digits( Int_Def_var) &  
        ,digits(  Int_08_var) &  
        ,digits(  Int_16_var)    
 
 
  write(unit=*,fmt="(TR1,A,5I20)") &
        " RADIX:      " &
        ,radix(  Int_01_var) &  
        ,radix(  Int_02_var) &  
        ,radix( Int_Def_var) &  
        ,radix(  Int_08_var) &  
        ,radix(  Int_16_var)    
 
 
  write(unit=*,fmt="(TR1,A,5I20)") &
        " RANGE:      " &
        ,range(  Int_01_var) &  
        ,range(  Int_02_var) &  
        ,range( Int_Def_var) &  
        ,range(  Int_08_var) &  
        ,range(  Int_16_var)    
 
 
  write(unit=*,fmt="(TR1,A,5I20)") &
        " HUGE:       " &
        ,huge(  Int_01_var) &  
        ,huge(  Int_02_var) &  
        ,huge( Int_Def_var) &  
        ,huge(  Int_08_var) &  
        ,huge(  Int_16_var)    
 
 
  ! FloatingPointModel:
  write(unit=*,fmt="(//TR1,A)") &
        "FLOATINGPOINT MODEL (Real/Complex):"
 
  write(unit=*,fmt="(TR1,A,4A20)") &
        " Name:       " &
       ,"  Single" &  
       ,"  Double" &  
       ,"  Extnd1" &  
       ,"  Extnd2"    
 
 
  write(unit=*,fmt="(TR1,A,4I20)") &
        " KIND:       " &
        ,kind(  Single_var) &  
        ,kind(  Double_var) &  
        ,kind(  Extnd1_var) &  
        ,kind(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4I20)") &
        " DIGITS:     " &
        ,digits(  Single_var) &  
        ,digits(  Double_var) &  
        ,digits(  Extnd1_var) &  
        ,digits(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4I20)") &
        " RADIX:      " &
        ,radix(  Single_var) &  
        ,radix(  Double_var) &  
        ,radix(  Extnd1_var) &  
        ,radix(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4I20)") &
        " MINEXPONENT:" &
        ,minexponent(  Single_var) &  
        ,minexponent(  Double_var) &  
        ,minexponent(  Extnd1_var) &  
        ,minexponent(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4I20)") &
        " MAXEXPONENT:" &
        ,maxexponent(  Single_var) &  
        ,maxexponent(  Double_var) &  
        ,maxexponent(  Extnd1_var) &  
        ,maxexponent(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4I20)") &
        " PRECISION:  " &
        ,precision(  Single_var) &  
        ,precision(  Double_var) &  
        ,precision(  Extnd1_var) &  
        ,precision(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4I20)") &
        " RANGE:      " &
        ,range(  Single_var) &  
        ,range(  Double_var) &  
        ,range(  Extnd1_var) &  
        ,range(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4ES20.3)") &
        " EPSILON:    " &
        ,epsilon(  Single_var) &  
        ,epsilon(  Double_var) &  
        ,epsilon(  Extnd1_var) &  
        ,epsilon(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4ES20.3)") &
        " HUGE:       " &
        ,huge(  Single_var) &  
        ,huge(  Double_var) &  
        ,huge(  Extnd1_var) &  
        ,huge(  Extnd2_var)    
 
 
  write(unit=*,fmt="(TR1,A,4ES20.3)") &
        " TINY:       " &
        ,tiny(  Single_var) &  
        ,tiny(  Double_var) &  
        ,tiny(  Extnd1_var) &  
        ,tiny(  Extnd2_var)    
 
 
 
  write(unit=*,fmt="(//)") 
 
 
  stop
 
end program NumericModel
