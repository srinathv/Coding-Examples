module component_type_mod

  !----------------------------------------------------------------------------
  ! share code & libs
  !----------------------------------------------------------------------------
  use shr_kind_mod     , only: r8 => SHR_KIND_R8
  use shr_kind_mod     , only: cs => SHR_KIND_CS
  use shr_kind_mod     , only: cl => SHR_KIND_CL

       real(r8)        , pointer       :: drv2mdl(:)  => null() ! area correction factors
end module
