

  PROGRAM testOrDefine


#if defined(HAVE_A) || defined(HAVE_B)
  write(*,*) "IN A or B"
#else
  write(*,*) "not in A or B"
#endif

  END PROGRAM testOrDefine
