
      if( .not. ( My Foo > Your Foo ) )then                                     ! ass.fpp: 1
      write( unit= *, fmt= *) "assertion failed: ass.fpp: 1: " // "( My         ! ass.fpp: 1
     &Foo > Your Foo )"                                                         ! ass.fpp: 1
      stop "assertion failed"                                                   ! ass.fpp: 1
      end if                                                                    ! ass.fpp: 1
