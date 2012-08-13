!?>?? assert ( My Foo > Your Foo )
if( .not. ( My Foo > Your Foo ) )then
write( unit= *, fmt= *) "assertion failed: ass.fpp: 1: " // "( My Foo > Your Foo )"
stop "assertion failed"
end if
!?>?? This was produced using the following SET file
