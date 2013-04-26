       program callRdtsc

       integer*8 rdtsc,t0,t1 
       t0=rdtsc(1) 
       do 1 i=1,1000000000 
1     continue 
       t1=rdtsc(2) 
       print *,t1-t0 
       write(*,'(z18,z18)') t0,t1 
       stop 
       end 
