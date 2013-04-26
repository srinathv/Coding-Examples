
/* 
   This timer returns wall clock time.   
   The first entry is current time in seconds.
   The second entry is microseconds. 
*/ 
void wtime_(double tim[2], int *ierr2 )
{
   struct timeval time1; 
   int ierr; 
   double elap; 
   ierr = gettimeofday(&time1, NULL) ; 
   *ierr2 = ierr; 
   if (ierr != 0 ) printf("bad return of gettimeofday, ierr = %d \n",ierr);  
   tim[0] = time1.tv_sec;
   tim[1] = time1.tv_usec; 
}
/* 
   This timer returns the difference between current
   wall clock time and the one returned by wtime_ and 
   returns a double precision number 
*/ 
double wdiff_(double tim[2], int *ierr2 )
{
   struct timeval time1;
   int ierr; 
   double wdiff; 
   ierr = gettimeofday(&time1, NULL); 
   *ierr2 = ierr; 
   if (ierr != 0 ) printf("bad return of gettime of day, ierr = %d \n", ierr); 
   tim[0] = time1.tv_sec - tim[0];
   tim[1] = time1.tv_usec - tim[1]; 
   wdiff = 1.e6*tim[0] + tim[1]; 
   return ( wdiff/1.e6) ;   
}   

