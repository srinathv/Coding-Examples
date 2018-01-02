#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

int main(int argc, char** argv) {
//make comp start and stop time (for loop)
//catch overall time
//think about  how these intervals are to be treated across ranks
//maybe do an all reduce of loop times per rank as a runtime option 

    time_t mpiendwait, compendwait, endtime,looptime;
    time_t starttime = time(NULL);
    time_t delay = 10; // end loop after this time has elapsed

    endtime = starttime + delay;

    printf("start time is : %s", ctime(&starttime));

    looptime = starttime;
    while (looptime < endtime)
    {
        looptime = time(NULL);
    }
      printf("end time is %s", ctime(&endtime));
}