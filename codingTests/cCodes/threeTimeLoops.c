#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

int main(int argc, char** argv) {
//make comp start and stop time (for loop)
//catch overall time
//think about  how these intervals are to be treated across ranks
//maybe do an all reduce of loop times per rank as a runtime option 

    time_t mpiendwait, compendwait, looptime;
    time_t loop1time, loop2time, loop3time;
    time_t endtime;
    time_t starttime = time(NULL);
    time_t delay1 = 10; // end loop after this time has elapsed
    time_t delay2 = 5; // end loop after this time has elapsed
    time_t delay3 = 15; // end loop after this time has elapsed

    printf("start time is : %s", ctime(&starttime));

// section 1
    looptime = starttime;
    endtime = starttime + delay1;
    while (looptime < endtime)
    {
        looptime = time(NULL);
    }
printf("end loop 1 time is %s", ctime(&endtime));

// section 2
    starttime = time(NULL);
    looptime = starttime;
    endtime = starttime + delay2;
    while (looptime < endtime)
    {
        looptime = time(NULL);
    }
printf("end loop 2 time is %s", ctime(&endtime));


// section 3
    starttime = time(NULL);
    looptime = starttime;
    endtime = starttime + delay3;
    while (looptime < endtime)
    {
        looptime = time(NULL);
    }
printf("end loop 3 time is %s", ctime(&endtime));
}