#include <stdio.h>
#include <mpi.h>

#if !defined(__GNUC__) || defined(_CRAYC)
#error Compile this code with Gnu compiler!
#endif


int main(int argc, char *argv[])
{
    int ntasks, rank;

    ntasks = 1;
    rank = 10;

    printf("Hello world from task %d of %d\n", rank, ntasks);

    return 0;
}
