program mpi_hello
  use mpi
  use omp_lib
  implicit none

  integer :: rc, rank, ntasks, thread

  call MPI_Init(rc)
  call MPI_Comm_size(MPI_COMM_WORLD, ntasks, rc)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, rc)
 
  !$omp parallel private(thread)
    thread = omp_get_thread_num()
    print *, 'Hello!', rank, '/', ntasks, ' and thread ', thread
  !$omp end parallel

 call MPI_Finalize(rc)

  
end program mpi_hello
