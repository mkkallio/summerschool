program mpi_hello
  use mpi
  implicit none

  integer :: rc, rank, ntasks

  call MPI_Init(rc)
  call MPI_Comm_size(MPI_COMM_WORLD, ntasks, rc)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, rc)
 
  if (rank == 0) then  
    print *, 'Hello!', rank, '/', ntasks
  else
    print *, 'Hello from ', rank
  end if
 

 call MPI_Finalize(rc)

  
end program mpi_hello
