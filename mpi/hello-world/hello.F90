program mpi_hello
  use mpi
  implicit none

  integer :: rc, rank, ntasks

  call MPI_Init()

  print *, 'Hello!'
  call MPI_Finalize()

  
end program mpi_hello
