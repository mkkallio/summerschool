program pario
  use mpi
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
     write(error_unit, *) 'Maximum number of tasks is 64!'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
     write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call single_writer()

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine single_writer()
    implicit none

    ! TODO: Implement a function that writers the whole array of elements
    !       to a file so that single process is responsible for the file io
    ! call mpi_file_open(MPI_COMM_WORLD, 'paroo_1a.dat', mpi_mode_create + mpi_mode_wronly, )
    
    
    call mpi_gather(localvector, localsize, MPI_INT, fullvector, localsize, MPI_INT, 0, MPI_COMM_WORLD, rc)

    if(my_id == 0) then
      open(10, file='foo.dat')
      write(10,*) fullvector
      close(10) 
    end if   

  end subroutine single_writer

  subroutine reader()
    implicit none
    
    

  end subroutine reader

end program pario
