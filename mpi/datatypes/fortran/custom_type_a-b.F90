program datatype1
  use mpi
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  !TODO: declare variable for datatype
  integer :: vector_a, vector_b
  integer :: i, j
  integer :: stride(4), block(4)

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  if (rank == 0) then
     write(*,*) 'Data in rank 0'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if


  !TODO: create datatype describing one row, use mpi_type_vector
  ! create block and stride
  do i = 1, 4
   block(i) = i
   stride(i) = i-1 + (i-1)*16
  end do

  call mpi_type_vector(8, 1, 8, MPI_INT, vector_a, ierr)
  call mpi_type_indexed(10, block, stride, MPI_INT, vector_b, ierr)
  call mpi_type_commit(vector_a, ierr)
  call mpi_type_commit(vector_b, ierr)
  

  !TODO: send first row of matrix from rank 0 to 1
  if (rank == 0) then
    call mpi_send(array, 1, vector_b, 1, 0, MPI_COMM_WORLD, ierr)
  else
    call mpi_recv(array(1,1), 1, vector_b, 0,0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
  end if

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype

  call mpi_finalize(ierr)

end program datatype1
