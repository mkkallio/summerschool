program exchange
  use mpi
  implicit none
  integer, parameter :: size = 100
  integer :: rc, myid, ntasks, count
  integer :: dest, recv 
  integer :: status(MPI_STATUS_SIZE)
  integer :: message(size)
  integer :: receiveBuffer(size)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid
  if (myid < ntasks-1) then
    dest = myid + 1
  else 
    dest = 0
  end if

  if (myid == 0) then
    recv = ntasks-1
  else
    recv = myid-1
  end if
  
  write(*,*) 'Rank: ', myid, &
      ' sent ', message, ' to destination ', dest
  if (myid == 0) then
    call MPI_Sendrecv(message, size, MPI_INTEGER, dest, dest, receiveBuffer, size, MPI_INTEGER, MPI_PROC_NULL, myid ,MPI_COMM_WORLD, status, rc)
  else if (myid == ntasks-1) then
    call MPI_Sendrecv(message, size, MPI_INTEGER, MPI_PROC_NULL, dest, receiveBuffer, size, MPI_INTEGER, recv, myid ,MPI_COMM_WORLD, status, rc)
  else
    call MPI_Sendrecv(message, size, MPI_INTEGER, dest, dest, receiveBuffer, size, MPI_INTEGER, recv, myid ,MPI_COMM_WORLD, status, rc)
  end if

  write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
      ' received ', receiveBuffer(1)



  ! SAME AS ABOVE BUT WITH MPI_Send, MPI_Recv
  ! if (myid == 0) then
  !  call MPI_Send(message, size, MPI_INTEGER, dest, dest, MPI_COMM_WORLD, rc)
  !    write(*,*) 'Rank: ', myid, &
  !        ' sent ', message, ' to destination ', dest
  !
  !  call MPI_Recv(receiveBuffer, size, MPI_INTEGER, recv, myid, MPI_COMM_WORLD, status, rc)
  !
  !    write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
  !        ' received ', receiveBuffer(1)
  ! else
  !    call MPI_Recv(receiveBuffer, size, MPI_INTEGER, recv, myid, MPI_COMM_WORLD, status, rc)
  !    write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
  !        ' received ', receiveBuffer(1)
  !
  !    call MPI_Send(message, size, MPI_INTEGER, dest, dest, MPI_COMM_WORLD, rc)
  !    write(*,*) 'Rank: ', myid, &
  !        ' sent ', message, ' to destination ', dest
  !
  ! end if
  
  !write (*,*) 'message swap between 0 and 1'
  ! BELOW IS FOR 2 core message swapping !
  ! TODO: Implement sending and receiving as defined in the assignment
  !if ( myid == 0 ) then
  !   call MPI_Send(message, size, MPI_INTEGER, 1, 111, MPI_COMM_WORLD, rc)
  !   call MPI_Recv(receiveBuffer, size, MPI_INTEGER, 1, 100, MPI_COMM_WORLD, status, rc)
  !   
  !   write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
  !        ' received ', receiveBuffer(1)
  !else if (myid == 1) then
  !   call MPI_Recv(receiveBuffer, size, MPI_INTEGER, 0,111, MPI_COMM_WORLD, status, rc)
  !   call MPI_Send(message, size, MPI_INTEGER, 0, 100, MPI_COMM_WORLD, rc)
  !
  !   write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
  !        ' received ', receiveBuffer(1)
  !end if

  call mpi_finalize(rc)

end program exchange
