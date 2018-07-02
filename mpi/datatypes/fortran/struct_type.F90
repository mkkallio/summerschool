program datatype_struct
  use mpi
  implicit none
  type particle
     real :: coords(3)
     integer :: charge
     character(len=2) :: label
  end type particle
  integer, parameter :: n = 1000
  integer :: i, ierror,  myid,  ntasks, tag
  type(particle) :: particles(n)

  integer, parameter :: cnt=3
  integer:: particle_mpi_type, temp_type
  integer:: types(cnt),blocklen(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: disp(cnt), tmp_disp(2)
  integer(KIND=MPI_ADDRESS_KIND) :: lb, extent
  real(8) :: t1,t2

  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)

  ! insert some data for the particle struct
  if(myid == 0) then
     do i = 1, n
        call random_number(particles(i)%coords)
        particles(i)%charge = 54
        particles(i)%label = 'Xe'
     end do
  end if

  ! TODO: define the datatype for type particle
  blocklen = [3,1,2]
  call mpi_get_address(particles(1)%coords(1), disp(1), ierror)
  call mpi_get_address(particles(1)%charge, disp(2), ierror)
  call mpi_get_address(particles(1)%label, disp(3), ierror)
  disp(3) = disp(3) - disp(1)
  disp(2) = disp(2) - disp(1)
  disp(1) = 0
  types = [MPI_DOUBLE_PRECISION, MPI_INT, MPI_CHAR]

  call mpi_type_create_struct(3, blocklen, disp, types, particle_mpi_type, ierror)
  ! TODO: Check extent.
  call mpi_type_get_extent(particle_mpi_type, lb, extent, ierror)
  ! (Not really neccessary on most systems.)
  ! TODO: resize the particle_mpi_type if needed
  if(extent /= disp(2)-disp(1)) then
     ! TODO: resize the particle_mpi_type if needed
     temp_type = particle_mpi_type
     call mpi_get_address(particles(1)%coords(1), tmp_disp(1), ierror)
     call mpi_get_address(particles(2)%coords(1), tmp_disp(2), ierror) 
     call mpi_type_create_resized(temp_type, lb, tmp_disp(2)-tmp_disp(1),  particle_mpi_type, ierror)
     !call mpi_type_create_resized(temp_type, lb, sizeof(particles(1)),  particle_mpi_type, ierror)
     call mpi_type_commit(particle_mpi_type, ierror)
     call mpi_type_free(temp_type, ierror)
  end if


  t1=MPI_WTIME()
  if(myid == 0) then
     do i=1,1000
        call MPI_SEND(particles, n, particle_mpi_type, 1, i, &
             MPI_COMM_WORLD,ierror)
     end do
  else if(myid == 1) then
     do i=1, 1000
        call MPI_RECV(particles, n, particle_mpi_type, 0, i, &
             MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
     end do
  end if
  t2=MPI_WTIME()

  write(*,*) "Time: ", myid, (t2-t1) / 1000d0
  write(*,*) "Check:", myid, particles(n)%coords(1)

  call MPI_TYPE_free(particle_mpi_type, ierror)

  call MPI_FINALIZE(ierror)


end program datatype_struct
