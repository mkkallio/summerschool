! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  ! part 1: start communication
  subroutine exchange_init(field0, parallel)
    use mpi

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(inout) :: parallel
    integer :: ierr
    integer :: robj(2), sobj(2)

    ! TODO
    !call MPI_Send_init(field0%data(:,0), field0%ny, MPI_DOUBLE_PRECISION, parallel%nleft, 0, MPI_COMM_WORLD, sobj, ierr)
    !call MPI_Recv_init(field0%data(:,(field0%ny+1)), field0%ny, MPI_DOUBLE_PRECISION, parallel%nright, 0, MPI_COMM_WORLD, robj, ierr)
    
    ! send to left, receive from right
    call MPI_Isend(field0%(:,0), field%ny, MPI_DOUBLE_PRECISION, parallel%nleft, 0, MPI_COMM_WORLD, sobj(1), ierr)
    call MPI_IRecv(field0%data(:,field0%ny), field0%ny, MPI_DOUBLE_PRECISION, parallel%nright, 0, MPI_COMM_WORLD, robj(1), ierr)

    ! send to right, receive from left    
    call MPI_Isend(field0%(:,field0%ny), field%ny, MPI_DOUBLE_PRECISION, parallel%nright, 0, MPI_COMM_WORLD, sobj(2), ierr)
    call MPI_IRecv(field0%data(:,0), field0%ny, MPI_DOUBLE_PRECISION, parallel%nright, 0, MPI_COMM_WORLD, robj(2), ierr)

  end subroutine exchange_init

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  ! Update only the border-independent part of the field
  subroutine evolve_interior(curr, prev, a, dt)
    implicit none
    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    ! TODO
    do i = 1, nx
       do j = 2, ny-1
         curr%data(i,j) = prev%data(i,j) + a * dt &
               & ((prev%data(i-i,j) - 2.0 * prev%data(i,j) + prev%data(i+1,j)) / curr%dx**2 &
               &   (prev%data(i,j-1) - 2.0 * prev%data(i,j) + Ãprev%data(i,j+1)) / curr%dy**2
       end do
    end do

  end subroutine evolve_interior

  ! Finalize the non-blocking communication
  subroutine exchange_finalize(parallel)
    use mpi
    implicit none
    type(parallel_data), intent(inout) :: parallel
    integer :: ierr

    ! TODO
  end subroutine exchange_finalize

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  ! Update only the border-dependent part
  subroutine evolve_edges(curr, prev, a, dt)

    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    ! TODO

  end subroutine evolve_edges

end module core
