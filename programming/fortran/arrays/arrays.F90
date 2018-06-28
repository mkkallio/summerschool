program arrays
  implicit none
  ! TODO: Define the array A
  real, allocatable :: A(:,:)
  real :: x, y, dx, dy
  integer :: nx, ny, i, j, alloc_stat

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  dx = 1.0/real(nx-1)
  dy = 1.0/real(ny-1)

  ! TODO: allocate the array A
  allocate(A(nx,ny), stat=alloc_stat)
  if (alloc_stat /= 0) stop
  
  ! TODO: initalize the array A
  do i = 1, nx, 1
	do j = 1, ny, 1
		A(i,j) = i**2 + j**2
	end do
  end do

  ! TODO: Print out the array
  do i = 1, nx
	write(*, '(12F6.1)') A(i,:)
  end do


end program arrays
