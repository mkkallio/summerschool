program loops
	implicit none
	! TODO define parameters nx and ny
	integer, parameter :: nx =10, ny = 10
	! TODO: define real-valued array A
	real, dimension(nx,ny) :: A
	
	integer :: i, j
	
	
	! TODO initialize array A here
	real :: x = nx/10
	real :: y = ny/10
	
	do i = 1, nx, 1
		do j = 1, ny, 1
		A(i,j) = i**2 + j**2
		end do
	end do


	!--------------------------------------------------
	! Print out the array
	! the ':' syntax means the whole row, see the Fortran arrays lecture
	do i = 1, nx
		write(*, '(12F6.1)') A(i,:)
	end do

end program loops
