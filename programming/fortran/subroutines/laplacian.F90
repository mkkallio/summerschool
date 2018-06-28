module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01

contains

  subroutine initialize(array)
    ! TODO: implement a subroutine that initializes the input array
	integer :: i, j, nx, ny
	real, allocatable :: array(:,:)
	
	nx = size(array, dim=1)
	ny = size(array, dim=2)
	array = 0
	
	do i = 1, nx, 1
		do j = 1, ny, 1
		array(i,j) = i**2 + j**2
		end do
	end do

  end subroutine initialize

  subroutine laplacian(current, previous)
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "previous" and returns it as an array "current"
	integer :: i, j, nx, ny
	real, allocatable :: current(:,:), previous(:,:)
	
	current = 0
	nx = size(previous, dim=1)
	ny = size(previous, dim=2)
	
	do i = 2, nx-1
		do j = 2, ny-1
			current(i,j) = (previous(i-1,j) - (2*previous(i,j)) + previous(i+1,j)) / dx**2 &
						+ (previous(i,j-1) - (2*previous(i,j)) + previous(i,j+1)) / dy**2
		end do
	end do
  end subroutine laplacian

  subroutine write_field(array)
    ! TODO: write a subroutine that prints "array" on screen
	integer :: i, nx
	real, allocatable :: array(:,:)
	
	nx = size(array, dim=1)
	
	write(*,*) "Array:"
	do i = 1, nx
		write(*, '(14F8.1)') array(i,:)
	end do
	
  end subroutine write_field

end module laplacian_mod
