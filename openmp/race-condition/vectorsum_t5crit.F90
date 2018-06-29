program vectorsum
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex, t_sum
  integer(kind=ik) :: i

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  sumex = nx*(nx+1_ik)/2_ik
  write(*,*) 'Arithmetic sum formula (exact):                  ', sumex

  sum = 0
  ! TODO: Parallelize the computation
  !$omp parallel private(i, t_sum)
  
  	!$omp do 
		do i = 1, nx
     			t_sum = t_sum + vecA(i)
		end do
  	!$omp end do
	
	!$omp critical(do_sum)
		sum = sum + t_sum
	!$omp end critical(do_sum)

  !$omp end parallel

  write(*,*) 'Sum: ', sum
end program vectorsum
