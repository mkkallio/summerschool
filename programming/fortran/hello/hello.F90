program hello
	implicit none
  
	integer :: x = 2,y = 3,z
	integer :: a,b,c
  
	z = x*y

	write (*,*) 'Enter two integers for multiplication'
	read(*,*) a,b

  
	c = a*b
  
	write (*,*) 'Hello world from Fortran!'
	write (*,*) 'x is ',x, ' and y is ',y
	write (*,*) 'x multiplied by y is ',z
	write (*,*)
  
	write(*,*) 'a is ',a, ' and b is ',b
	write(*,*) 'a multiplied by b is ',c
end program hello
