module vector_algebra
  use iso_fortran_env, only : REAL64
  implicit none
  type vector_t
     real(REAL64) :: x, y, z
  end type vector_t

  ! TODO: overload operators needed by the parser
	interface operator(+)
		module procedure vector_sum
	end interface
  ! ...

contains
  ! TODO: implement the corresponding functions

  function vector_sum(v1, v2) result(v3)
    !    ...
	type(vector_t), intent(in) :: v1, v2 
	type(vector_t) :: v3
	
	v3%x = v1%x + v2%x
	v3%y = v1%y + v2%y
	v3%z = v1%z + v2%z
	
  end function vector_sum

  ! ...

end module vector_algebra
