program hello
  use omp_lib
  implicit none
  integer :: id, nt

  print *, 'Hello world!'
  !$omp parallel private(id) shared(nt)
  	id = omp_get_thread_num()
	
	!$omp single
		nt = omp_get_num_threads()
	!$omp end single
	
	!$omp critical
		print *, '..from ', id
  	!$omp end critical

  !$omp end parallel
  print *, 'Total number of threads: ', nt    

end program hello
