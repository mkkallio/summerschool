program exer1
  implicit none
  integer :: var1, var2
  var1 = 1
  var2 = 2

  ! TODO:
  !   Test different data sharing clauses here
  !$omp parallel
  print *, 'normal'
  print *, 'Region:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  !$omp end parallel 
  print *, 'After region: var1=', var1, 'var2=', var2
  print *
  
  !$omp parallel private(var1, var2)
  print *, 'private'
  print *, 'Region:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  !$omp end parallel
  print *, 'After region: var1=', var1, 'var2=', var2
  print *
  
  !$omp parallel firstprivate(var1, var2)
  print *, 'firstprivate'
  print *, 'Region:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  !$omp end parallel
  print *, 'After region: var1=', var1, 'var2=', var2
  print *

end program exer1
