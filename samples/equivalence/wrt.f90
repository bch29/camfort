program wrt 
  implicit none
  integer :: x = 97
  character :: y
  character :: z(20)
  equivalence (x, y, z(2))

  z(1) = "c"

  write (*,'(i8,A)') x, y


  write (*,'(i8,A,A,A)') x, y, z(1), z(2)

  x = 97 +    98 * (2**8)

  write (*,'(i8,A,A,A,A)') x, y, z(1), z(2), z(3)


end program wrt
