program test
  use, intrinsic :: iso_c_binding
  use :: integer32_set
  implicit none

  type(int32_set) :: data
  integer(c_int32_t) :: i, j


  data = new_int32_set()

  do i = 1,10
    j = 1
    call data%push(j)
  end do

  print*,data%size()


end program test
