program test
  use, intrinsic :: iso_c_binding
  use :: integer32_set
  implicit none

  type(int32_set) :: my_set
  integer(c_int32_t) :: i

  ! Ensure that you create the memory for the set using this built-in method.
  my_set = new_int32_set()

  ! Let us add some data, backwards.
  do i = 10,1,-1
    call my_set%push(i)
  end do

  print*,"size:",my_set%size

  print*,my_set%data

  ! We can get it to go from min->max using the sort method.
  call my_set%sort()

  print*,"size:",my_set%size

  print*,my_set%data

  ! Suppose we want to remove the even numbers.
  do i = 0,10, 2
    call my_set%pop(i)
  end do

  print*,"size:",my_set%size

  ! Tada.
  print*,my_set%data

  !! Don't forget to destroy the memory. :)
  call my_set%destroy()

  ! Which will give you a size of -1.
  print*,my_set%size


end program test
