module integer32_set
  use, intrinsic :: iso_c_binding
  implicit none


  type :: int32_set
    integer(c_int32_t), dimension(:), pointer :: data => null()
    integer(c_int32_t) :: size_internal = 0
  contains
    procedure :: push => int32_set_push
  end type int32_set


contains


  function new_int32_set() result(a)
    implicit none

    type(int32_set) :: a

    allocate(a%data(0))
  end function new_int32_set


  subroutine int32_set_push(this, new_value)
    implicit none

    class(int32_set), intent(inout) :: this
    integer(c_int32_t), intent(in), value :: new_value
    integer(c_int32_t) :: i
    logical(c_bool) :: found
    integer(c_int32_t), dimension(:), pointer :: new_data

    found = .false.

    ! See if it's in there
    do i = 1,this%size_internal
      if (this%data(i) == new_value) then
        found = .true.
        exit
      end if
    end do

    ! If not, add it in to a new pointer.
    if (.not. found) then
      allocate(new_data(this%size_internal + 1))
      do i = 1,this%size_internal
        new_data(i) = this%data(i)
      end do
      new_data(this%size_internal + 1) = new_value

      deallocate(this%data)
      this%data => new_data
      this%size_internal = this%size_internal + 1
    end if
  end subroutine int32_set_push


end module integer32_set
