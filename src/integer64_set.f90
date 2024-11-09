module integer64_set
  use, intrinsic :: iso_c_binding
  use :: constants_f90
  implicit none


  private


  public :: int64_set
  public :: new_int64_set


  type :: int64_set
    integer(c_int64_t), dimension(:), pointer :: data => null()
    integer(c_int64_t) :: size = 0
  contains
    procedure :: push => int64_set_push
    procedure :: clear => int64_set_clear
    procedure :: pop => int64_set_pop
    procedure :: sort => int64_set_sort
    procedure :: destroy => int64_set_destroy
  end type int64_set


contains


  function new_int64_set() result(a)
    implicit none

    type(int64_set) :: a

    allocate(a%data(0))
    a%size = 0
  end function new_int64_set


  subroutine int64_set_push(this, new_value)
    implicit none

    class(int64_set), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: new_value
    integer(c_int64_t) :: i
    logical(c_bool) :: found
    integer(c_int64_t), dimension(:), pointer :: new_data

    found = .false.

    ! See if it's in there.
    do i = 1,this%size
      if (this%data(i) == new_value) then
        found = .true.
        exit
      end if
    end do

    ! If not return.
    if (found) then
      return
    end if

    ! Add it in to a new pointer.
    allocate(new_data(this%size + 1))
    do i = 1,this%size
      new_data(i) = this%data(i)
    end do
    new_data(this%size + 1) = new_value

    ! Deallocate the old pointer, swap in the new pointer.
    deallocate(this%data)
    this%data => new_data

    this%size = this%size + 1
  end subroutine int64_set_push


  subroutine int64_set_clear(this)
    implicit none

    class(int64_set), intent(inout) :: this

    ! This is pretty simple, just clearing out all data and reset the size.
    this%size = 0

    deallocate(this%data)
    allocate(this%data(0))
  end subroutine int64_set_clear


  subroutine int64_set_pop(this, value_to_pop)
    implicit none

    class(int64_set), intent(inout) :: this
    integer(c_int64_t), intent(in), value :: value_to_pop
    logical(c_bool) :: found
    integer(c_int64_t) :: i, j
    integer(c_int64_t), dimension(:), pointer :: new_data

    found = .false.

    ! See if we have this value.
    do i = 1,this%size
      if (this%data(i) == value_to_pop) then
        found = .true.
      end if
    end do

    ! If we don't, nothing to do.
    if (.not. found) then
      return
    end if

    allocate(new_data(this%size - 1))

    ! Now, we want to skip the popped value, so we're going to make this an asynchronous index.
    j = 1
    do i = 1,this%size
      if (this%data(i) == value_to_pop) then
        cycle
      end if

      new_data(j) = this%data(i)
      j = j + 1
    end do

    ! Now we deallocate and swap in the new pointer.
    deallocate(this%data)
    this%data => new_data

    this%size = this%size - 1
  end subroutine int64_set_pop


  subroutine int64_set_sort(this)
    implicit none

    class(int64_set), intent(inout) :: this
    integer(c_int64_t) :: current, current_index
    integer(c_int64_t) :: i
    integer(c_int64_t), dimension(:), pointer :: new_data

    allocate(new_data(this%size))

    do i = 1,this%size
      ! Find the minimum value using the built-in searchers.
      current = minval(this%data)
      current_index = minloc(this%data, (1))

      ! Swap it to the literal max so we don't find it again.
      ! If we do find C_INT_MAX again, that means it was already in the set.
      this%data(current_index) = C_INT_MAX

      new_data(i) = current
    end do

    ! Now deallocate old and swap the pointers.
    deallocate(this%data)
    this%data => new_data
  end subroutine int64_set_sort


  subroutine int64_set_destroy(this)
    implicit none

    class(int64_set), intent(inout) :: this

    deallocate(this%data)
    this%size = -1
  end subroutine int64_set_destroy


end module integer64_set
