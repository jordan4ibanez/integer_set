module integer32_set
   use, intrinsic :: iso_c_binding
   implicit none


   type :: int32_set
      integer(c_int32_t), dimension(:), pointer :: data => null()
      integer(c_int32_t) :: size = 0
   contains

   end type int32_set


contains


   function new_int32_set() result(a)
      implicit none

      type(int32_set) :: a

      allocate(a%data(0))
   end function new_int32_set


   subroutine int32_set_push(this, data)
      implicit none

      class(int32_set), intent(inout) :: this
      integer(c_int32_t), intent(in), value :: data
      integer(c_int32_t) :: i
      logical(c_bool) :: found

      found = .false.

      do i = 1,this%size

      end do
   end subroutine int32_set_push


end module integer32_set
