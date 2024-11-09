module integer_set
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, integer_set!"
  end subroutine say_hello
end module integer_set
