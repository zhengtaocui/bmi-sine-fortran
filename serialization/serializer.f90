module serialization

  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  use bmif_2_0

  implicit none
  integer, parameter :: SER_MAX_FILE_NAME = 2048

  private
  public :: serializer
  type, abstract :: serializer
     
     contains
       !serialization 
!       procedure(bmif_calling_c_test), deferred :: calling_c_test
       procedure(bmif_serialize), deferred :: serialize
       procedure(bmif_deserialize), deferred :: deserialize
       procedure(bmif_compare), deferred :: compare
  end type serializer

  abstract interface

!  subroutine bmif_calling_c_test(this)
!      use bmif_2_0
!      import :: serializer
!      class(serializer), intent(in) :: this
!  end subroutine bmif_calling_c_test

  function bmif_serialize(this, model_in, ser_file) result(bmi_status)
     use bmif_2_0
     import :: serializer
     class(serializer), intent(in) :: this
     class(bmi), intent(in) :: model_in
     character(len=*), intent(in) :: ser_file
     integer :: bmi_status
   end function bmif_serialize

  function bmif_deserialize(this, model_out, ser_file) result(bmi_status)
     use bmif_2_0
     import :: serializer
     class(serializer), intent(in) :: this
     class(bmi), intent(inout) :: model_out
     character(len=*), intent(in) :: ser_file
     integer :: bmi_status
  end function bmif_deserialize

  function bmif_compare(this, model1, model2) result(bmi_status)
     use bmif_2_0
     import :: serializer
     class(serializer), intent(in) :: this
     class(bmi), intent(in) :: model1
     class(bmi), intent(in) :: model2
     integer :: bmi_status
   end function bmif_compare
  end interface
end module serialization
