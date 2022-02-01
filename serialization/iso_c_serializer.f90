! The Basic Model Interface ISO_C_BINDINGING compatible free functions
!
! @author: Nels Frazier
! @email: nels.frazier@noaa.gov
! Date: August 23, 2021
!
! This module provides a set of ISO_C_BINDING compatable functions
! that allow a Fortran BMI compatible model to interoperate with a C program, given that the
! BMI module implelements a `register` function that is able to return an appropriate opaque handle
! to the C caller.

module iso_c_serialization
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_char, c_null_char, c_int, c_double, c_float
  use serialization
  use iso_c_bmif_2_0, only: c_to_f_string,   f_to_c_string, box
  implicit none

  integer, parameter :: SER_MAX_FILE_NAME = 2048

  type serializer_adapter
    class(serializer), pointer :: ptr => null()
  end type

  contains

    ! 
    function serialize(this, model_in, ser_file) result(bmi_status) bind(C, name="serialize")
      type(c_ptr) :: this, model_in
      character(kind=c_char, len=1), dimension(SER_MAX_FILE_NAME), intent(in) :: ser_file
      !character(kind=c_char, len=1), dimension(2048), intent(in) :: ser_file
      integer(kind=c_int) :: bmi_status
      character(len=:), allocatable :: f_ser_file 
      !use a wrapper for c interop
      type(serializer_adapter), pointer :: ser_adptr
      type(box), pointer :: bmi_box 

      !extract the fortran type from handle
      call c_f_pointer(this, ser_adptr)
      !convert c style string to fortran character array
      f_ser_file = c_to_f_string(ser_file)

      call c_f_pointer(model_in, bmi_box)

      bmi_status = ser_adptr%ptr%serialize( bmi_box%ptr, f_ser_file)

      deallocate(f_ser_file)
    end function serialize

    function deserialize(this, model_out, ser_file) result(bmi_status) bind(C, name="deserialize")
      type(c_ptr) :: this, model_out
      character(kind=c_char, len=1), dimension(SER_MAX_FILE_NAME), intent(in) :: ser_file
      !character(kind=c_char, len=1), dimension(2048), intent(in) :: ser_file
      integer(kind=c_int) :: bmi_status
      character(len=:), allocatable :: f_ser_file 
      !use a wrapper for c interop
      type(serializer_adapter), pointer :: ser_adptr
      type(box), pointer :: bmi_box 

      !extract the fortran type from handle
      call c_f_pointer(this, ser_adptr)
      !convert c style string to fortran character array
      f_ser_file = c_to_f_string(ser_file)

      call c_f_pointer(model_out, bmi_box)

      bmi_status = ser_adptr%ptr%deserialize( bmi_box%ptr, f_ser_file)

      deallocate(f_ser_file)
    end function deserialize

    function compare(this, model1, model2) result(bmi_status) bind(C, name="compare")
      type(c_ptr) :: this, model1, model2
      integer(kind=c_int) :: bmi_status
      !use a wrapper for c interop
      type(serializer_adapter), pointer :: ser_adptr
      type(box), pointer :: bmi_box1 
      type(box), pointer :: bmi_box2 

      !extract the fortran type from handle
      call c_f_pointer(this, ser_adptr)

      call c_f_pointer(model1, bmi_box1)
      call c_f_pointer(model2, bmi_box2)

      bmi_status = ser_adptr%ptr%compare( bmi_box1%ptr, bmi_box2%ptr )

    end function compare

end module iso_c_serialization