! The `serializer` Interface ISO_C_BINDINGING compatible free functions
!
! @author: Zhengtao Cui
! @email: Zhengtao.Cui@noaa.gov
! Date: Feb 18, 2022
!
! This module provides a set of ISO_C_BINDING compatable functions
! that allow a Fortran serializer to interoperate with a C program, 
! given that the drivered subclass of `serializer` implelements a `register` 
! function that is able to return an appropriate opaque handle
! to the C caller.
!

module iso_c_serialization
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_char, c_null_char, c_int, c_double, c_float
  use serialization
  use iso_c_bmif_2_0, only: c_to_f_string,   f_to_c_string, box
  implicit none

  integer, parameter :: SER_MAX_FILE_NAME = 2048

  !
  !This `serializer_adapter` wrapper is necessary becasue the `serializer` is
  ! an abstract class and can not be instantiated, the concrete subclass of
  ! this abstract class is not known at this stage.
  !
  type serializer_adapter
    class(serializer), pointer :: ptr => null()
  end type

  contains

    ! the serialize function iso C binding
    function serialize(this, model_in, ser_file) result(bmi_status) bind(C, name="serialize")
      type(c_ptr), value :: this, model_in
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

    ! the deserialize function iso C binding
    function deserialize(this, model_out, ser_file) result(bmi_status) bind(C, name="deserialize")
      type(c_ptr), value :: this, model_out
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

    ! the model compare function iso C binding
    function compare(this, model1, model2) result(bmi_status) bind(C, name="compare")
      type(c_ptr), value :: this, model1, model2
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
