module state_serialization
  use serialization
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  private
  public :: state_serializer

  type, extends(serializer) :: state_serializer
     contains
       !serialization 
       procedure :: calling_c_test
       procedure :: serialize => serialize_states
       procedure :: deserialize => deserialize_states
       procedure :: compare => compare_states
  end type state_serializer
  
  interface
    function f_c_serialize( names, length, count, cptr, cptr1, cptr2) result( result ) &
                    bind(C, name="c_serialize")      
       use, intrinsic :: iso_c_binding
       type(c_ptr), dimension(:), intent(in) :: names
       integer(c_int), intent(in) :: length
       integer(c_int), intent(in) :: count
       !real(c_float), dimension(:), intent(in) :: cptr
       type(c_ptr), value, intent(in) :: cptr
       type(c_ptr), value, intent(in) :: cptr1
       type(c_ptr), value, intent(in) :: cptr2

    end function f_c_serialize
  end interface

contains

     subroutine calling_c_test(this, testint, fstring1, fstring)
         use bmif_2_0
         class(state_serializer), intent(in) :: this
         character(len=*), intent(inout) :: fstring1
         character(len=*), intent(inout) :: fstring
         integer testint
         write(*,*) testint
         call c_wrapper_test(testint, fstring1, fstring)
     end subroutine calling_c_test

     function serialize_states(this, model_in, ser_file) result (bmi_status)
            use bmif_2_0
            use, intrinsic :: iso_c_binding
            class(state_serializer), intent(in) :: this
            class(bmi), intent(in) :: model_in
            character(len=*), intent(in) :: ser_file
            integer :: bmi_status
            real, dimension(1), target :: realtemp
            real(c_float), dimension(3), target :: realtemp2
            integer, dimension(1), target :: inttemp
            real*8, dimension(1), target :: doubletemp
            real*8, dimension(2), target :: doubletemp2
            character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
            character(len=BMI_MAX_TYPE_NAME) :: type
            integer :: n = 1, varcount
            type( c_ptr ), allocatable, dimension(:), target :: temp

            open( unit=51, file=ser_file, status='replace', form='unformatted', &
            access='sequential')

            write(*, "(a)") "Inside serialize_state!"
            write(*, "(a)") ser_file

            bmi_status = model_in%get_var_count( 'all', varcount)
            write(*,*) 'varcount = ', varcount
            bmi_status = model_in%get_var_names( 'all', names)
            write(*,*) 'names = ', names

            allocate( temp(varcount) )
            realtemp2(1) = 100.0
            realtemp2(2) = 200.0
            realtemp2(3) = 300.0
            doubletemp2(1) = 400.0
            doubletemp2(2) = 500.0
            inttemp(1) = 101 
            temp(1) = c_loc(realtemp2)
            temp(2) = c_loc(doubletemp2)
            temp(3) = c_loc(names)
            bmi_status = f_c_serialize( c_loc( names ), BMI_MAX_VAR_NAME, &
                                varcount, c_loc(realtemp2), c_loc(inttemp(1)), &
                                c_loc(temp) )

            do n = 1, varcount
               bmi_status = model_in%get_var_type( names(n), type )  
               select case( type )
               case( 'integer' )
                  write(*,*) 'integer'
                  bmi_status = model_in%get_value_int( names(n), inttemp)
                  temp(n) = c_loc(inttemp(1))
                  write(*,*) trim(names(n)), ' = ', inttemp
                  write(51) inttemp
               case( 'real' )
                  write(*,*) 'real'
                  bmi_status = model_in%get_value_float( names(n), realtemp)
                  temp(n) = c_loc(realtemp(1))
                  write(*,*) trim(names(n)), ' = ', realtemp
                  write(51) realtemp
               case( 'double' )
                  write(*,*) 'double'
                  bmi_status = model_in%get_value_double( names(n), doubletemp)
                  temp(n) = c_loc(doubletemp(1))
                  write(*,*) trim(names(n)), ' = ', doubletemp
                  write(51) doubletemp
               case default
                       write(*,*) 'unknown type'
               end select  
            end do

            close(51)

            deallocate( temp ) 
            bmi_status = BMI_SUCCESS
     end function serialize_states

     function deserialize_states(this, model_out, ser_file) result (bmi_status)
            use bmif_2_0
            class(state_serializer), intent(in) :: this
            class(bmi), intent(inout) :: model_out
            character(len=*), intent(in) :: ser_file
            integer :: bmi_status
            real, dimension(1) :: realtemp
            integer, dimension(1) :: inttemp
            real*8, dimension(1) :: doubletemp
            character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
            character(len=BMI_MAX_TYPE_NAME) :: typename
            integer :: n = 1, varcount

            write(*, "(a)") "Inside deserialize_state!"
            write(*, "(a)") ser_file
            open( unit=52, file=ser_file, status='old', form='unformatted', &
                    access='sequential' )

            bmi_status = model_out%get_var_count( 'all', varcount)
            write(*,*) 'varcount = ', varcount
            bmi_status = model_out%get_var_names( 'all', names)
            write(*,*) 'names = ', names(1)

            do n = 1, varcount
               bmi_status = model_out%get_var_type( names(n), typename )  
               select case( typename )
               case( 'integer' )
                  write(*,*) 'integer'
                  read(52) inttemp
                  write(*,*) names(n), ' = ', inttemp
                  bmi_status = model_out%set_value_int( names(n), inttemp )  
               case( 'real' )
                  write(*,*) 'real'
                  read(52) realtemp
                  write(*,*) names(n), ' = ', realtemp
                  bmi_status = model_out%set_value_float( names(n), realtemp )  
               case( 'double' )
                  write(*,*) 'double'
                  read(52) doubletemp
                  write(*,*) names(n), ' = ', doubletemp
                  bmi_status = model_out%set_value_double( names(n), doubletemp )  
               case default
                       write(*,*) 'unknown type'
               end select  

            end do

            close(52)
            bmi_status = BMI_SUCCESS
     end function deserialize_states

     function compare_states(this, model1, model2) result (bmi_status)
            use bmif_2_0
            class(state_serializer), intent(in) :: this
            class(bmi), intent(in) :: model1
            class(bmi), intent(in) :: model2
            integer :: bmi_status
            real, dimension(1) :: realtemp1, realtemp2
            integer, dimension(1) :: inttemp1, inttemp2
            real*8, dimension(1) :: doubletemp1, doubletemp2
            character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
            character(len=BMI_MAX_TYPE_NAME) :: typename
            integer :: n = 1, varcount, length1, length2, i

            bmi_status = model1%get_var_count( 'all', varcount)
            bmi_status = model2%get_var_names( 'all', names)

            do n = 1, varcount
               bmi_status = model1%get_var_type( names(n), typename )  
               bmi_status = model1%get_var_length( names(n), length1 )  
               bmi_status = model2%get_var_length( names(n), length2 )  
               if ( length1 .ne. length2 ) then
                  write(*, *) 'varaibale ', names(n), 'length is not equal!' 
                  write(*, *) 'length1 = ', length1, 'length2 = ', length2 
                  bmi_status = BMI_FAILURE
                  exit
               end if
               select case( typename )
               case( 'integer' )
                  write(*,*) 'integer'
                  bmi_status = model1%get_value_int( names(n), inttemp1 )  
                  bmi_status = model2%get_value_int( names(n), inttemp2 )  

                  do i = 1, length1
                     if ( inttemp1(i) .ne. inttemp2(i) ) then
                        write(*, *) 'varaibale ', names(n), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
               case( 'real' )
                  write(*,*) 'real'
                  bmi_status = model1%get_value_float( names(n), realtemp1 )  
                  bmi_status = model2%get_value_float( names(n), realtemp2 )  
                  write(*,*) 'realtemp1 = ', realtemp1
                  write(*,*) 'realtemp2 = ', realtemp2
                  do i = 1, length1
                     if ( realtemp1(i) .ne. realtemp2(i) ) then
                        write(*, *) 'varaibale ', names(n), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
               case( 'double' )
                  write(*,*) 'double'
                  bmi_status = model1%get_value_double( names(n), doubletemp1 )  
                  bmi_status = model2%get_value_double( names(n), doubletemp2 )  

                  do i = 1, length1
                     if ( doubletemp1(i) .ne. doubletemp2(i) ) then
                        write(*, *) 'varaibale ', names(n), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
               case default
                       write(*,*) 'unknown type'
                       bmi_status = BMI_FAILURE 
                       return
               end select  
            end do
            bmi_status = BMI_SUCCESS
     end function compare_states

end module state_serialization
