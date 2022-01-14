module state_serialization
  use serialization
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  private
  public :: state_serializer

  type :: realarray
     real, allocatable :: elements(:) 
  end type realarray

  type :: doublearray
     real*8, allocatable :: elements(:) 
  end type doublearray

  type :: intarray
     integer, allocatable :: elements(:) 
  end type intarray

  type, extends(serializer) :: state_serializer
     contains
       !serialization 
       procedure :: calling_c_test
       procedure :: serialize => serialize_states
       procedure :: deserialize => deserialize_states
       procedure :: compare => compare_states
  end type state_serializer
  
  interface
     function f_c_serialize( names, length, types, typelength,  count, cptr2, ser_file) &
                    result( result ) bind(C, name="c_serialize")      
       use, intrinsic :: iso_c_binding
       type(c_ptr), dimension(:), intent(in) :: names
       type(c_ptr), dimension(:), intent(in) :: types
       integer(c_int), intent(in) :: length
       integer(c_int), intent(in) :: typelength
       integer(c_int), intent(in) :: count
       type(c_ptr), value, intent(in) :: cptr2
       !character(kind=c_char), dimension(:), intent(in) :: ser_file 
       type(c_ptr), value, intent(in) :: ser_file

    end function f_c_serialize

    function f_c_deserialize( names, length, types, typelength,  count, cptr2, ser_file) &
                    result( result ) bind(C, name="c_deserialize")      
       use, intrinsic :: iso_c_binding
       type(c_ptr), dimension(:), intent(in) :: names
       type(c_ptr), dimension(:), intent(in) :: types
       integer(c_int), intent(in) :: length
       integer(c_int), intent(in) :: typelength
       integer(c_int), intent(in) :: count
       type(c_ptr), value, intent(in) :: cptr2
       !character(kind=c_char), dimension(:), intent(in) :: ser_file 
       type(c_ptr), value, intent(in) :: ser_file

    end function f_c_deserialize
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
            use serialization
            use, intrinsic :: iso_c_binding
            class(state_serializer), intent(in) :: this
            class(bmi), intent(in) :: model_in
            character(len=*), intent(in) :: ser_file
            character(kind=c_char), dimension(len(ser_file)+1), target :: c_ser_file
            integer :: bmi_status
            type(realarray), dimension(:), allocatable, target :: realtemp
            type(intarray), dimension(:), allocatable, target :: inttemp
            type(doublearray), dimension(:), allocatable, target :: doubletemp
            character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
            character(len=BMI_MAX_TYPE_NAME) :: type
            character(len=BMI_MAX_TYPE_NAME), pointer :: types(:)
            integer :: n = 1, varcount, realcount, intcount, doublecount, &
                    realidx, intidx, doubleidx
            type( c_ptr ), allocatable, dimension(:), target :: temp

            bmi_status = model_in%get_var_count( 'all', varcount)
            bmi_status = model_in%get_var_names( 'all', names)

            allocate( types(varcount) )
            realcount = 0
            intcount = 0
            doublecount = 0
            do n = 1, varcount
               bmi_status = model_in%get_var_type( names(n), type )  
               types(n) = type
               select case( type )
               case( 'integer' )
                  intcount = intcount + 1
               case( 'real' )
                  realcount = realcount + 1
               case( 'double' )
                  doublecount = doublecount + 1
               case default
                       write(*,*) 'unknown type'
               end select  
            end do

            if ( realcount .gt. 0 ) then
              allocate( realtemp(realcount) )
            end if

            if ( intcount .gt. 0 ) then
              allocate( inttemp(intcount) )
            end if

            if ( doublecount .gt. 0 ) then
              allocate( doubletemp(doublecount) )
            end if

            allocate( temp(varcount) )

            realidx=0
            intidx=0
            doubleidx=0
             
            do n = 1, varcount
               bmi_status = model_in%get_var_type( names(n), type )  
               select case( type )
               case( 'integer' )
                  intidx = intidx + 1
                  allocate( inttemp(intidx)%elements(1) )
                  bmi_status = model_in%get_value_int( names(n), &
                                                    inttemp(intidx)%elements)
                  temp(n) = c_loc(inttemp(intidx)%elements(1))
               case( 'real' )
                  realidx = realidx + 1
                  allocate( realtemp(realidx)%elements(1) )
                  bmi_status = model_in%get_value_float( names(n), &
                                        realtemp(realidx)%elements)
                  temp(n) = c_loc(realtemp(realidx)%elements(1))
               case( 'double' )
                  doubleidx = doubleidx + 1
                  allocate( doubletemp(doubleidx)%elements(1) )
                  bmi_status = model_in%get_value_double( names(n), &
                          doubletemp(doubleidx)%elements)
                  temp(n) = c_loc(doubletemp(doubleidx)%elements(1))
               case default
                       write(*,*) 'unknown type'
               end select  
            end do

            !pass filename string to c string
            do n = 1, len(ser_file)
             c_ser_file(n) = ser_file(n:n)
            end do
            c_ser_file(len(ser_file)+1) = c_null_char

            bmi_status = f_c_serialize( c_loc( names ), BMI_MAX_VAR_NAME, &
                                        c_loc( types ), BMI_MAX_TYPE_NAME, &
                                varcount, c_loc(temp), c_loc( c_ser_file ) )

            if ( realcount .gt. 0 ) then
              do n = 1, realcount
                deallocate( realtemp(n)%elements ) 
              end do
              deallocate( realtemp )
            endif 

            if ( intcount .gt. 0 ) then
              do n = 1, intcount
                deallocate( inttemp(n)%elements ) 
              end do
              deallocate( inttemp )
            endif 

            if ( doublecount .gt. 0 ) then
              do n = 1, doublecount
                 deallocate( doubletemp(n)%elements ) 
              end do
              deallocate( doubletemp )
            endif 

            deallocate( temp ) 
            deallocate( types ) 

            bmi_status = BMI_SUCCESS
     end function serialize_states

     function deserialize_states(this, model_out, ser_file) result (bmi_status)
            use bmif_2_0
            use, intrinsic :: iso_c_binding
            class(state_serializer), intent(in) :: this
            class(bmi), intent(inout) :: model_out
            character(len=*), intent(in) :: ser_file
            character(kind=c_char), dimension(len(ser_file)+1), target :: c_ser_file
            integer :: bmi_status
            type(realarray), dimension(:), allocatable, target :: realtemp
            type(intarray), dimension(:), allocatable, target :: inttemp
            type(doublearray), dimension(:), allocatable, target :: doubletemp
            character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
            character(len=BMI_MAX_TYPE_NAME) :: type
            character(len=BMI_MAX_TYPE_NAME), pointer :: types(:)
            integer :: n = 1, varcount, realcount, intcount, doublecount, &
                    realidx, intidx, doubleidx
            type( c_ptr ), allocatable, dimension(:), target :: temp

            bmi_status = model_out%get_var_count( 'all', varcount)
            bmi_status = model_out%get_var_names( 'all', names)

            allocate( types(varcount) )
            !count types
            realcount = 0
            intcount = 0
            doublecount = 0
            do n = 1, varcount
               bmi_status = model_out%get_var_type( names(n), type )  
               types(n) = type
               select case( type )
               case( 'integer' )
                  intcount = intcount + 1
               case( 'real' )
                  realcount = realcount + 1
               case( 'double' )
                  doublecount = doublecount + 1
               case default
                       write(*,*) 'unknown type'
               end select  
            end do

            if ( realcount .gt. 0 ) then
              allocate( realtemp(realcount) )
            end if

            if ( intcount .gt. 0 ) then
              allocate( inttemp(intcount) )
            end if

            if ( doublecount .gt. 0 ) then
              allocate( doubletemp(doublecount) )
            end if

            allocate( temp(varcount) )

            realidx=0
            intidx=0
            doubleidx=0
             
            do n = 1, varcount
               bmi_status = model_out%get_var_type( names(n), type )  
               select case( type )
               case( 'integer' )
                  intidx = intidx + 1
                  allocate( inttemp(intidx)%elements(1) )
                  temp(n) = c_loc(inttemp(intidx)%elements(1))
               case( 'real' )
                  realidx = realidx + 1
                  allocate( realtemp(realidx)%elements(1) )
                  temp(n) = c_loc(realtemp(realidx)%elements(1))
               case( 'double' )
                  doubleidx = doubleidx + 1
                  allocate( doubletemp(doubleidx)%elements(1) )
                  temp(n) = c_loc(doubletemp(doubleidx)%elements(1))
               case default
                       write(*,*) 'unknown type'
               end select  
            end do

            do n = 1, len(ser_file)
             c_ser_file(n) = ser_file(n:n)
            end do
            c_ser_file(len(ser_file)+1) = c_null_char
            !write(*,*) c_ser_file

            bmi_status = f_c_deserialize( c_loc( names ), BMI_MAX_VAR_NAME, &
                                        c_loc( types ), BMI_MAX_TYPE_NAME, &
                                varcount, c_loc(temp), c_loc( c_ser_file ) )

            realidx=0
            intidx=0
            doubleidx=0
            do n = 1, varcount
               bmi_status = model_out%get_var_type( names(n), type )  
               select case( type )
               case( 'integer' )
                  bmi_status = model_out%set_value_int( names(n), &
                                         inttemp(intidx)%elements )  
               case( 'real' )
                  realidx = realidx + 1
                  bmi_status = model_out%set_value_float( names(n), &
                                         realtemp(realidx)%elements )  
               case( 'double' )
                  doubleidx = doubleidx + 1
                  bmi_status = model_out%set_value_double( names(n), &
                                        doubletemp(doubleidx)%elements )  
               case default
                       write(*,*) 'unknown type'
               end select  
            end do

            if ( realcount .gt. 0 ) then
              do n = 1, realcount
                deallocate( realtemp(n)%elements ) 
              end do
              deallocate( realtemp )
            endif 

            if ( intcount .gt. 0 ) then
              do n = 1, intcount
                deallocate( inttemp(n)%elements ) 
              end do
              deallocate( inttemp )
            endif 

            if ( doublecount .gt. 0 ) then
              do n = 1, doublecount
                 deallocate( doubletemp(n)%elements ) 
              end do
              deallocate( doubletemp )
            endif 

            deallocate( temp ) 
            deallocate( types ) 
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
                  write(*, *) 'varaibale ', trim(names(n)), 'length is not equal!' 
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
                        write(*, *) 'varaibale ', trim(names(n)), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
               case( 'real' )
                  bmi_status = model1%get_value_float( names(n), realtemp1 )  
                  bmi_status = model2%get_value_float( names(n), realtemp2 )  
                  do i = 1, length1
                     if ( realtemp1(i) .ne. realtemp2(i) ) then
                        write(*, *) 'varaibale ', trim(names(n)), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
               case( 'double' )
                  bmi_status = model1%get_value_double( names(n), doubletemp1 )  
                  bmi_status = model2%get_value_double( names(n), doubletemp2 )  

                  do i = 1, length1
                     if ( doubletemp1(i) .ne. doubletemp2(i) ) then
                        write(*, *) 'varaibale ', trim(names(n)), ' is not equal!' 
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

     function serializer_factory(this) result(bmi_status) &
                                                 bind(C, name="serializer_factory")
        use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
        use bmif_2_0
        implicit none
        type(c_ptr), intent(out) :: this ! If not value, then from the C perspective `this` is a void**
        type(state_serializer), pointer :: bmi_serializer 
        integer(kind=c_int) :: bmi_status
        allocate(state_serializer::bmi_serializer)
        if( .not. associated( bmi_serializer ) ) then
          bmi_status = BMI_FAILURE
        else
          this = c_loc(bmi_serializer)
          bmi_status = BMI_SUCCESS
        endif
  end function serializer_factory


  function serializer_destroy(this) result(bmi_status) bind(C, name="serializer_destroy")
    use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
    use bmif_2_0
    implicit none
    type(c_ptr), intent(in) :: this ! If not value, then from the C perspective `this` is a void**
    type(state_serializer), pointer :: bmi_serializer
    integer(kind=c_int) :: bmi_status

    call c_f_pointer(this, bmi_serializer)

    if( .not. associated( bmi_serializer ) ) then
      bmi_status = BMI_FAILURE
    else
      deallocate( bmi_serializer )
      bmi_status = BMI_SUCCESS
    endif
  end function serializer_destroy

  function create_adapter(this, serializer_ptr) result(bmi_status) bind(C, name="c_create_adapter")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use bmif_2_0
   use iso_c_serialization
   implicit none
   type(c_ptr), intent(out) :: this ! If not value, then from the C perspective `this` is a void**
   type(c_ptr), intent(in) :: serializer_ptr ! If not value, then from the C perspective `this` is a void**
   integer(kind=c_int) :: bmi_status
   !Create the model instance to use
   type(state_serializer), pointer :: f_serializer_ptr
   !Create a simple pointer wrapper
   type(serializer_adapter), pointer :: adapter

   !allocate the pointer box
   allocate(adapter)

   call c_f_pointer( serializer_ptr, f_serializer_ptr)
   !associate the wrapper pointer the created model instance
   adapter%ptr => f_serializer_ptr

   if( .not. associated( adapter ) .or. .not. associated( adapter%ptr ) ) then
    bmi_status = BMI_FAILURE
   else
    !Return the pointer to box
    this = c_loc(adapter)
    bmi_status = BMI_SUCCESS
   endif
 end function create_adapter

  function delete_adapter(this) result(bmi_status) bind(C, &
          name="c_delete_adapter")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use bmif_2_0
   use iso_c_serialization
   implicit none
   type(c_ptr), intent(in) :: this ! If not value, then from the C perspective `this` is a void**
   type(serializer_adapter), pointer :: f_this
   integer(kind=c_int) :: bmi_status
   !Create a simple pointer wrapper

   call c_f_pointer( this, f_this)

   if( .not. associated( f_this ) ) then
    bmi_status = BMI_FAILURE
   else
    deallocate( f_this ) 
    bmi_status = BMI_SUCCESS
   endif
 end function delete_adapter

end module state_serialization
