! ----------------------------------------------
! state_serializer.f90
! ----------------------------------------------
! auther: Zhengtao Cui
! created on Jan. 5, 2022
! Last date of modification: Feb 10, 2022
! Reference: 
!
! Description: Implement the abstract 'serializer' class. This is the 
!              case that we use Fortan to do the serialization, minimizing 
!              the usage of iso C binding functions.
!              serialize the model states to a binary file using
!              the msgpack-c library in Fortran programing language
! 		 

module state_serialization
  use serialization
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_bool
  implicit none

  private
  public :: state_serializer

  ! 
  !wrapper for array, Fortran doesn't allow a array of arrays
  ! use these wrappers instead 
  !
  type :: realarray
     real, allocatable :: elements(:) 
  end type realarray

  type :: intarray
     integer, allocatable :: elements(:) 
  end type intarray

  type :: int1array
     integer(kind=1), allocatable :: elements(:) 
  end type int1array

  type :: int2array
     integer(kind=2), allocatable :: elements(:) 
  end type int2array

  type :: int8array
     integer(kind=8), allocatable :: elements(:) 
  end type int8array

  type :: real8array
     real*8, allocatable :: elements(:) 
  end type real8array

  type :: strarray
     character (len=:), allocatable :: elements
  end type strarray

  type :: logarray
     logical(kind=c_bool), allocatable :: elements(:)
  end type logarray

  !
  !sub-class of 'serializer' abstrace class
  !
  type, extends(serializer) :: state_serializer
     contains
       !serialization 
       procedure :: calling_c_test
       procedure :: serialize => serialize_states
       procedure :: deserialize => deserialize_states
       procedure :: compare => compare_states
  end type state_serializer
  
  interface

     !
     !interface for ISO C Binding to call the C c_serialize function
     !
     function f_c_serialize( names, length, types, typelength,  count, &
                     var_sizes, cptr2, ser_file) &
                    result( result ) bind(C, name="c_serialize")      
       use, intrinsic :: iso_c_binding
       type(c_ptr), dimension(:), intent(in) :: names
       type(c_ptr), dimension(:), intent(in) :: types
       type(c_ptr), value, intent(in) :: var_sizes
       integer(c_int), intent(in) :: length
       integer(c_int), intent(in) :: typelength
       integer(c_int), intent(in) :: count
       type(c_ptr), value, intent(in) :: cptr2
       !character(kind=c_char), dimension(:), intent(in) :: ser_file 
       type(c_ptr), value, intent(in) :: ser_file

    end function f_c_serialize

     !
     !interface for ISO C Binding to call the C c_deserialize function
     !
    function f_c_deserialize( names, length, types, typelength,  count, &
                    var_sizes, cptr2, ser_file) &
                    result( result ) bind(C, name="c_deserialize")      
       use, intrinsic :: iso_c_binding
       type(c_ptr), dimension(:), intent(in) :: names
       type(c_ptr), dimension(:), intent(in) :: types
       type(c_ptr), value, intent(in) :: var_sizes
       integer(c_int), intent(in) :: length
       integer(c_int), intent(in) :: typelength
       integer(c_int), intent(in) :: count
       type(c_ptr), value, intent(in) :: cptr2
       !character(kind=c_char), dimension(:), intent(in) :: ser_file 
       type(c_ptr), value, intent(in) :: ser_file

    end function f_c_deserialize
  end interface

contains

     !
     ! test code to call a C function, pasing string and integer 
     ! data types. No ISO C Binding is needed.
     !
     subroutine calling_c_test(this, testint, fstring1, fstring)
         use bmif_2_0
         class(state_serializer), intent(in) :: this
         character(len=*), intent(inout) :: fstring1
         character(len=*), intent(inout) :: fstring
         integer testint
         write(*,*) testint
         call c_wrapper_test(testint, fstring1, fstring)
     end subroutine calling_c_test

     !
     ! does the actual serialization work for a given model obj and a disk file
     !
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
            type(real8array), dimension(:), allocatable, target :: real8temp
            type(intarray), dimension(:), allocatable, target :: inttemp
            type(int1array), dimension(:), allocatable, target :: int1temp
            type(int2array), dimension(:), allocatable, target :: int2temp
            type(int8array), dimension(:), allocatable, target :: int8temp
            type(strarray), dimension(:), allocatable, target :: strtemp
            type(logarray), dimension(:), allocatable, target :: logtemp
            logical, dimension(:), allocatable :: log4temp

            !integer, dimension(:), pointer :: intptr
            integer, pointer :: intptr

            character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
            character(len=BMI_MAX_TYPE_NAME) :: type
            character(len=BMI_MAX_TYPE_NAME), pointer :: types(:)
            integer :: n = 1, varcount, realcount, intcount, real8count, &
                    strcount, realidx, intidx, real8idx, stridx, &
                    varlength,                                   &
                    int1count, int1idx, &
                    int2count, int2idx, &
                    int8count, int8idx, &
                    logcount, logidx
            integer, dimension(:), allocatable, target :: lengths

            type( c_ptr ), allocatable, dimension(:), target :: temp

            bmi_status = model_in%get_var_count( 'all', varcount)
            bmi_status = model_in%get_var_names( 'all', names)

            allocate( types(varcount) )
            allocate( lengths(varcount) )
            !
            ! count the number of variables for each data type
            !
            realcount = 0
            real8count = 0
            intcount = 0
            strcount = 0
            int1count =0
            int2count =0
            int8count =0
            logcount = 0
            do n = 1, varcount
               bmi_status = model_in%get_var_type( names(n), type )  
               types(n) = type
               select case( type )
               case( 'integer4' )
                  intcount = intcount + 1
               case( 'integer1' )
                  int1count = int1count + 1
               case( 'integer2' )
                  int2count = int2count + 1
               case( 'integer8' )
                  int8count = int8count + 1
               case( 'real4' )
                  realcount = realcount + 1
               case( 'real8' )
                  real8count = real8count + 1
               case( 'character' )
                  strcount = strcount + 1
               case( 'logical' )
                  logcount = logcount + 1
               case default
                       write(*,*) 'unknown type: ', type
               end select  
            end do

            !allocate spaces for each datatype
            if ( realcount .gt. 0 ) then
              allocate( realtemp(realcount) )
            end if

            if ( real8count .gt. 0 ) then
              allocate( real8temp(real8count) )
            end if

            if ( intcount .gt. 0 ) then
              allocate( inttemp(intcount) )
            end if

            if ( int1count .gt. 0 ) then
              allocate( int1temp(int1count) )
            end if

            if ( int2count .gt. 0 ) then
              allocate( int2temp(int2count) )
            end if

            if ( int8count .gt. 0 ) then
              allocate( int8temp(int8count) )
            end if

            if ( strcount .gt. 0 ) then
              allocate( strtemp(strcount) )
            end if

            if ( logcount .gt. 0 ) then
              allocate( logtemp(logcount) )
            end if

            allocate( temp(varcount) )

            realidx=0
            real8idx=0
            intidx=0
            int1idx=0
            int2idx=0
            int8idx=0
            stridx=0 
            logidx=0 
            !
            !get the values for each variable and put the pointers to these
            ! values in a pointer array, `temp`. 
            !
            do n = 1, varcount
               bmi_status = model_in%get_var_type( names(n), type )  
               bmi_status = model_in%get_var_length( names(n), varlength )  
               lengths(n) = varlength
               select case( type )
               case( 'integer4' )
                  intidx = intidx + 1
                  allocate( inttemp(intidx)%elements(varlength) )
                  bmi_status = model_in%get_value_int( names(n), &
                                                    inttemp(intidx)%elements)
!                  write(*,*) 'int'
!                  write(*,*) inttemp(intidx)%elements
!                  bmi_status = model_in%get_value_ptr_int( names(n), intptr)
!
!                  write(*,*) "intptr: ", intptr

                  temp(n) = c_loc(inttemp(intidx)%elements(1))
!                  temp(n) = c_loc(intptr)
               case( 'integer1' )
                  int1idx = int1idx + 1
                  allocate( int1temp(int1idx)%elements(varlength) )
                  bmi_status = model_in%get_value_int1( names(n), &
                                                    int1temp(int1idx)%elements)
                  temp(n) = c_loc(int1temp(int1idx)%elements(1))
               case( 'integer2' )
                  int2idx = int2idx + 1
                  allocate( int2temp(int2idx)%elements(varlength) )
                  bmi_status = model_in%get_value_int2( names(n), &
                                                    int2temp(int2idx)%elements)
                  temp(n) = c_loc(int2temp(int2idx)%elements(1))
               case( 'integer8' )
                  int8idx = int8idx + 1
                  allocate( int8temp(int8idx)%elements(varlength) )
                  bmi_status = model_in%get_value_int8( names(n), &
                                                    int8temp(int8idx)%elements)
                  temp(n) = c_loc(int8temp(int8idx)%elements(1))
               case( 'real4' )
                  realidx = realidx + 1
                  allocate( realtemp(realidx)%elements(varlength) )
                  bmi_status = model_in%get_value_float( names(n), &
                                        realtemp(realidx)%elements)
!                  write(*,*) 'float'
!                  write(*,*) realtemp(realidx)%elements
                  temp(n) = c_loc(realtemp(realidx)%elements(1))
               case( 'real8' )
                  real8idx = real8idx + 1
                  allocate( real8temp(real8idx)%elements(varlength) )
                  bmi_status = model_in%get_value_double( names(n), &
                                        real8temp(real8idx)%elements)
!                  write(*,*) 'double'
!                  write(*,*) real8temp(real8idx)%elements
                  temp(n) = c_loc(real8temp(real8idx)%elements(1))
               case( 'character' )
                  stridx = stridx + 1
                  allocate(  &
                       character(len=varlength) :: strtemp(stridx)%elements )
                  bmi_status = model_in%get_value_string( names(n), &
                                        strtemp(stridx)%elements)
!                  write(*,*) 'string'
!                  write(*,*) trim( strtemp(stridx)%elements)
                  temp(n) = c_loc(strtemp(stridx)%elements)
               case( 'logical' )
                  logidx = logidx + 1
                  allocate(  logtemp(logidx)%elements(varlength) )
                  allocate(  log4temp(varlength) )
                  bmi_status = model_in%get_value_logical( names(n), &
                                        log4temp )
!                  write(*,*) 'string'
!                  write(*,*) trim( strtemp(stridx)%elements)
                  logtemp(logidx)%elements = log4temp
                  temp(n) = c_loc(logtemp(logidx)%elements(1))
                  deallocate( log4temp )
               case default
                       write(*,*) 'unknown type: ', type
               end select  
            end do

            !pass filename string to c string
            do n = 1, len(ser_file)
             c_ser_file(n) = ser_file(n:n)
            end do
            c_ser_file(len(ser_file)+1) = c_null_char

            !
            !Now passing all variables to the C function to perform
            ! serialization.
            !
            bmi_status = f_c_serialize( c_loc( names ), BMI_MAX_VAR_NAME, &
                                        c_loc( types ), BMI_MAX_TYPE_NAME, &
                                varcount, c_loc(lengths), &
                                c_loc(temp), c_loc( c_ser_file ) )

            ! serialization is done now, cleanning up
            if ( realcount .gt. 0 ) then
              do n = 1, realcount
                deallocate( realtemp(n)%elements ) 
              end do
              deallocate( realtemp )
            endif 

            if ( real8count .gt. 0 ) then
              do n = 1, real8count
                deallocate( real8temp(n)%elements ) 
              end do
              deallocate( real8temp )
            endif 

            if ( intcount .gt. 0 ) then
              do n = 1, intcount
                deallocate( inttemp(n)%elements ) 
              end do
              deallocate( inttemp )
            endif 

            if ( int1count .gt. 0 ) then
              do n = 1, int1count
                deallocate( int1temp(n)%elements ) 
              end do
              deallocate( int1temp )
            endif 

            if ( int2count .gt. 0 ) then
              do n = 1, int2count
                deallocate( int2temp(n)%elements ) 
              end do
              deallocate( int2temp )
            endif 

            if ( int8count .gt. 0 ) then
              do n = 1, int8count
                deallocate( int8temp(n)%elements ) 
              end do
              deallocate( int8temp )
            endif 

            if ( strcount .gt. 0 ) then
              do n = 1, strcount
                deallocate( strtemp(n)%elements ) 
              end do
              deallocate( strtemp )
            endif 

            if ( logcount .gt. 0 ) then
              do n = 1, logcount
                deallocate( logtemp(n)%elements ) 
              end do
              deallocate( logtemp )
            endif 

            deallocate( temp ) 
            deallocate( types ) 
            deallocate( lengths ) 

            bmi_status = BMI_SUCCESS
     end function serialize_states

     !
     ! does the actual deserialization work for a given model obj and a 
     !  disk file
     !
     function deserialize_states(this, model_out, ser_file) result (bmi_status)
            use bmif_2_0
            use, intrinsic :: iso_c_binding
            class(state_serializer), intent(in) :: this
            class(bmi), intent(inout) :: model_out
            character(len=*), intent(in) :: ser_file
            character(kind=c_char), dimension(len(ser_file)+1), target :: c_ser_file
            integer :: bmi_status
            type(realarray), dimension(:), allocatable, target :: realtemp
            type(real8array), dimension(:), allocatable, target :: real8temp
            type(intarray), dimension(:), allocatable, target :: inttemp
            type(int1array), dimension(:), allocatable, target :: int1temp
            type(int2array), dimension(:), allocatable, target :: int2temp
            type(int8array), dimension(:), allocatable, target :: int8temp
            type(strarray), dimension(:), allocatable, target :: strtemp
            type(logarray), dimension(:), allocatable, target :: logtemp

            logical, dimension(:), allocatable :: log4temp

            character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
            character(len=BMI_MAX_TYPE_NAME) :: type
            character(len=BMI_MAX_TYPE_NAME), pointer :: types(:)
            integer :: n = 1, varcount, realcount, intcount, real8count, &
                    strcount, realidx, intidx, real8idx, stridx, &
                    varlength,                                   &
                    int1count, int1idx, &
                    int2count, int2idx, &
                    int8count, int8idx, &
                    logcount, logidx
            integer, dimension(:), allocatable, target :: lengths
            type( c_ptr ), allocatable, dimension(:), target :: temp

            bmi_status = model_out%get_var_count( 'all', varcount)
            bmi_status = model_out%get_var_names( 'all', names)

            allocate( types(varcount) )
            allocate( lengths(varcount) )
            !
            !count the number of variables for each datatype
            !
            realcount = 0
            real8count = 0
            intcount = 0
            int1count = 0
            int2count = 0
            int8count = 0
            strcount = 0
            logcount = 0
            do n = 1, varcount
               bmi_status = model_out%get_var_type( names(n), type )  
               types(n) = type
               select case( type )
               case( 'integer4' )
                  intcount = intcount + 1
               case( 'integer1' )
                  int1count = int1count + 1
               case( 'integer2' )
                  int2count = int2count + 1
               case( 'integer8' )
                  int8count = int8count + 1
               case( 'real4' )
                  realcount = realcount + 1
               case( 'real8' )
                  real8count = real8count + 1
               case( 'character' )
                  strcount = strcount + 1
               case( 'logical' )
                  logcount = logcount + 1
               case default
                       write(*,*) 'unknown type: ', type
               end select  
            end do

            !
            !allocate spaces for each datatype
            ! to hold pointers to each variable in each dataype
            if ( realcount .gt. 0 ) then
              allocate( realtemp(realcount) )
            end if

            if ( real8count .gt. 0 ) then
              allocate( real8temp(real8count) )
            end if

            if ( intcount .gt. 0 ) then
              allocate( inttemp(intcount) )
            end if

            if ( int1count .gt. 0 ) then
              allocate( int1temp(int1count) )
            end if

            if ( int2count .gt. 0 ) then
              allocate( int2temp(int2count) )
            end if

            if ( int8count .gt. 0 ) then
              allocate( int8temp(int8count) )
            end if

            if ( strcount .gt. 0 ) then
              allocate( strtemp(strcount) )
            end if

            if ( logcount .gt. 0 ) then
              allocate( logtemp(logcount) )
            end if

            allocate( temp(varcount) )

            realidx=0
            real8idx=0
            intidx=0
            int1idx=0
            int2idx=0
            int8idx=0
            stridx=0 
            logidx=0
            !
            !allocate space for each variable, and set the pointers to these
            ! variables into an array, `temp`.
            !
            do n = 1, varcount
               bmi_status = model_out%get_var_type( names(n), type )  
               bmi_status = model_out%get_var_length( names(n), varlength )  
               lengths(n)=varlength
               select case( type )
               case( 'integer4' )
                  intidx = intidx + 1
                  allocate( inttemp(intidx)%elements(varlength) )
                  temp(n) = c_loc(inttemp(intidx)%elements(1))
               case( 'integer1' )
                  int1idx = int1idx + 1
                  allocate( int1temp(int1idx)%elements(varlength) )
                  temp(n) = c_loc(int1temp(int1idx)%elements(1))
               case( 'integer2' )
                  int2idx = int2idx + 1
                  allocate( int2temp(int2idx)%elements(varlength) )
                  temp(n) = c_loc(int2temp(int2idx)%elements(1))
               case( 'integer8' )
                  int8idx = int8idx + 1
                  allocate( int8temp(int8idx)%elements(varlength) )
                  temp(n) = c_loc(int8temp(int8idx)%elements(1))
               case( 'real4' )
                  realidx = realidx + 1
                  allocate( realtemp(realidx)%elements(varlength) )
                  temp(n) = c_loc(realtemp(realidx)%elements(1))
               case( 'real8' )
                  real8idx = real8idx + 1
                  allocate( real8temp(real8idx)%elements(varlength) )
                  temp(n) = c_loc(real8temp(real8idx)%elements(1))
               case( 'character' )
                  stridx = stridx + 1
                  allocate(  &
                       character(len=varlength) :: strtemp(stridx)%elements )
                  temp(n) = c_loc(strtemp(stridx)%elements)
               case( 'logical' )
                  logidx = logidx + 1
                  allocate( logtemp(logidx)%elements(varlength) )
                  temp(n) = c_loc(logtemp(logidx)%elements(1))
               case default
                       write(*,*) 'unknown type: ', type
               end select  
            end do

            do n = 1, len(ser_file)
             c_ser_file(n) = ser_file(n:n)
            end do
            c_ser_file(len(ser_file)+1) = c_null_char
            !write(*,*) c_ser_file

            !
            !Now we call the C function to obtain values of each model state
            !variable passed by an array of pointers 
            !
            bmi_status = f_c_deserialize( c_loc( names ), BMI_MAX_VAR_NAME, &
                                        c_loc( types ), BMI_MAX_TYPE_NAME, &
                                varcount, c_loc(lengths), &
                                c_loc(temp), c_loc( c_ser_file ) )

            realidx=0
            intidx=0
            int1idx=0
            int2idx=0
            int8idx=0
            real8idx=0
            stridx=0
            logidx=0
            !
            ! papulate model states with values obtained from above
            ! C function 
            !
            do n = 1, varcount
               bmi_status = model_out%get_var_type( names(n), type )  
               select case( type )
               case( 'integer4' )
                  intidx = intidx + 1
                  bmi_status = model_out%set_value_int( names(n), &
                                         inttemp(intidx)%elements )  
               case( 'integer1' )
                  int1idx = int1idx + 1
                  bmi_status = model_out%set_value_int1( names(n), &
                                         int1temp(int1idx)%elements )  
               case( 'integer2' )
                  int2idx = int2idx + 1
                  bmi_status = model_out%set_value_int2( names(n), &
                                         int2temp(int2idx)%elements )  
               case( 'integer8' )
                  int8idx = int8idx + 1
                  bmi_status = model_out%set_value_int8( names(n), &
                                         int8temp(int8idx)%elements )  
               case( 'real4' )
                  realidx = realidx + 1
!                  write(*,*) 'deserialize:', realtemp(realidx)%elements(:lengths(n))
                  bmi_status = model_out%set_value_float( names(n), &
                                         realtemp(realidx)%elements )  
               case( 'real8' )
                  real8idx = real8idx + 1
                  bmi_status = model_out%set_value_double( names(n), &
                                        real8temp(real8idx)%elements )  
               case( 'character' )
                  stridx = stridx + 1
!                  write(*,*) "F deserizalize:", trim(strtemp(stridx)%elements)
                  bmi_status = model_out%set_value_string( names(n), &
                                        strtemp(stridx)%elements )  
               case( 'logical' )
                  logidx = logidx + 1
!                  write(*,*) 'F deserialize:', logtemp(logidx)%elements
                  allocate(log4temp(lengths(n)))
                  log4temp = logtemp(logidx)%elements(:lengths(n))
                  bmi_status = model_out%set_value_logical( names(n), &
                                        log4temp )  
!                  write(*,*) 'F deserialize: deallocate log4temp'
                  deallocate(log4temp)
               case default
                       write(*,*) 'unknown type: ', type
               end select  
            end do

            !cleaning up 
            if ( realcount .gt. 0 ) then
              do n = 1, realcount
                deallocate( realtemp(n)%elements ) 
              end do
              deallocate( realtemp )
            endif 

            if ( real8count .gt. 0 ) then
              do n = 1, real8count
                deallocate( real8temp(n)%elements ) 
              end do
              deallocate( real8temp )
            endif 

            if ( intcount .gt. 0 ) then
              do n = 1, intcount
                deallocate( inttemp(n)%elements ) 
              end do
              deallocate( inttemp )
            endif 

            if ( int1count .gt. 0 ) then
              do n = 1, int1count
                deallocate( int1temp(n)%elements ) 
              end do
              deallocate( int1temp )
            endif 

            if ( int2count .gt. 0 ) then
              do n = 1, int2count
                deallocate( int2temp(n)%elements ) 
              end do
              deallocate( int2temp )
            endif 

            if ( int8count .gt. 0 ) then
              do n = 1, int8count
                deallocate( int8temp(n)%elements ) 
              end do
              deallocate( int8temp )
            endif 

            if ( strcount .gt. 0 ) then
              do n = 1, strcount
                deallocate( strtemp(n)%elements ) 
              end do
              deallocate( strtemp )
            endif 

            if ( logcount .gt. 0 ) then
              do n = 1, logcount
                deallocate( logtemp(n)%elements ) 
              end do
              deallocate( logtemp )
            endif 

            deallocate( temp ) 
            deallocate( types ) 
            deallocate( lengths ) 
            bmi_status = BMI_SUCCESS
     end function deserialize_states

     !
     !compare each state variable for two given model objects.
     ! return BMI_FAILURE on the first non-matching variable. Return
     ! BMI_SUCCESS when all state variables are equal in the two model
     ! instances.
     !
     function compare_states(this, model1, model2) result (bmi_status)
            use bmif_2_0
            class(state_serializer), intent(in) :: this
            class(bmi), intent(in) :: model1
            class(bmi), intent(in) :: model2
            integer :: bmi_status
            real, dimension(:), allocatable :: realtemp1, realtemp2
            real(kind=8), dimension(:), allocatable :: doubletemp1, doubletemp2
            integer, dimension(:), allocatable :: inttemp1, inttemp2
            integer(kind=1), dimension(:), allocatable :: int1temp1, int1temp2
            integer(kind=2), dimension(:), allocatable :: int2temp1, int2temp2
            integer(kind=8), dimension(:), allocatable :: int8temp1, int8temp2
            character (len=:), allocatable :: strtemp1, strtemp2
            logical, dimension(:), allocatable :: logtemp1, logtemp2
            character(len=BMI_MAX_VAR_NAME), pointer :: names(:)
            character(len=BMI_MAX_TYPE_NAME) :: typename
            integer :: n = 1, varcount, length1, length2, i

            bmi_status = model1%get_var_count( 'all', varcount)
            bmi_status = model2%get_var_names( 'all', names)

            !
            !compare each state variable
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
               case( 'integer4' )
!                  write(*,*) 'integer'
                  allocate( inttemp1( length1 ) )
                  allocate( inttemp2( length2 ) )
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
                  deallocate( inttemp1 )
                  deallocate( inttemp2 )
               case( 'integer1' )
!                  write(*,*) 'integer1'
                  allocate( int1temp1( length1 ) )
                  allocate( int1temp2( length2 ) )
                  bmi_status = model1%get_value_int1( names(n), int1temp1 )  
                  bmi_status = model2%get_value_int1( names(n), int1temp2 )  

                  do i = 1, length1
                     if ( int1temp1(i) .ne. int1temp2(i) ) then
                        write(*, *) 'varaibale ', trim(names(n)), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
                  deallocate( int1temp1 )
                  deallocate( int1temp2 )
               case( 'integer2' )
!                  write(*,*) 'integer2'
                  allocate( int2temp1( length1 ) )
                  allocate( int2temp2( length2 ) )
                  bmi_status = model1%get_value_int2( names(n), int2temp1 )  
                  bmi_status = model2%get_value_int2( names(n), int2temp2 )  

                  do i = 1, length1
                     if ( int2temp1(i) .ne. int2temp2(i) ) then
                        write(*, *) 'varaibale ', trim(names(n)), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
                  deallocate( int2temp1 )
                  deallocate( int2temp2 )
               case( 'integer8' )
!                  write(*,*) 'integer8'
                  allocate( int8temp1( length1 ) )
                  allocate( int8temp2( length2 ) )
                  bmi_status = model1%get_value_int8( names(n), int8temp1 )  
                  bmi_status = model2%get_value_int8( names(n), int8temp2 )  

                  do i = 1, length1
                     if ( int8temp1(i) .ne. int8temp2(i) ) then
                        write(*, *) 'varaibale ', trim(names(n)), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
                  deallocate( int8temp1 )
                  deallocate( int8temp2 )
               case( 'real4' )
                  allocate( realtemp1( length1 ) )
                  allocate( realtemp2( length2 ) )
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
                  deallocate( realtemp1 )
                  deallocate( realtemp2 )
               case( 'real8' )
                  allocate( doubletemp1( length1 ) )
                  allocate( doubletemp2( length2 ) )
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
                  deallocate( doubletemp1 )
                  deallocate( doubletemp2 )

               case( 'character' )
                   allocate( character(len=length1) :: strtemp1 )
                   allocate( character(len=length2) :: strtemp2 )
                   bmi_status = model1%get_value_string( names(n), strtemp1 ) 
                   bmi_status = model2%get_value_string( names(n), strtemp2 )  

                   if ( strtemp1 .ne. strtemp2 ) then
                        write(*, *) 'varaibale ', trim(names(n)), &
                                                       ' is not equal!' 
                        write(*, *) 'strtemp1: ', strtemp1, 'strtemp2: ', strtemp2
                        bmi_status = BMI_FAILURE 
                        return
                   end if  

                   deallocate( strtemp1 )
                   deallocate( strtemp2 )

               case( 'logical' )
                  allocate( logtemp1( length1 ) )
                  allocate( logtemp2( length2 ) )
                  bmi_status = model1%get_value_logical( names(n), logtemp1 ) 
                  bmi_status = model2%get_value_logical( names(n), logtemp2 )  
!                  write(*,*) 'logtemp1', logtemp1
!                  write(*,*) 'logtemp2', logtemp2
                  do i = 1, length1
                     if ( logtemp1(i) .neqv. logtemp2(i) ) then
                        write(*, *) 'varaibale ', trim(names(n)), ' is not equal!' 
                        write(*, *) 'Mismatch: i = ', i, logtemp1(i), logtemp2(i)
                        bmi_status = BMI_FAILURE 
                        return
                     end if  
                  end do
                  deallocate( logtemp1 )
                  deallocate( logtemp2 )

               case default
                       write(*,*) 'unknown type'
                       bmi_status = BMI_FAILURE 
                       return
               end select  
            end do
            bmi_status = BMI_SUCCESS
     end function compare_states

     !
     !create an object of the serializer
     !
     function get_serializer_handle(this) result(bmi_status) &
                                     bind(C, name="get_serializer_handle")
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
  end function get_serializer_handle


  !
  !delete and cleanup space for the serializer object
  function delete_serializer_handle(this) result(bmi_status) bind(C, &
                                      name="delete_serializer_handle")
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
  end function delete_serializer_handle

  !
  ! create the `serializer_adapter` object from the given pointer of 
  ! a serializer object
  !
  function get_serializer_box(this, serializer_ptr) result(bmi_status) bind(C, name="get_serializer_box")
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
   !associate the adapter pointer with the created serializer instance
   adapter%ptr => f_serializer_ptr

   if( .not. associated( adapter ) .or. .not. associated( adapter%ptr ) ) then
    bmi_status = BMI_FAILURE
   else
    !Return the pointer to box
    this = c_loc(adapter)
    bmi_status = BMI_SUCCESS
   endif
 end function get_serializer_box 

 !
 !delete the adapter object
 !this should to be used only after the serializer object it points to has been
 !deleted.
 !
  function delete_serializer_box(this) result(bmi_status) bind(C, &
          name="delete_serializer_box")
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
 end function delete_serializer_box

  function register_serializer(this) result(bmi_status) bind(C, &
          name="register_serializer")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use bmif_2_0
   use iso_c_serialization
   implicit none
   type(c_ptr), intent(out) :: this ! If not value, then from the C perspective `this` is a void**
   type(c_ptr) :: serializer_ptr
   integer(kind=c_int) :: bmi_status

   bmi_status =  get_serializer_handle(serializer_ptr)

   if ( bmi_status .eq. BMI_FAILURE ) then
           return
   end if

   bmi_status = get_serializer_box(this, serializer_ptr)

  end function register_serializer

  function unregister_serializer(this) result(bmi_status) bind(C, &
          name="unregister_serializer")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use bmif_2_0
   use iso_c_serialization
   implicit none
   type(c_ptr), intent(inout) :: this ! If not value, then from the C perspective `this` is a void**
   type(serializer_adapter), pointer :: f_this
   integer(kind=c_int) :: bmi_status

   call c_f_pointer( this, f_this)

   if( .not. associated( f_this%ptr ) ) then
      bmi_status = BMI_FAILURE
   else
      deallocate( f_this%ptr )
      bmi_status = BMI_SUCCESS
   endif

   if ( bmi_status .eq. BMI_FAILURE ) then
           return
   end if

   bmi_status = delete_serializer_box(this)

 end function unregister_serializer

end module state_serialization
