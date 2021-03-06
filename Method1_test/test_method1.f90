!----------------------------------------------
! test_serialize_c.c
! ----------------------------------------------
! auther: Zhengtao Cui
! created on Feb. 2, 2022
! Last date of modification: Feb 18, 2022
! Reference: https://github.com/NOAA-OWP/cfe.git
!            test_serialize/serialize_state.c
!
! Description: test program for the the Fortran version of the 
!             serialization/deserialization code in ../serialization,
!             using Fortran programing language

program test_serialize

  use bmif_2_0, only: BMI_FAILURE, BMI_SUCCESS, BMI_MAX_COMPONENT_NAME
  use bmisinef
  use state_serialization
  use fixtures, only: status, config_file

  implicit none

  type (bmi_sine) :: m1, m2
  type (state_serializer) :: s

  type try
     integer :: i
     real :: x, y
  end type try

  type(try) :: mytry = try( 101, 10.3, 11.4 )

  character (len=80) :: serialize_file
  character (len=80) :: serialize_file1
  character (len=BMI_MAX_COMPONENT_NAME), parameter :: &
       expected = "The 2D Heat Equation"
  character (len=BMI_MAX_COMPONENT_NAME), pointer :: name
  integer :: testint = 101, n = 0
  double precision :: start_time, current_time, end_time
  double precision :: pause_time = 50

  serialize_file = 'serialize.out'
  serialize_file1 = 'serialize1.out'

  status = m1%get_component_name(name)

  write(*,*) 'Component name: ', trim(name)

  status = m1%initialize(config_file)
  status = m2%initialize(config_file)

  !call s%calling_c_test(testint, serialize_file1, serialize_file)

  status = m1%get_start_time( start_time ) 
  status = m1%get_end_time( end_time ) 
  current_time = start_time
  do while (current_time <= pause_time)
     status = m1%update()
     status = m1%get_current_time(current_time)
  end do

  write(*,*) 'Before serialize: comparing two models ...'
  status = s%compare(m1, m2) 
  if ( status .ne. BMI_SUCCESS ) then
     write(*,*) 'model1 and model2 are not equal!' 
  else
     write(*,*) 'model1 and model2 are equal!' 
  end if

  status = s%serialize( m1, serialize_file )

  status = s%deserialize( m2, serialize_file )

  write(*,*) 'Pause and then update two models ...'
  do while (current_time <= end_time)
     status = m1%update()
     status = m1%get_current_time(current_time)
  end do
  current_time = pause_time

  do while (current_time <= end_time)
     status = m2%update()
     status = m2%get_current_time(current_time)
  end do

  status = s%compare(m1, m2) 
 
  write(*,*) 'After serialize/descrialize: comparing two models ...'
  if ( status .ne. BMI_SUCCESS ) then
     write(*,*) 'model1 and model2 are not equal!' 
  else
     write(*,*) 'model1 and model2 are equal!' 
  end if
end program test_serialize
