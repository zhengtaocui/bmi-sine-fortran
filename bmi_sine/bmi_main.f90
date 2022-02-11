! Run the sine model through its BMI.
program bmi_main

  use bmisinef
  use, intrinsic :: iso_fortran_env, only : file_unit=>input_unit
  implicit none

  character (len=*), parameter :: output_file = "bmisinef.out"
  character (len=*), parameter :: var_name = "sine2d"
  integer, parameter :: ndims = 0

  type (bmi_sine) :: model
  integer :: arg_count = 0
  character (len=80) :: arg
  integer :: i, j, s, grid_id, grid_size, grid_shape(ndims)
  double precision :: current_time, end_time
  real, allocatable :: sine(:)

  do while (arg_count <= 1)
    call get_command_argument(arg_count, arg)
    arg_count = arg_count + 1
  end do

  if (len_trim(arg) == 0) then
     write(*,"(a)") "Usage: run_bmisinef CONFIGURATION_FILE"
     write(*,"(a)")
     write(*,"(a)") "Run the sinef model through its BMI with a configuration file."
     write(*,"(a)") "Output is written to the file `bmisinef.out`."
     stop
  end if

  open(file_unit,file=output_file)

  write(file_unit,"(a)") "Initialize model."
  s = model%initialize(arg)

  s = model%get_current_time(current_time)
  s = model%get_end_time(end_time)
  s = model%get_var_length( var_name, grid_size )
!  s = model%get_var_grid(var_name, grid_id)
!  s = model%get_grid_size(grid_id, grid_size)
!  s = model%get_grid_shape(grid_id, grid_shape)

  allocate(sine(grid_size))

  do while (current_time <= end_time )
     write(file_unit,"(a, f6.3)") "Model values at time = ", current_time
     s = model%get_value(var_name, sine)
     write (file_unit,"(f15.4)", advance="no") sine
     write (file_unit,*)
     s = model%update()
     s = model%get_current_time(current_time)
  end do

  call model%print_model_info( )

  deallocate(sine)
  s = model%finalize()
  write(file_unit,"(a)") "Finalize model."

  close(file_unit)

end program bmi_main
