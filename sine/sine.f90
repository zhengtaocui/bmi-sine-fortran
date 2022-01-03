! An example of the sine equation.
module sinef

  implicit none

  ! Define the attributes of the model.
  type :: sine_model
     integer :: id

     real :: dt
     real :: t
     real :: t_end

     real :: alpha

     real :: sinevalue
     real :: sinevalue_tmp
  end type sine_model

  private :: initialize

contains

  ! Initializes the model with values read from a file.
  subroutine initialize_from_file(model, config_file)
    character (len=*), intent (in) :: config_file
    type (sine_model), intent (out) :: model

    open(15, file=config_file)
    read(15, *) model%alpha, model%t_end
    close(15)
    call initialize(model)
  end subroutine initialize_from_file

  ! Initializes the model with default hardcoded values.
  subroutine initialize_from_defaults(model)
    type (sine_model), intent (out) :: model

    model%alpha = 0
    model%t_end = 20.
    call initialize(model)
  end subroutine initialize_from_defaults

  ! Allocates memory and sets values for either initialization technique.
  subroutine initialize(model)
    type (sine_model), intent (inout) :: model

    model%id = 0
    model%t = 0.
    model%dt = 1. 

    model%sinevalue = 0.
    model%sinevalue_tmp = 0.
  end subroutine initialize

  ! Frees memory when program completes.
  subroutine cleanup(model)
    type (sine_model), intent (inout) :: model
    return
  end subroutine cleanup

  ! Steps the sine model forward in time.
  subroutine advance_in_time(model)
    type (sine_model), intent (inout) :: model
    model%sinevalue_tmp = sin( model%t )
    model%sinevalue = model%sinevalue_tmp
    model%t = model%t + model%dt
  end subroutine advance_in_time

  ! A helper routine for displaying model parameters.
  subroutine print_info(model)
    type (sine_model), intent (in) :: model

    write(*,"(a10, f8.2)") "sinevalue", model%sinevalue
    write(*,"(a10, f8.2)") "sinevalue_tmp:", model%sinevalue_tmp
    write(*,"(a10, f8.2)") "alpha:", model%alpha
    write(*,"(a10, f8.2)") "dt:", model%dt
    write(*,"(a10, f8.2)") "t:", model%t
    write(*,"(a10, f8.2)") "t_end:", model%t_end
  end subroutine print_info

  ! A helper routine that prints the current state of the model.
  subroutine print_values(model)
    type (sine_model), intent (in) :: model
    integer :: i, j
    character(len=30) :: rowfmt

    write(rowfmt,'(a)') '(1x,f6.1))'
    write(*,fmt=rowfmt) model%sinevalue
  end subroutine print_values

end module sinef
