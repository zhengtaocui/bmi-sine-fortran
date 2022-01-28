! An example of the sine equation.
module sinef

  implicit none
  integer, parameter :: MAX_STRING_LENGTH = 2048 

  ! Define the attributes of the model.
  type :: sine_model
     integer :: id

     real :: dt
     real :: t
     real :: t_end

     real :: alpha

     integer :: n_x
     integer :: n_y

     real :: sinevalue
     real*8, dimension(:), allocatable :: sinevalue_tmp(:)

     real, dimension(:,:), allocatable :: sine2d
     real*8, pointer :: sine2d_ptr(:,:)

     character(len=MAX_STRING_LENGTH) :: description

     logical, dimension(:), allocatable :: logvar(:)

  end type sine_model

  private :: initialize

contains

  ! Initializes the model with values read from a file.
  subroutine initialize_from_file(model, config_file)
    character (len=*), intent (in) :: config_file
    type (sine_model), intent (out) :: model

    open(15, file=config_file)
    read(15, *) model%id, model%alpha, model%t, model%t_end, model%dt
    read(15, *) model%n_x, model%n_y
    close(15)
    call initialize(model)
  end subroutine initialize_from_file

  ! Initializes the model with default hardcoded values.
  subroutine initialize_from_defaults(model)
    type (sine_model), intent (out) :: model

    model%id    = 0
    model%alpha = 0
    model%t     = 0.
    model%t_end = 20.
    model%dt     = 1.
    model%n_x     = 5
    model%n_y     = 4
    call initialize(model)
  end subroutine initialize_from_defaults

  ! Allocates memory and sets values for either initialization technique.
  subroutine initialize(model)
    type (sine_model), intent (inout) :: model

    model%sinevalue = 0.

    allocate( model%sinevalue_tmp( model%n_x ) ) 
    allocate( model%sine2d( model%n_y, model%n_x ) ) 
    allocate( model%sine2d_ptr( model%n_y, model%n_x ) ) 
    allocate( model%logvar( model%n_y ) ) 

    model%description = 'Model initialized!'

    model%sinevalue_tmp = 0.
    model%sine2d = 0.
    model%sine2d_ptr = 0.

    model%logvar = .TRUE.

  end subroutine initialize

  ! Frees memory when program completes.
  subroutine cleanup(model)
    type (sine_model), intent (inout) :: model
    if ( associated( model%sine2d_ptr ) ) then
        deallocate(model%sine2d_ptr)
    end if

    if ( allocated( model%sine2d ) ) then
        deallocate(model%sine2d)
    end if

    if ( allocated( model%sinevalue_tmp ) ) then
        deallocate(model%sinevalue_tmp)
    end if

    if ( allocated( model%logvar ) ) then
        deallocate(model%logvar)
    end if

    return
  end subroutine cleanup

  ! Steps the sine model forward in time.
  subroutine advance_in_time(model)
    type (sine_model), intent (inout) :: model
    integer i, j
    model%sinevalue = sin( model%t )
    model%t = model%t + model%dt
 
    model%description = 'Model advanced one step!'

    do i = 1, model%n_x
       ! set values to 110.xxx, 120.xxx, 130.xxx, ..., 
       model%sinevalue_tmp(i) = model%sinevalue + i * 10. + 100.
       do j = 1, model%n_y 
          ! set values to 
          !        110.xxx, 120.xxx, 130.xxx, ..., 
          !        210.xxx, 220.xxx, 230.xxx, ...,
          !        310.xxx, 320.xxx, 330.xxx, ...,
          !        ...
          model%sine2d(j, i) = model%sinevalue + i * 10. + j*100.
          model%sine2d_ptr(j, i) = model%sinevalue + i * 100. + j*1000.
       end do
    end do
    do j = 1, model%n_y 
      if ( modulo(j,2) .eq. 0 ) then 
         model%logvar(j) = .FALSE.
      else
         model%logvar(j) = .TRUE.
      end if
    end do

  end subroutine advance_in_time

  ! A helper routine for displaying model parameters.
  subroutine print_info(model)
    type (sine_model), intent (in) :: model
    character(len=30) :: rowfmt
    integer i, j

    write(*,"(1x, a13, a50)") "description: ", model%description
    write(*,"(a10, f8.2)") "sinevalue", model%sinevalue
    write(*,"(a10, f8.2)") "alpha:", model%alpha
    write(*,"(a10, f8.2)") "dt:", model%dt
    write(*,"(a10, f8.2)") "t:", model%t
    write(*,"(a10, f8.2)") "t_end:", model%t_end
    write(*,"(a15)") "sinevalue_tmp:"
    write(rowfmt,'(a,I2,a)') '(1x,',model%n_x, 'f12.4))'
    write(*,rowfmt) (model%sinevalue_tmp(i), i = 1, model%n_x)
    write(*,"(a8)") "sine2d:"
    do j = 1, model%n_y
       write(*,rowfmt) (model%sine2d(j,i), i = 1, model%n_x)
    end do

    write(*,"(a12)") "sine2d_ptr:"
    do j = 1, model%n_y
       write(*,rowfmt) (model%sine2d_ptr(j,i), i = 1, model%n_x)
    end do

    write(*,"(a7)") "logvar:"
    write(*,*) (model%logvar(i), i = 1, model%n_y)

  end subroutine print_info

  ! A helper routine that prints the current state of the model.
  subroutine print_values(model)
    type (sine_model), intent (in) :: model
    integer :: i, j
    character(len=30) :: rowfmt

!    write(rowfmt,'(a)') '(1x,f6.1))'
!    write(*,fmt=rowfmt) model%sinevalue

    call print_info( model )
  end subroutine print_values

end module sinef
