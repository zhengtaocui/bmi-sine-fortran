! ----------------------------------------------
! bmi_sine.f90
! ----------------------------------------------
! auther: Zhengtao Cui
! created on Jan. 5, 2022
! Last date of modification: Feb 18, 2022
! Reference: 
!
! Description: Implemented the dummy 'sine' Fortran model using the
!              Fortran BMI interfaces.
! 		 
module bmisinef

  use sinef
  use bmif_2_0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  integer, parameter :: STATE_VAR_NAME_COUNT = 15

  type :: variable
     integer :: index
     character(len=BMI_MAX_VAR_NAME) :: name
     character(len=BMI_MAX_TYPE_NAME) :: type
     integer :: size ! (nbytes / itemize)
     character(len=BMI_MAX_ROLE_NAME) :: role
     character(len=BMI_MAX_UNITS_NAME) :: units
     character(len=BMI_MAX_LOCATION_NAME) :: location ! e.g. "node", "face"
     integer :: grid ! e.g. 0
  end type variable

  type, extends (bmi) :: bmi_sine
     private
     type (sine_model) :: model
   contains
     procedure :: get_component_name => sine_component_name
     procedure :: get_input_item_count => sine_input_item_count
     procedure :: get_output_item_count => sine_output_item_count
     procedure :: get_input_var_names => sine_input_var_names
     procedure :: get_output_var_names => sine_output_var_names
     procedure :: initialize => sine_initialize
     procedure :: finalize => sine_finalize
     procedure :: get_start_time => sine_start_time
     procedure :: get_end_time => sine_end_time
     procedure :: get_current_time => sine_current_time
     procedure :: get_time_step => sine_time_step
     procedure :: get_time_units => sine_time_units
     procedure :: update => sine_update
     procedure :: update_until => sine_update_until
     procedure :: get_var_grid => sine_var_grid
     procedure :: get_grid_type => sine_grid_type
     procedure :: get_grid_rank => sine_grid_rank
     procedure :: get_grid_shape => sine_grid_shape
     procedure :: get_grid_size => sine_grid_size
     procedure :: get_grid_spacing => sine_grid_spacing
     procedure :: get_grid_origin => sine_grid_origin
     procedure :: get_grid_x => sine_grid_x
     procedure :: get_grid_y => sine_grid_y
     procedure :: get_grid_z => sine_grid_z
     procedure :: get_grid_node_count => sine_grid_node_count
     procedure :: get_grid_edge_count => sine_grid_edge_count
     procedure :: get_grid_face_count => sine_grid_face_count
     procedure :: get_grid_edge_nodes => sine_grid_edge_nodes
     procedure :: get_grid_face_edges => sine_grid_face_edges
     procedure :: get_grid_face_nodes => sine_grid_face_nodes
     procedure :: get_grid_nodes_per_face => sine_grid_nodes_per_face
     procedure :: get_var_type => sine_var_type
     procedure :: get_var_units => sine_var_units
     procedure :: get_var_itemsize => sine_var_itemsize
     procedure :: get_var_nbytes => sine_var_nbytes
     procedure :: get_var_location => sine_var_location
     procedure :: get_value_int => sine_get_int
     procedure :: get_value_int1 => sine_get_int1
     procedure :: get_value_int2 => sine_get_int2
     procedure :: get_value_int8 => sine_get_int8
     procedure :: get_value_float => sine_get_float
     procedure :: get_value_double => sine_get_double
     procedure :: get_value_string => sine_get_string
     procedure :: get_value_logical => sine_get_logical
     generic :: get_value => &
          get_value_int, &
          get_value_int1, &
          get_value_int2, &
          get_value_int8, &
          get_value_float, &
          get_value_double, &
          get_value_string, &
          get_value_logical
     procedure :: get_value_ptr_int => sine_get_ptr_int
     procedure :: get_value_ptr_int1 => sine_get_ptr_int1
     procedure :: get_value_ptr_int2 => sine_get_ptr_int2
     procedure :: get_value_ptr_int8 => sine_get_ptr_int8
     procedure :: get_value_ptr_logical => sine_get_ptr_logical
     procedure :: get_value_ptr_float => sine_get_ptr_float
     procedure :: get_value_ptr_double => sine_get_ptr_double
     procedure :: get_value_ptr_string => sine_get_ptr_string
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_int1, &
          get_value_ptr_int2, &
          get_value_ptr_int8, &
          get_value_ptr_float, &
          get_value_ptr_double, &
          get_value_ptr_string, &
          get_value_ptr_logical
     procedure :: get_value_at_indices_int => sine_get_at_indices_int
     procedure :: get_value_at_indices_float => sine_get_at_indices_float
     procedure :: get_value_at_indices_double => sine_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => sine_set_int
     procedure :: set_value_int1 => sine_set_int1
     procedure :: set_value_int2 => sine_set_int2
     procedure :: set_value_int8 => sine_set_int8
     procedure :: set_value_float => sine_set_float
     procedure :: set_value_double => sine_set_double
     procedure :: set_value_string => sine_set_string
     procedure :: set_value_logical => sine_set_logical
     generic :: set_value => &
          set_value_int, &
          set_value_int1, &
          set_value_int2, &
          set_value_int8, &
          set_value_float, &
          set_value_double, &
          set_value_string, &
          set_value_logical
     procedure :: set_value_at_indices_int => sine_set_at_indices_int
     procedure :: set_value_at_indices_float => sine_set_at_indices_float
     procedure :: set_value_at_indices_double => sine_set_at_indices_double
     generic :: set_value_at_indices => &
          set_value_at_indices_int, &
          set_value_at_indices_float, &
          set_value_at_indices_double
     !
     !new functions for serialization
     !
     procedure :: get_bmi_version => sine_get_bmi_version
     procedure :: get_var_count => sine_get_var_count
     procedure :: get_var_names => sine_get_var_names
     procedure :: get_var_index => sine_get_var_index
     procedure :: get_var_role => sine_get_var_role
     procedure :: get_var_length => sine_get_var_length
     !------------------------------------------------------
     procedure :: print_model_info
  end type bmi_sine

  private
  public :: bmi_sine

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "The Sine Equation"

  ! Exchange items
  integer, parameter :: input_item_count = 7
  integer, parameter :: output_item_count = 8
  integer, parameter :: all_item_count = STATE_VAR_NAME_COUNT
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: output_items
  character (len=BMI_MAX_VAR_NAME), target, &
          dimension(all_item_count) :: all_items

  type(variable), dimension(STATE_VAR_NAME_COUNT) :: &
          var_info = (/variable(1, 't', 'real', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 0 ),               &
                      variable(2, 'alpha', 'real', 1, &
                                'not_set', 'm^2/s', &
                                'node', 0 ),                &
                      variable(3, 'dt', 'real', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 0 ),               &
                      variable(4, 't_end', 'real', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 0 ),               &
                      variable(5, 'n_x', 'integer', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 0 ),               &
                      variable(6, 'n_y', 'integer', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 0 ),               &
                      variable(7, 'id', 'integer', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 0 ),               &
                      variable(8, 'sinevalue', 'real', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 0 ),               &
                      variable(9, 'sinevalue_tmp', 'real*8', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 1 ),               &
                      variable(10, 'sine2d', 'real', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 2 ),               &
                      variable(11, 'sine2d_ptr', 'real*8', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 2 ),               &
                      variable(12, 'description', 'character', &
                                  MAX_STRING_LENGTH, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 1 ),               &
                      variable(13, 'logvar', 'logical', &
                                  1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 1 ),               &
                      variable(14, 'int2d', 'integer', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 2 ),               &
                      variable(15, 'double2d', 'real*8', 1, &
                                'not_set', 'DIMENSIONLESS', &
                                'node', 2 ) /)

      interface get_type_and_kind
           module procedure get_type_and_kind_scalar
           module procedure get_type_and_kind_array
           module procedure get_type_and_kind_2darray
      end interface get_type_and_kind
  
!
!this doesn't work, got error 
!Error: ‘sine_get_ptr_int’ and ‘sine_get_ptr_double’ cannot be mixed
!FUNCTION/SUBROUTINE for GENERIC ‘get_value_ptr’ at (1)

!      interface sine_get_ptr_int
!           module procedure sine_get_ptr_int_scalar
!           module procedure sine_get_ptr_int_array
!           module procedure sine_get_ptr_int_2darray
!      end interface sine_get_ptr_int
contains

  function get_type_and_kind_scalar( var ) result ( typeandkind )
      class(*), intent(in) :: var
      character(len=BMI_MAX_TYPE_NAME) :: typeandkind
      
      select type (var )
        type is ( integer(kind=1) )  
           typeandkind='integer1'
        type is ( integer(kind=2) )  
           typeandkind='integer2'
        type is ( integer(kind=4) )  
           typeandkind='integer4'
        type is ( integer(kind=8) )  
           typeandkind='integer8'
        type is ( real(kind=4) )  
           typeandkind='real4'
        type is ( real(kind=8) )  
           typeandkind='real8'
     !https://stackoverflow.com/questions/27392400/fortran-type-is-character
        type is ( character(*) )  
           typeandkind='character'
!        type is ( character(kind=4) )  
!           typeandkind='character4'
        type is ( logical )  
           typeandkind='logical'
        class default
           typeandkind='unknown'

        end select
  end function get_type_and_kind_scalar

  function get_type_and_kind_array( var ) result ( typeandkind )
      class(*), dimension(:), intent(in) :: var
      character(len=BMI_MAX_TYPE_NAME) :: typeandkind
      
      select type (var )
        type is ( integer(kind=1) )  
           typeandkind='integer1'
        type is ( integer(kind=2) )  
           typeandkind='integer2'
        type is ( integer(kind=4) )  
           typeandkind='integer4'
        type is ( integer(kind=8) )  
           typeandkind='integer8'
        type is ( real(kind=4) )  
           typeandkind='real4'
        type is ( real(kind=8) )  
           typeandkind='real8'
     !https://stackoverflow.com/questions/27392400/fortran-type-is-character
        type is ( character(*) )  
           typeandkind='character'
!        type is ( character(kind=4) )  
!           typeandkind='character4'
        type is ( logical )  
           typeandkind='logical'
        class default
           typeandkind='unknown'
        end select
  end function get_type_and_kind_array

  function get_type_and_kind_2darray( var ) result ( typeandkind )
      class(*), dimension(:,:), intent(in) :: var
      character(len=BMI_MAX_TYPE_NAME) :: typeandkind
      
      select type (var )
        type is ( integer(kind=1) )  
           typeandkind='integer1'
        type is ( integer(kind=2) )  
           typeandkind='integer2'
        type is ( integer(kind=4) )  
           typeandkind='integer4'
        type is ( integer(kind=8) )  
           typeandkind='integer8'
        type is ( real(kind=4) )  
           typeandkind='real4'
        type is ( real(kind=8) )  
           typeandkind='real8'
     !https://stackoverflow.com/questions/27392400/fortran-type-is-character
        type is ( character(*) )  
           typeandkind='character'
!        type is ( character(kind=4) )  
!           typeandkind='character4'
        type is ( logical )  
           typeandkind='logical'
        class default
           typeandkind='unknown'
        end select
  end function get_type_and_kind_2darray

  ! Get the name of the model.
  function sine_component_name(this, name) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function sine_component_name

  ! Count the input variables.
  function sine_input_item_count(this, count) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = input_item_count
    bmi_status = BMI_SUCCESS
  end function sine_input_item_count

  ! Count the output variables.
  function sine_output_item_count(this, count) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = output_item_count
    bmi_status = BMI_SUCCESS
  end function sine_output_item_count

  ! List input variables.
  function sine_input_var_names(this, names) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    input_items(1) = 'id'
    input_items(2) = 'alpha'
    input_items(3) = 't'
    input_items(4) = 't_end'
    input_items(5) = 'dt'
    input_items(6) = 'n_x'
    input_items(7) = 'n_y'
    names => input_items
    bmi_status = BMI_SUCCESS
  end function sine_input_var_names

  ! List output variables.
  function sine_output_var_names(this, names) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    output_items(1) = 'sinevalue'
    output_items(2) = 'sinevalue_tmp'
    output_items(3) = 'sine2d'
    output_items(4) = 'sine2d_ptr'
    output_items(5) = 'description'
    output_items(6) = 'logvar'
    output_items(7) = 'int2d'
    output_items(8) = 'double2d'
    names => output_items
    bmi_status = BMI_SUCCESS
  end function sine_output_var_names

  ! BMI initializer.
  function sine_initialize(this, config_file) result (bmi_status)
    class (bmi_sine), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
       call initialize_from_file(this%model, config_file)
    else
       call initialize_from_defaults(this%model)
    end if

    !sinevalue_tmp
    var_info(9)%size =  this%model%n_x
    !sine2d
    var_info(10)%size =  this%model%n_x * this%model%n_y
    !sine2d_ptr
    var_info(11)%size =  this%model%n_x * this%model%n_y
    var_info(13)%size =  this%model%n_y
    !int2d
    var_info(14)%size =  this%model%n_x * this%model%n_y
    var_info(15)%size =  this%model%n_x * this%model%n_y

    var_info(1)%type = get_type_and_kind( this%model%t )
    var_info(2)%type = get_type_and_kind( this%model%alpha )
    var_info(3)%type = get_type_and_kind( this%model%dt )
    var_info(4)%type = get_type_and_kind( this%model%t_end )
    var_info(5)%type = get_type_and_kind( this%model%n_x )
    var_info(6)%type = get_type_and_kind( this%model%n_y )
    var_info(7)%type = get_type_and_kind( this%model%id )
    var_info(8)%type = get_type_and_kind( this%model%sinevalue )
    var_info(9)%type = get_type_and_kind( this%model%sinevalue_tmp )
    var_info(10)%type = get_type_and_kind( this%model%sine2d )
    var_info(11)%type = get_type_and_kind( this%model%sine2d_ptr )
    var_info(12)%type = get_type_and_kind( this%model%description )
    var_info(13)%type = get_type_and_kind( this%model%logvar )
    var_info(14)%type = get_type_and_kind( this%model%int2d )
    var_info(15)%type = get_type_and_kind( this%model%double2d )

    bmi_status = BMI_SUCCESS
  end function sine_initialize

  ! BMI finalizer.
  function sine_finalize(this) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
  end function sine_finalize

  ! Model start time.
  function sine_start_time(this, time) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    bmi_status = BMI_SUCCESS
  end function sine_start_time

  ! Model end time.
  function sine_end_time(this, time) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%t_end)
    bmi_status = BMI_SUCCESS
  end function sine_end_time

  ! Model current time.
  function sine_current_time(this, time) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%t)
    bmi_status = BMI_SUCCESS
  end function sine_current_time

  ! Model time step.
  function sine_time_step(this, time_step) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%dt)
    bmi_status = BMI_SUCCESS
  end function sine_time_step

  ! Model time units.
  function sine_time_units(this, units) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
  end function sine_time_units

  ! Advance model by one time step.
  function sine_update(this) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
  end function sine_update

  ! Advance the model until the given time.
  function sine_update_until(this, time) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    if (time < this%model%t) then
       bmi_status = BMI_FAILURE
       return
    end if

    n_steps_real = (time - this%model%t) / this%model%dt
    n_steps = floor(n_steps_real)
    do i = 1, n_steps
       s = this%update()
    end do
    call update_frac(this, n_steps_real - dble(n_steps)) ! See near bottom of file
    bmi_status = BMI_SUCCESS
  end function sine_update_until

  ! Get the grid id for a particular variable.
  function sine_var_grid(this, name, grid) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('sine2d')
       grid = 2
       bmi_status = BMI_SUCCESS
    case('sine2d_ptr')
       grid = 2
       bmi_status = BMI_SUCCESS
    case('sinevalue_tmp')
       grid = 1
       bmi_status = BMI_SUCCESS
    case('logvar')
       grid = 1
       bmi_status = BMI_SUCCESS
    case('int2d')
       grid = 2
       bmi_status = BMI_SUCCESS
    case('double2d')
       grid = 2
       bmi_status = BMI_SUCCESS
    case default
       grid = 0
       bmi_status = BMI_SUCCESS
    end select
  end function sine_var_grid

  ! The type of a variable's grid.
  function sine_grid_type(this, grid, type) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
       type = "scalar"
       bmi_status = BMI_SUCCESS
    case(1)
       type = "1d"
       bmi_status = BMI_SUCCESS
    case(2)
       type = "2d"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_type

  ! The number of dimensions of a grid.
  function sine_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
       rank = 0
       bmi_status = BMI_SUCCESS
    case(1)
       rank = 1
       bmi_status = BMI_SUCCESS
    case(2)
       rank = 2
       bmi_status = BMI_SUCCESS
    case default
       rank = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_rank

  ! The dimensions of a grid.
  function sine_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status

    select case(grid)
    case(1)
       shape(:) = [this%model%n_x]
       bmi_status = BMI_SUCCESS
    case(2)
       shape(:) = [this%model%n_y, this%model%n_x]
       bmi_status = BMI_SUCCESS
    case default
       shape(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_shape

  ! The total number of elements in a grid.
  function sine_grid_size(this, grid, size) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
       size = 1
       bmi_status = BMI_SUCCESS
    case(1)
       size = this%model%n_x
       bmi_status = BMI_SUCCESS
    case(2)
       size = this%model%n_x * this%model%n_y
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_size

  ! The distance between nodes of a grid.
  function sine_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status

    select case(grid)
    case(1)
       spacing(:) = [1]
       bmi_status = BMI_SUCCESS
    case(2)
       spacing(:) = [1,1]
       bmi_status = BMI_SUCCESS
    case default
       spacing(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_spacing

  ! Coordinates of grid origin.
  function sine_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status

    select case(grid)
    case(1)
       origin(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case(2)
       origin(:) = [0.d0, 0.d0]
       bmi_status = BMI_SUCCESS
    case default
       origin(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_origin

  ! X-coordinates of grid nodes.
  function sine_grid_x(this, grid, x) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(1)
       x(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case(2)
       x(:) = [0.d0, 0.d0]
       bmi_status = BMI_SUCCESS
    case default
       x(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_x

  ! Y-coordinates of grid nodes.
  function sine_grid_y(this, grid, y) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(1)
       y(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case(2)
       y(:) = [0.d0, 0.d0]
       bmi_status = BMI_SUCCESS
    case default
       y(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_y

  ! Z-coordinates of grid nodes.
  function sine_grid_z(this, grid, z) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(1)
       z(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case(2)
       z(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       z(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_z

  ! Get the number of nodes in an unstructured grid.
  function sine_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    select case(grid)
    case(1:2)
       bmi_status = this%get_grid_size(grid, count)
    case default
       count = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function sine_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function sine_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function sine_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function sine_grid_face_count

  ! Get the edge-node connectivity.
  function sine_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer :: bmi_status

    edge_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function sine_grid_edge_nodes

  ! Get the face-edge connectivity.
  function sine_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer :: bmi_status

    face_edges(:) = -1
    bmi_status = BMI_FAILURE
  end function sine_grid_face_edges

  ! Get the face-node connectivity.
  function sine_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer :: bmi_status

    face_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function sine_grid_face_nodes

  ! Get the number of nodes for each face.
  function sine_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_sine), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status

    nodes_per_face(:) = -1
    bmi_status = BMI_FAILURE
  end function sine_grid_nodes_per_face

  ! The data type of the variable, as a string.
  function sine_var_type(this, name, type) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status, n

    do n = 1, STATE_VAR_NAME_COUNT
      if( var_info(n)%name == name ) then
           type = var_info(n)%type
           bmi_status = BMI_SUCCESS
           return 
      end if 
    end do
    type = '-'
    bmi_status = BMI_FAILURE
  end function sine_var_type

  ! The units of the given variable.
  function sine_var_units(this, name, units) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case("id")
       units = "NA"
       bmi_status = BMI_SUCCESS
    case("alpha")
       units = "1"
       bmi_status = BMI_SUCCESS
    case default
       units = "-"
       bmi_status = BMI_FAILURE
    end select
  end function sine_var_units

  ! Memory use per array element.
  function sine_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status

    select case(name)
    case("id")
       size = sizeof(this%model%id)  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("n_x")
       size = sizeof(this%model%n_x)  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("n_y")
       size = sizeof(this%model%n_y)  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("sinevalue")
       size = sizeof(this%model%sinevalue)  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("description")
       size = sizeof(this%model%description)
       bmi_status = BMI_SUCCESS
    case("t")
       size = sizeof(this%model%t)
       bmi_status = BMI_SUCCESS
    case("dt")
       size = sizeof(this%model%dt)
       bmi_status = BMI_SUCCESS
    case("t_end")
       size = sizeof(this%model%t_end)
       bmi_status = BMI_SUCCESS
    case("alpha")
       size = sizeof(this%model%alpha)
       bmi_status = BMI_SUCCESS
    case("sinevalue_tmp")
       size = sizeof(this%model%sinevalue_tmp(1))
       bmi_status = BMI_SUCCESS
    case("sine2d")
       size = sizeof(this%model%sine2d(1,1))
       bmi_status = BMI_SUCCESS
    case("sine2d_ptr")
       size = sizeof(this%model%sine2d_ptr(1,1))
       bmi_status = BMI_SUCCESS
    case("logvar")
       size = sizeof(this%model%logvar(1))
       bmi_status = BMI_SUCCESS
    case("int2d")
       size = sizeof(this%model%int2d(1,1))
       bmi_status = BMI_SUCCESS
    case("double2d")
       size = sizeof(this%model%int2d(1,1))
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_var_itemsize

  ! The size of the given variable.
  function sine_var_nbytes(this, name, nbytes) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: nbytes
    integer :: bmi_status
    integer :: s1, s2, s3, grid, grid_size, item_size

    s1 = this%get_var_grid(name, grid)
    s2 = this%get_grid_size(grid, grid_size)
    s3 = this%get_var_itemsize(name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       nbytes = -1
       bmi_status = BMI_FAILURE
    end if
  end function sine_var_nbytes

  ! The location (node, face, edge) of the given variable.
  function sine_var_location(this, name, location) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status

    select case(name)
    case default
       location = "node"
       bmi_status = BMI_SUCCESS
    end select
  end function sine_var_location

  ! Get a copy of a integer variable's values, flattened.
  function sine_get_int(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("id")
       dest = [this%model%id]
       bmi_status = BMI_SUCCESS
    case("n_x")
       dest = [this%model%n_x]
       bmi_status = BMI_SUCCESS
    case("n_y")
       dest = [this%model%n_y]
       bmi_status = BMI_SUCCESS
    case("int2d")
       dest = reshape(this%model%int2d, [size(this%model%int2d)])
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_int

  function sine_get_int1(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer(kind=1), intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_int1

  function sine_get_int2(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer(kind=2), intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_int2

  function sine_get_int8(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer(kind=8), intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_int8

  ! Get a copy of a real variable's values, flattened.
  function sine_get_float(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("sinevalue")
       dest = [this%model%sinevalue]
       bmi_status = BMI_SUCCESS
    case("t")
       dest = [this%model%t]
       bmi_status = BMI_SUCCESS
    case("dt")
       dest = [this%model%dt]
       bmi_status = BMI_SUCCESS
    case("t_end")
       dest = [this%model%t_end]
       bmi_status = BMI_SUCCESS
    case("alpha")
       dest = [this%model%alpha]
       bmi_status = BMI_SUCCESS
    case("sine2d")
       dest = reshape(this%model%sine2d, [size(this%model%sine2d)])
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1.0
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_float

  ! Get a copy of a double variable's values, flattened.
  function sine_get_double(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("sinevalue_tmp")
       dest = this%model%sinevalue_tmp
       bmi_status = BMI_SUCCESS
    case("sine2d_ptr")
       dest = reshape(this%model%sine2d_ptr, [size(this%model%sine2d_ptr)])
       bmi_status = BMI_SUCCESS
    case("double2d")
       dest = reshape(this%model%double2d, [size(this%model%double2d)])
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_double

  function sine_get_string(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(inout) :: dest
    integer :: bmi_status

    select case(name)
    case("description")
       dest = this%model%description
       bmi_status = BMI_SUCCESS
    case default
       dest = '-'
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_string

  function sine_get_logical(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    logical, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("logvar")
       dest = this%model%logvar
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = .false.
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_logical

  ! Get a reference to an integer-valued variable, flattened.
 function sine_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_sine), intent(in), target :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
!    integer, pointer, intent(inout) :: dest_ptr
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case("id")
       src = c_loc( this%model%id )
       call c_f_pointer( src, dest_ptr, [ 1 ] )
       bmi_status = BMI_SUCCESS
    case("n_x")
       src = c_loc( this%model%n_x )
       call c_f_pointer( src, dest_ptr, [ 1 ] )
       bmi_status = BMI_SUCCESS
    case("n_y")
       src = c_loc( this%model%n_y )
       call c_f_pointer( src, dest_ptr, [ 1 ] )
       bmi_status = BMI_SUCCESS
    case("int2d")
       src = c_loc( this%model%int2d(1,1) )
       call c_f_pointer( src, dest_ptr, [size(this%model%int2d)] )
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_int

 function sine_get_ptr_int1(this, name, dest_ptr) result (bmi_status)
    class (bmi_sine), intent(in), target :: this
    character (len=*), intent(in) :: name
    integer(kind=1), pointer, intent(inout) :: dest_ptr(:)
!    integer, pointer, intent(inout) :: dest_ptr
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_int1

 function sine_get_ptr_int2(this, name, dest_ptr) result (bmi_status)
    class (bmi_sine), intent(in), target :: this
    character (len=*), intent(in) :: name
    integer(kind=2), pointer, intent(inout) :: dest_ptr(:)
!    integer, pointer, intent(inout) :: dest_ptr
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_int2

 function sine_get_ptr_int8(this, name, dest_ptr) result (bmi_status)
    class (bmi_sine), intent(in), target :: this
    character (len=*), intent(in) :: name
    integer(kind=8), pointer, intent(inout) :: dest_ptr(:)
!    integer, pointer, intent(inout) :: dest_ptr
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_int8

 function sine_get_ptr_logical(this, name, dest_ptr) result (bmi_status)
    class (bmi_sine), intent(in), target :: this
    character (len=*), intent(in) :: name
    logical, pointer, intent(inout) :: dest_ptr(:)
!    integer, pointer, intent(inout) :: dest_ptr
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case("logvar")
       dest_ptr => this%model%logvar
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_logical

  ! Get a reference to a real-valued variable, flattened.
  function sine_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    !Note the `target` attribute here, it is necessary, othewise, pointer of
    ! non-pointer type of the `bmi_sine` components cannot be obtained.
    class (bmi_sine), intent(in), target :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case("sinevalue")
       src = c_loc( this%model%sinevalue )
       call c_f_pointer( src, dest_ptr, [ 1 ] )
       bmi_status = BMI_SUCCESS
    case("t")
       src = c_loc( this%model%t )
       call c_f_pointer( src, dest_ptr, [ 1 ] )
       bmi_status = BMI_SUCCESS
    case("dt")
       src = c_loc( this%model%dt )
       call c_f_pointer( src, dest_ptr, [ 1 ] )
       bmi_status = BMI_SUCCESS
    case("t_end")
       src = c_loc( this%model%t_end )
       call c_f_pointer( src, dest_ptr, [ 1 ] )
       bmi_status = BMI_SUCCESS
    case("alpha")
       src = c_loc( this%model%alpha )
       call c_f_pointer( src, dest_ptr, [ 1 ] )
       bmi_status = BMI_SUCCESS
    case("sine2d")
       src = c_loc( this%model%sine2d(1,1) )
       call c_f_pointer( src, dest_ptr, [size(this%model%sine2d)] )
       bmi_status = BMI_SUCCESS
    case default
       dest_ptr => null()
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_float

 ! ! Get a reference to an double-valued variable, flattened.
  function sine_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    !Note the `target` attribute here, it is necessary, othewise, pointer of
    ! non-pointer type of the `bmi_sine` components cannot be obtained.
    class (bmi_sine), intent(in), target :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case("sinevalue_tmp")
!       src = c_loc( this%model%sinevalue_tmp(1) )
!       call c_f_pointer( src, dest_ptr, [ size( this%model%sinevalue_tmp ) ] )
        dest_ptr => this%model%sinevalue_tmp
       bmi_status = BMI_SUCCESS
    case("double2d")
       src = c_loc( this%model%double2d(1,1) )
       call c_f_pointer( src, dest_ptr, [ size( this%model%double2d ) ] )
    case("sine2d_ptr")
       src = c_loc( this%model%sine2d_ptr(1,1) )
       call c_f_pointer( src, dest_ptr, [ size( this%model%sine2d_ptr ) ] )
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_double

  function sine_get_ptr_string(this, name, dest_ptr) result (bmi_status)
    !Note the `target` attribute here, it is necessary, othewise, pointer of
    ! non-pointer type of the `bmi_sine` components cannot be obtained.
    class (bmi_sine), intent(in), target :: this
    character (len=*), intent(in) :: name
    character(len=:), pointer, intent(inout) :: dest_ptr
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case("description")
       dest_ptr => this%model%description
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_string

  ! Get values of an integer variable at the given locations.
  function sine_get_at_indices_int(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    integer, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_at_indices_int

  ! Get values of a real variable at the given locations.
  function sine_get_at_indices_float(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    real, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_at_indices_float

  ! Get values of a double variable at the given locations.
  function sine_get_at_indices_double(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    double precision, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_at_indices_double

  ! Set new integer values.
  function sine_set_int(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("id")
       this%model%id = src(1)
       bmi_status = BMI_SUCCESS
    case("n_x")
       this%model%n_x = src(1)
       bmi_status = BMI_SUCCESS
    case("n_y")
       this%model%n_y = src(1)
       bmi_status = BMI_SUCCESS
    case("int2d")
       this%model%int2d = reshape( src, [this%model%n_y, this%model%n_x] )
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_int

  function sine_set_int1(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer(kind=1), intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_int1

  function sine_set_int2(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer(kind=2), intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_int2

  function sine_set_int8(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer(kind=8), intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_int8

  ! Set new real values.
  function sine_set_float(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("sinevalue")
       this%model%sinevalue = src(1)
       bmi_status = BMI_SUCCESS
    case("t")
       this%model%t = src(1)
       bmi_status = BMI_SUCCESS
    case("dt")
       this%model%dt = src(1)
       bmi_status = BMI_SUCCESS
    case("t_end")
       this%model%t_end = src(1)
       bmi_status = BMI_SUCCESS
    case("alpha")
       this%model%alpha = src(1)
       bmi_status = BMI_SUCCESS
    case("sine2d")
       this%model%sine2d = reshape( src, [this%model%n_y, this%model%n_x] )
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_float

  ! Set new double values.
  function sine_set_double(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("sinevalue_tmp")
       this%model%sinevalue_tmp = src
       bmi_status = BMI_SUCCESS
    case("sine2d_ptr")
       this%model%sine2d_ptr = reshape(src, [this%model%n_y, this%model%n_x])
       bmi_status = BMI_SUCCESS
    case("double2d")
       this%model%double2d = reshape(src, [this%model%n_y, this%model%n_x])
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_double

  function sine_set_string(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(in) :: src
    integer :: bmi_status

    select case(name)
    case("description")
       this%model%description = src
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_string

  function sine_set_logical(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    logical, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("logvar")
       this%model%logvar = src
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_logical

  ! Set integer values at particular locations.
  function sine_set_at_indices_int(this, name, inds, src) &
       result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    integer, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_at_indices_int

  ! Set real values at particular locations.
  function sine_set_at_indices_float(this, name, inds, src) &
       result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    real, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_at_indices_float

  ! Set double values at particular locations.
  function sine_set_at_indices_double(this, name, inds, src) &
       result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    double precision, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_at_indices_double

  ! A non-BMI helper routine to advance the model by a fractional time step.
  subroutine update_frac(this, time_frac)
    class (bmi_sine), intent(inout) :: this
    double precision, intent(in) :: time_frac
    real :: time_step

    if (time_frac > 0.0) then
       time_step = this%model%dt
       this%model%dt = time_step*real(time_frac)
       call advance_in_time(this%model)
       this%model%dt = time_step
    end if
  end subroutine update_frac

  function sine_get_bmi_version(this, version) result(bmi_status)
      class(bmi_sine), intent(in) :: this
      character(len=*), intent(out) :: version
      integer :: bmi_status
      version = "2.0_nGen_extension"
      bmi_status = BMI_SUCCESS
  end function sine_get_bmi_version

  function sine_get_var_count(this, role, count) result(bmi_status)
      class(bmi_sine), intent(in) :: this
      character(len=*), intent(in) :: role
      integer, intent(out) :: count
      integer :: bmi_status, n

      if( role == 'all' ) then
        count = STATE_VAR_NAME_COUNT
      else
        count = 0
        do n = 1, STATE_VAR_NAME_COUNT
           if( var_info(n)%role == role ) then
             count = count + 1
           end if
        end do
      end if
      bmi_status = BMI_SUCCESS
  end function sine_get_var_count

  function sine_get_var_names(this, role, names) result(bmi_status)
      class(bmi_sine), intent(in) :: this
      character(len=*), intent(in) :: role
      character(len=*), pointer, intent(out) :: names(:)
      integer :: bmi_status, n, role_count

      role_count = 1
      do n = 1, STATE_VAR_NAME_COUNT
        if( var_info(n)%role == role .or. role == 'all' ) then
           all_items(role_count) = var_info(n)%name
           role_count = role_count + 1
        end if 
      end do
      names => all_items
      bmi_status = BMI_SUCCESS
  end function sine_get_var_names

  function sine_get_var_index(this, name, index) result(bmi_status)
      class(bmi_sine), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: index
      integer :: bmi_status, n

      do n = 1, STATE_VAR_NAME_COUNT
        if( var_info(n)%name == name ) then
           index = var_info(n)%index 
           bmi_status = BMI_SUCCESS
           return 
        end if 
      end do
      index = -1
      bmi_status = BMI_FAILURE
  end function sine_get_var_index

  function sine_get_var_role(this, name, role) result(bmi_status)
      class(bmi_sine), intent(in) :: this
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: role
      integer :: bmi_status, n

      do n = 1, STATE_VAR_NAME_COUNT
        if( var_info(n)%name == name ) then
           role = var_info(n)%role 
           bmi_status = BMI_SUCCESS
           return 
        end if 
      end do
      role = "not_set"
      bmi_status = BMI_FAILURE
  end function sine_get_var_role

  function sine_get_var_length(this, name, size) result(bmi_status)
      class(bmi_sine), intent(in) :: this
      character(len=*), intent(in) :: name
      integer, intent(out) :: size
      integer :: bmi_status, n

      do n = 1, STATE_VAR_NAME_COUNT
        if( var_info(n)%name == name ) then
           size = var_info(n)%size 
           bmi_status = BMI_SUCCESS
           return 
        end if 
      end do
      size = -1
      bmi_status = BMI_FAILURE
  end function sine_get_var_length

  ! A non-BMI procedure for model introspection.
  subroutine print_model_info(this)
    class (bmi_sine), intent(in) :: this

    call print_info(this%model)
  end subroutine print_model_info

  function get_bmi_handle(this) result(bmi_status) bind(C, name="get_bmi_handle")
    use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
    implicit none
    type(c_ptr) :: this ! If not value, then from the C perspective `this` is a void**
    type(bmi_sine), pointer :: bmi_model
    integer(kind=c_int) :: bmi_status
    allocate(bmi_sine::bmi_model)
    if( .not. associated( bmi_model ) ) then
      bmi_status = BMI_FAILURE
    else
      this = c_loc(bmi_model)
      bmi_status = BMI_SUCCESS
    endif
  end function get_bmi_handle

  function destroy_bmi_handle(this) result(bmi_status) bind(C, name="destroy_bmi_handle")
    use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
    implicit none
    type(c_ptr) :: this ! If not value, then from the C perspective `this` is a void**
    type(bmi_sine), pointer :: bmi_model => null()
    integer(kind=c_int) :: bmi_status

    call c_f_pointer(this, bmi_model)

    if( .not. associated( bmi_model ) ) then
      bmi_status = BMI_FAILURE
    else
      bmi_status = bmi_model%finalize()
      deallocate( bmi_model )
      nullify( bmi_model )
      bmi_status = BMI_SUCCESS
    endif
  end function destroy_bmi_handle

  function get_box_handle(this, bmi_ptr) result(bmi_status) bind(C, name="get_box_handle")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use iso_c_bmif_2_0
   implicit none
   type(c_ptr), intent(out) :: this ! If not value, then from the C perspective `this` is a void**
   type(c_ptr), intent(in) :: bmi_ptr ! If not value, then from the C perspective `this` is a void**
   integer(kind=c_int) :: bmi_status
   !Create the model instance to use
   type(bmi_sine), pointer :: f_bmi_ptr
   !Create a simple pointer wrapper
   type(box), pointer :: bmi_box

   !allocate the pointer box
   allocate(bmi_box)

   call c_f_pointer( bmi_ptr, f_bmi_ptr)
   !associate the wrapper pointer the created model instance
   bmi_box%ptr => f_bmi_ptr

   if( .not. associated( bmi_box ) .or. .not. associated( bmi_box%ptr ) ) then
    bmi_status = BMI_FAILURE
   else
    !Return the pointer to box
    this = c_loc(bmi_box)
    bmi_status = BMI_SUCCESS
   endif
 end function get_box_handle

  function destroy_box_handle(this) result(bmi_status) bind(C, name="destroy_box_handle")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use iso_c_bmif_2_0
   implicit none
   type(c_ptr), intent(in) :: this ! If not value, then from the C perspective `this` is a void**
   type(box), pointer :: f_this
   integer(kind=c_int) :: bmi_status
   !Create a simple pointer wrapper

   call c_f_pointer( this, f_this)

   if( .not. associated( f_this ) ) then
    bmi_status = BMI_FAILURE
   else
!
!    this will results in  double free spaces
!    if ( associated( f_this%ptr ) ) then
!       deallocate( f_this%ptr )
!       nullify( f_this%ptr )
!    endif
    deallocate( f_this ) 
    bmi_status = BMI_SUCCESS
   endif
 end function destroy_box_handle

  function register_bmi(this) result(bmi_status) bind(C, name="register_bmi")
    use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
    implicit none
    type(c_ptr), intent(out) :: this ! If not value, then from the C perspective `this` is a void**
    type(c_ptr) :: bmi_ptr
    integer(kind=c_int) :: bmi_status

    bmi_status = get_bmi_handle( bmi_ptr )

    if ( bmi_status .eq. BMI_FAILURE ) then 
            return
    end if
    bmi_status = get_box_handle( this, bmi_ptr )

  end function register_bmi

  function unregister_bmi(this) result(bmi_status) bind(C, name="unregister_bmi")
    use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
    use iso_c_bmif_2_0
    implicit none
    type(c_ptr), intent(inout) :: this ! If not value, then from the C perspective `this` is a void**
    type(box), pointer :: bmi_box
    integer(kind=c_int) :: bmi_status

    call c_f_pointer( this, bmi_box )

    if( .not. associated( bmi_box%ptr ) ) then
      bmi_status = BMI_FAILURE
    else
      bmi_status = bmi_box%ptr%finalize()
      deallocate( bmi_box%ptr )
      nullify( bmi_box%ptr )
      bmi_status = BMI_SUCCESS
    endif

    if ( bmi_status .eq. BMI_FAILURE ) then 
            return
    end if
    bmi_status = destroy_box_handle( this )
  end function unregister_bmi

end module bmisinef
