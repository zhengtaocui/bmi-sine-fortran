module bmisinef

  use sinef
  use bmif_2_0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  integer, parameter :: STATE_VAR_NAME_COUNT = 3 

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
     procedure :: get_value_float => sine_get_float
     procedure :: get_value_double => sine_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
     procedure :: get_value_ptr_int => sine_get_ptr_int
     procedure :: get_value_ptr_float => sine_get_ptr_float
     procedure :: get_value_ptr_double => sine_get_ptr_double
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_float, &
          get_value_ptr_double
     procedure :: get_value_at_indices_int => sine_get_at_indices_int
     procedure :: get_value_at_indices_float => sine_get_at_indices_float
     procedure :: get_value_at_indices_double => sine_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => sine_set_int
     procedure :: set_value_float => sine_set_float
     procedure :: set_value_double => sine_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
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
  integer, parameter :: input_item_count = 2
  integer, parameter :: output_item_count = 1
  integer, parameter :: all_item_count = 3
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: &
       output_items = (/'sine_value_of_radian'/)
  character (len=BMI_MAX_VAR_NAME), target, &
          dimension(all_item_count) :: all_items

  type(variable), parameter, dimension(3) :: &
          var_info = (/variable(1, 'Radian', 'real', 1, &
                                'no_set', 'DIMENSIONLESS', &
                                'node', 0 ),                &
                      variable(2, 'plate_surface__thermal_diffusivity', 'real', 1, &
                                'no_set', 'm^2/s', &
                                'node', 0 ),                &
                      variable(3, 'sine_value_of_radian', 'real', 1, &
                                'no_set', 'DIMENSIONLESS', &
                                'node', 0 )/)
  
contains

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

    input_items(1) = 'Radian'
    input_items(2) = 'plate_surface__thermal_diffusivity'

    names => input_items
    bmi_status = BMI_SUCCESS
  end function sine_input_var_names

  ! List output variables.
  function sine_output_var_names(this, names) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

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
    case('Radian')
       grid = 0
       bmi_status = BMI_SUCCESS
    case('model__identification_number')
       grid = 1
       bmi_status = BMI_SUCCESS
    case default
       grid = -1
       bmi_status = BMI_FAILURE
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
       type = "uniform_rectilinear"
       bmi_status = BMI_SUCCESS
    case(1)
       type = "scalar"
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
       rank = 2
       bmi_status = BMI_SUCCESS
    case(1)
       rank = 0
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
    case(0)
       shape(:) = [1, 1]
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
       size = 1
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
    case(0)
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
    case(0)
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
    case(0:1)
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
    integer :: bmi_status

    select case(name)
    case("Radian")
       type = "real"
       bmi_status = BMI_SUCCESS
    case("sine_value_of_radian")
       type = "real"
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       type = "real"
       bmi_status = BMI_SUCCESS
    case("model__identification_number")
       type = "integer"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function sine_var_type

  ! The units of the given variable.
  function sine_var_units(this, name, units) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       units = "NA"
       bmi_status = BMI_SUCCESS
    case("model__identification_number")
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
    case("Radian")
       size = sizeof(this%model%sinevalue)  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("model__identification_number")
       size = sizeof(this%model%id)                ! 'sizeof' in gcc & ifort
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
    case("model__identification_number")
       dest = [this%model%id]
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_int

  ! Get a copy of a real variable's values, flattened.
  function sine_get_float(this, name, dest) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("Radian")
       ! This would be safe, but subject to indexing errors.
       ! do j = 1, this%model%n_y
       !    do i = 1, this%model%n_x
       !       k = j + this%model%n_y*(i-1)
       !       dest(k) = this%model%temperature(j,i)
       !    end do
       ! end do

       ! This is an equivalent, elementwise copy into `dest`.
       ! See https://stackoverflow.com/a/11800068/1563298
       dest = [this%model%t]
       bmi_status = BMI_SUCCESS
    case("sine_value_of_radian")
       dest = [this%model%sinevalue]
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       dest = [this%model%alpha]
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
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_double

  ! Get a reference to an integer-valued variable, flattened.
  function sine_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_int

  ! Get a reference to a real-valued variable, flattened.
  function sine_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_float

  ! Get a reference to an double-valued variable, flattened.
  function sine_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_sine), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_get_ptr_double

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
    case("model__identification_number")
       this%model%id = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_int

  ! Set new real values.
  function sine_set_float(this, name, src) result (bmi_status)
    class (bmi_sine), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("Radian")
       ! This would be safe, but subject to indexing errors.
       ! do j = 1, this%model%n_y
       !    do i = 1, this%model%n_x
       !       k = j + this%model%n_y*(i-1)
       !       dest(k) = this%model%temperature(j,i)
       !    end do
       ! end do

       ! This is an equivalent, elementwise copy into `dest`.
       ! See https://stackoverflow.com/a/11800068/1563298
       this%model%t = src(1)
       bmi_status = BMI_SUCCESS
    case("sine_value_of_radian")
       this%model%sinevalue = src(1)
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       this%model%alpha = src(1)
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
    case default
       bmi_status = BMI_FAILURE
    end select
  end function sine_set_double

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

  function bmi_factory(this) result(bmi_status) bind(C, name="bmi_factory")
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
  end function bmi_factory

  function bmi_destroy(this) result(bmi_status) bind(C, name="bmi_destroy")
    use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
    implicit none
    type(c_ptr) :: this ! If not value, then from the C perspective `this` is a void**
    type(bmi_sine), pointer :: bmi_model
    integer(kind=c_int) :: bmi_status

    call c_f_pointer(this, bmi_model)

    if( .not. associated( bmi_model ) ) then
      bmi_status = BMI_FAILURE
    else
      deallocate( bmi_model )
      bmi_status = BMI_SUCCESS
    endif
  end function bmi_destroy

  function create_box(this, bmi_ptr) result(bmi_status) bind(C, name="c_create_box")
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
 end function create_box

  function delete_box(this) result(bmi_status) bind(C, name="c_delete_box")
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
    deallocate( f_this ) 
    bmi_status = BMI_SUCCESS
   endif
 end function delete_box

end module bmisinef
