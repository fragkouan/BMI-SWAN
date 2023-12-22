    module bmiswan

    use m_swan
    use bmif_2_0
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
    implicit none

    type, extends (bmi) :: bmi_swan
        private
        type (swan_model) :: model
    contains
    procedure :: initialize => swan_initialize
    procedure :: update => swan_update
    procedure :: update_until => swan_update_until
    procedure :: finalize => swan_finalize
    procedure :: get_component_name => swan_component_name
    procedure :: get_input_item_count => swan_input_item_count
    procedure :: get_output_item_count => swan_output_item_count
    procedure :: get_input_var_names => swan_input_var_names
    procedure :: get_output_var_names => swan_output_var_names
    procedure :: get_var_grid => swan_var_grid
    procedure :: get_var_type => swan_var_type
    procedure :: get_var_units => swan_var_units
    procedure :: get_var_itemsize => swan_var_itemsize
    procedure :: get_var_nbytes => swan_var_nbytes
    procedure :: get_var_location => swan_var_location
    procedure :: get_current_time => swan_current_time
    procedure :: get_start_time => swan_start_time
    procedure :: get_end_time => swan_end_time
    procedure :: get_time_units => swan_time_units
    procedure :: get_time_step => swan_time_step
    procedure :: get_value_int => swan_get_int
    procedure :: get_value_float => swan_get_float
    procedure :: get_value_double => swan_get_double
    generic :: get_value => &
         get_value_int, &
         get_value_float, &
         get_value_double
    procedure :: get_value_ptr_int => swan_get_ptr_int
    procedure :: get_value_ptr_float => swan_get_ptr_float
    procedure :: get_value_ptr_double => swan_get_ptr_double
    generic :: get_value_ptr => &
         get_value_ptr_int, &
         get_value_ptr_float, &
         get_value_ptr_double
   procedure :: get_value_at_indices_int => swan_get_at_indices_int
   procedure :: get_value_at_indices_float => swan_get_at_indices_float
   procedure :: get_value_at_indices_double => swan_get_at_indices_double
   generic :: get_value_at_indices => &
        get_value_at_indices_int, &
        get_value_at_indices_float, &
        get_value_at_indices_double
   procedure :: set_value_int => swan_set_int
   procedure :: set_value_float => swan_set_float
   procedure :: set_value_double => swan_set_double
   generic :: set_value => &
        set_value_int, &
        set_value_float, &
        set_value_double
   procedure :: set_value_at_indices_int => swan_set_at_indices_int
   procedure :: set_value_at_indices_float => swan_set_at_indices_float
   procedure :: set_value_at_indices_double => swan_set_at_indices_double
   generic :: set_value_at_indices => &
        set_value_at_indices_int, &
        set_value_at_indices_float, &
        set_value_at_indices_double
   procedure :: get_grid_rank => swan_grid_rank
   procedure :: get_grid_size => swan_grid_size
   procedure :: get_grid_type => swan_grid_type
   procedure :: get_grid_shape => swan_grid_shape
   procedure :: get_grid_spacing => swan_grid_spacing
   procedure :: get_grid_origin => swan_grid_origin
   procedure :: get_grid_x => swan_grid_x
   procedure :: get_grid_y => swan_grid_y
   procedure :: get_grid_z => swan_grid_z
   procedure :: get_grid_node_count => swan_grid_node_count
   procedure :: get_grid_edge_count => swan_grid_edge_count
   procedure :: get_grid_face_count => swan_grid_face_count
   procedure :: get_grid_edge_nodes => swan_grid_edge_nodes
   procedure :: get_grid_face_edges => swan_grid_face_edges
   procedure :: get_grid_face_nodes => swan_grid_face_nodes
   procedure :: get_grid_nodes_per_face => swan_grid_nodes_per_face
   end type bmi_swan

   private
   public :: bmi_swan

   character (len=BMI_MAX_COMPONENT_NAME), target :: &
     component_name = "SWAN-BMI"

   !Exchnage items
   integer, parameter :: input_item_count = 3
   integer, parameter :: output_item_count = 7
   character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items = (/ &
       'sea_water_surface__elevation           ', &
       'sea_water_flow__x_component_of_velocity', &
       'sea_water_flow__y_component_of_velocity'/)
   character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: &
       output_items =(/ &
       'sea_surface_water_wave__height            ', &
       'sea_surface_water_wave__direction         ', &
       'sea_surface_water_wave__wavelength        ', &
       'sea_surface_water_wave__breaking_fraction ', &
       'coupling_status                           ', &
       'coupling_timestep                         ', &
       'simulation_duration                       ' &
       /)

   contains

   ! Get the name of the model.
   function swan_component_name(this, name) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   character (len=*), pointer, intent(out) :: name
   integer :: bmi_status

   name => component_name
   bmi_status = BMI_SUCCESS
   end function swan_component_name

   ! Count the input variables.
   function swan_input_item_count(this, count) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(out) :: count
   integer :: bmi_status

   count = input_item_count
   bmi_status = BMI_SUCCESS
   end function swan_input_item_count

   ! Count the output variables.
   function swan_output_item_count(this, count) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(out) :: count
   integer :: bmi_status

   count = output_item_count
   bmi_status = BMI_SUCCESS
   end function swan_output_item_count

   ! List input variables.
   function swan_input_var_names(this, names) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   character (*), pointer, intent(out) :: names(:)
   integer :: bmi_status

   names => input_items
   bmi_status = BMI_SUCCESS
   end function swan_input_var_names


   ! List output variables.
   function swan_output_var_names(this, names) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   character (*), pointer, intent(out) :: names(:)
   integer :: bmi_status
   names => output_items
   bmi_status = BMI_SUCCESS
   end function swan_output_var_names

   ! BMI initializer.
   function swan_initialize(this, config_file) result (bmi_status)
     !!!
     ! Initialise the SWAN model. This means:
     ! 1) Copy the SWAN configuration file to INPUT
     ! 2) Get the name of the SWAN configuration file without .swn
     ! 3) call the SWANINIT subroutine, which reads the SWAN configura-
     !    tion file and initialises the necessary arrays
     ! 4)
     !
     ! Inputs:
     ! config_file : Character containing the filepath for the SWAN con-
     !               figuration file
     !
     ! Outputs :
     ! this       : The BMI-SWAN model
     ! bmi_status : An integer stating if everything went without a
     !              hitch. = 0 if successful, = 1 if unsuccessful
     !
     ! Variables :
     ! n      : Integer index
     ! grid   : Integer containing the grid identifier of a variable
     ! status : Integer containing the result of a function being compl-
     !          eted successfully or not
     ! size :
     !!!
!***********************************************************************
     class (bmi_swan), intent(out) :: this
     character (len=*), intent(in) :: config_file
     integer :: bmi_status
     integer :: n
     integer :: grid, status
     integer :: size

!***********************************************************************
     ! Rename config_file to INPUT, as SWAN reads the configuration file
     ! "INPUT"
     call system("cp " // config_file // " ./INPUT")
     ! Get the basename of the SWAN configuration file, i.e. the filena-
     ! me without the .swn
     call get_basename(config_file)

     ! This subroutine can be found at swanmain.ftn. It reads the conf-
     ! iguration file, initialises the necessary array/parameters, ass-
     ! ings the default values
     call SWANINIT()

     ! Get simulation start time, end time and time-step
     call get_simulation_times(this%model)

     !! Initialise grid sizes
     ! Loop through the input items, i.e. the fields that BMI-SWAN acc-
     ! epts as inputs
     do n=1, input_item_count
        ! Get the input grid identifier
        status = this%get_var_grid(input_items(n), grid)

        ! Get the input field array size
        call get_input_field_size(this%model, grid, size)
     end do

     ! Get the coupling status between SWAN and Thetis
     call get_thetis_coupling_status(this%model)

     ! If SWAN and Thetis are fully coupled, or SWAN gives information
     ! to Thetis
     if ( &
        (this%model%coupled_status.eq.'Fully coupled') &
        .or. &
        (this%model%coupled_status.eq.'SWAN-to-Thetis') &
     ) then
     
        ! Get Thetis Grid size for
        call get_thetis_grid(this%model)

        ! Initialize output variable sizes and allocate the relevant arrays
        call set_output_field_size(this%model)
     end if

     bmi_status = BMI_SUCCESS
   end function swan_initialize

   ! BMI finalizer.
   function swan_finalize(this) result (bmi_status)
   class (bmi_swan), intent(inout) :: this
   integer :: bmi_status
   logical :: file_exists

   ! Rename output files
   call rename("PRINT", trim(basename) // "prt")
   INQUIRE( FILE='./Errfile', EXIST = file_exists)
   if (file_exists) then
     call rename("./Errfile", trim(basename) // "erf")
   else
     call rename("./norm_end", trim(basename) // "erf")
   end if

   call system("rm INPUT")

   call SWANFINALIZE()



   bmi_status = BMI_SUCCESS
   end function swan_finalize

   ! Model start time.
   function swan_start_time(this, time) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   double precision, intent(out) :: time
   integer :: bmi_status

   time = 0.d0
   !time = this%model%model_simulation%model_time%Timestep
   bmi_status = BMI_SUCCESS
   end function swan_start_time

   ! Model end time.
   function swan_end_time(this, time) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   double precision, intent(out) :: time
   integer :: bmi_status

   time = 0.d0
   bmi_status = BMI_SUCCESS
   end function swan_end_time

   ! Model current time.
   function swan_current_time(this, time) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   double precision, intent(out) :: time
   integer :: bmi_status

   time = 0.d0
   bmi_status = BMI_SUCCESS
   end function swan_current_time

   ! Model time step.
   function swan_time_step(this, time_step) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   double precision, intent(out) :: time_step
   integer :: bmi_status

   time_step = this%model%dt
   bmi_status = BMI_SUCCESS
   end function swan_time_step

   ! Model time units.
   function swan_time_units(this, units) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   character (len=*), intent(out) :: units
   integer :: bmi_status

   units = "s"
   bmi_status = BMI_SUCCESS
   end function swan_time_units

   ! Advance model by one time step.
   function swan_update(this) result (bmi_status)
   class (bmi_swan), intent(inout) :: this
   integer :: bmi_status

   bmi_status = BMI_SUCCESS
   end function swan_update

   ! Advance the model until the given time.
   function swan_update_until(this, time) result (bmi_status)
   class (bmi_swan), intent(inout) :: this
   double precision, intent(in) :: time
   integer :: time_int
   integer :: bmi_status

   time_int = INT((time)/this%model%dt)
   call set_last_time_step(this%model, time_int)

   call SWMAIN()
   bmi_status = BMI_SUCCESS
   end function swan_update_until

   ! Get the grid id for a particular variable.
   function swan_var_grid(this, name, grid) result (bmi_status)
     !!!
     ! Get the grid identifier, i.e. an integer, for a variable
     !
     ! Inputs:
     ! this :
     ! name : A character containing the name of the variable for which
     !        the grid identifier is requested
     !
     ! Outputs:
     ! grid       : An integer containing the grid identifier
     ! bmi_status : An integer stating if everything went without a
     !              hitch. = 0 if successful, = 1 if unsuccessful
     !
     ! Note:
     ! We use grid identifiers from 1 to 99 for the input variables,
     ! while for the output variables the identifiers range from 100 to
     ! 199. The grid identifier 200 is utilised for information for the
     ! coupling and the model, like the coupling status, the coupling
     ! timestep and the simulation duration
     !!!
   class (bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(out) :: grid
   integer :: bmi_status

   ! Depending on the name of the variable, get the grid identifier
   select case(name)

   ! If name = eta
   case('sea_water_surface__elevation')
     grid = 0
     bmi_status = BMI_SUCCESS

   ! If name = u (x-component of depth-averaged current velocity)
   case('sea_water_flow__x_component_of_velocity')
     grid = 1
     bmi_status = BMI_SUCCESS

   ! If name = v (y-component of depth-averaged current velocity)
   case('sea_water_flow__y_component_of_velocity')
     grid = 2
     bmi_status = BMI_SUCCESS

   ! If name = Hs (wave height)
   case('sea_surface_water_wave__height')
     grid = 101
     bmi_status = BMI_SUCCESS

   ! If name = Dir (mean wave direction)
   case('sea_surface_water_wave__direction')
     grid = 102
     bmi_status = BMI_SUCCESS

   ! If name = Wlen (mean wavelength)
   case('sea_surface_water_wave__wavelength')
     grid = 103
     bmi_status = BMI_SUCCESS

   ! If name = Qb (percentage of wave breaking)
   case('sea_surface_water_wave__breaking_fraction')
     grid = 104
     bmi_status = BMI_SUCCESS

   ! If name = "Coupling status" or "Coupling timestep" or "simulation
   ! duration"
   case('coupling_status', &
        'coupling_timestep', &
        'simulation_duration')
     grid = 200
     bmi_status = BMI_SUCCESS

   ! If the name does not belong to one of the above cases, terminate
   case default
     grid = -1
     bmi_status = BMI_FAILURE
   end select

   end function swan_var_grid

   ! The type of a variable's grid.
   function swan_grid_type(this, grid, type) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   character (len=*), intent(out) :: type
   integer :: bmi_status

   type = "scalar"
   bmi_status = BMI_SUCCESS
   end function swan_grid_type

   ! The number of dimensions of a grid.
   function swan_grid_rank(this, grid, rank) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, intent(out) :: rank
   integer :: bmi_status

   rank = 1
   bmi_status = BMI_SUCCESS
   end function swan_grid_rank

   ! The dimensions of a grid.
   function swan_grid_shape(this, grid, shape) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, dimension(:), intent(out) :: shape
   integer :: bmi_status

    shape(:) = [5 , 2]
    bmi_status = BMI_SUCCESS
    end function swan_grid_shape

   ! The total number of elements in a grid.
   function swan_grid_size(this, grid, size) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, intent(out) :: size
   integer :: bmi_status

   select case(grid)
   case(0) ! etas from Thetis
     size = this%model%water_elev%arr_size
     bmi_status = BMI_SUCCESS
   case(1) ! x-component of current (u-velocity) from Thetis
     size = this%model%cur_vel%x_comp_arr_size
     bmi_status = BMI_SUCCESS
   case(2) ! y-component of current (v-velocity) from Thetis
     size = this%model%cur_vel%y_comp_arr_size
     bmi_status = BMI_SUCCESS
   case(101, 102, 103, 104)
     size = this%model%output_var_size
     bmi_status = BMI_SUCCESS
   case(200)
     size = 1
     bmi_status = BMI_SUCCESS
   case default
     size = -1
     bmi_status = BMI_FAILURE
   end select
   end function swan_grid_size

   ! The distance between nodes of a grid.
   function swan_grid_spacing(this, grid, spacing) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   double precision, dimension(:), intent(out) :: spacing
   integer :: bmi_status

   spacing(:) = -1.d0
   bmi_status = BMI_FAILURE
   end function swan_grid_spacing

   ! Coordinates of grid origin.
   function swan_grid_origin(this, grid, origin) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   double precision, dimension(:), intent(out) :: origin
   integer :: bmi_status

   origin(:) = -1.d0
   bmi_status = BMI_FAILURE
   end function swan_grid_origin

   ! X-coordinates of grid nodes.
   function swan_grid_x(this, grid, x) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   double precision, dimension(:), intent(out) :: x
   integer :: bmi_status

   x = -1.d0
   bmi_status = BMI_SUCCESS
   end function swan_grid_x

   ! Y-coordinates of grid nodes.
   function swan_grid_y(this, grid, y) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   double precision, dimension(:), intent(out) :: y
   integer :: bmi_status

   y = -1.d0
   bmi_status = BMI_SUCCESS
   end function swan_grid_y

   ! Z-coordinates of grid nodes.
   function swan_grid_z(this, grid, z) result (bmi_status)
   class (bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   double precision, dimension(:), intent(out) :: z
   integer :: bmi_status

   z = -1.d0
   bmi_status = BMI_SUCCESS
   end function swan_grid_z

   ! Get the number of nodes in an unstructured grid.
   function swan_grid_node_count(this, grid, count) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, intent(out) :: count
   integer :: bmi_status

   count = 5
   bmi_status = BMI_SUCCESS
   end function swan_grid_node_count

   ! Get the number of edges in an unstructured grid.
   function swan_grid_edge_count(this, grid, count) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, intent(out) :: count
   integer :: bmi_status

   count = 1
   bmi_status = BMI_SUCCESS
   end function swan_grid_edge_count

   ! Get the number of faces in an unstructured grid.
   function swan_grid_face_count(this, grid, count) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, intent(out) :: count
   integer :: bmi_status


   count = 0
   bmi_status = BMI_SUCCESS
   end function swan_grid_face_count

   ! Get the edge-node connectivity.
   function swan_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, dimension(:), intent(out) :: edge_nodes
   integer :: bmi_status

   edge_nodes(:) = -1
   bmi_status = BMI_SUCCESS
   end function swan_grid_edge_nodes

   ! Get the face-edge connectivity.
   function swan_grid_face_edges(this, grid, face_edges) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, dimension(:), intent(out) :: face_edges
   integer :: bmi_status

   face_edges(:) = -1
   bmi_status = BMI_SUCCESS
   end function swan_grid_face_edges

   ! Get the face-node connectivity.
   function swan_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, dimension(:), intent(out) :: face_nodes
   integer :: bmi_status

   face_nodes(:) = -1
   bmi_status = BMI_SUCCESS
   end function swan_grid_face_nodes

   ! Get the number of nodes for each face.
   function swan_grid_nodes_per_face(this, grid, nodes_per_face) &
      result(bmi_status)
   class(bmi_swan), intent(in) :: this
   integer, intent(in) :: grid
   integer, dimension(:), intent(out) :: nodes_per_face
   integer :: bmi_status

   nodes_per_face(:) = -1
   bmi_status = BMI_SUCCESS
   end function swan_grid_nodes_per_face


   ! The data type of the variable, as a string.
   function swan_var_type(this, name, type) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   character (len=*), intent(out) :: type
   integer :: bmi_status

   select case(name)
   case("sea_water_surface__elevation", &
        "sea_water_flow__x_component_of_velocity", &
        "sea_water_flow__y_component_of_velocity", &
        'sea_surface_water_wave__height            ', &
        'sea_surface_water_wave__direction         ', &
        'sea_surface_water_wave__wavelength        ', &
        'sea_surface_water_wave__breaking_fraction ')
     type = "real"
     bmi_status = BMI_SUCCESS
   case('coupling_status')
     type='integer'
     bmi_status = BMI_SUCCESS
   case ('coupling_timestep', &
         "simulation_duration")
     type='double precision'
     bmi_status = BMI_SUCCESS
   case default
     type= "-"
     bmi_status = BMI_FAILURE
   end select
   end function swan_var_type

   ! The units of the given variable.
   function swan_var_units(this, name, units) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   character (len=*), intent(out) :: units
   integer :: bmi_status

   units = 'something'
   bmi_status = BMI_SUCCESS
   end function swan_var_units

   ! Memory use per array element.
   function swan_var_itemsize(this, name, size) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(out) :: size
   integer :: bmi_status

   size = 10
   bmi_status = BMI_SUCCESS
   end function swan_var_itemsize


   ! The size of the given variable.
   function swan_var_nbytes(this, name, nbytes) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(out) :: nbytes
   integer :: bmi_status

   nbytes = 10
   bmi_status = BMI_SUCCESS
   end function swan_var_nbytes

   ! The location (node, face, edge) of the given variable.
   function swan_var_location(this, name, location) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   character (len=*), intent(out) :: location
   integer :: bmi_status

   location = "node"
   bmi_status = BMI_SUCCESS
   end function swan_var_location

   ! Get a copy of a integer variable's values, flattened.
   function swan_get_int(this, name, dest) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(inout) :: dest(:)
   integer :: bmi_status

   if (name=='coupling_status') then
     call convert_coupling_stat_to_int(this%model%coupled_status, dest)
   end if

   bmi_status = BMI_SUCCESS
   end function swan_get_int

   ! Get a copy of a real variable's values, flattened.
   function swan_get_float(this,  name, dest) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   real, intent(inout) :: dest(:)
   integer :: bmi_status

   call get_output_field(name, dest)

   bmi_status = BMI_SUCCESS
   end function swan_get_float

   ! Get a copy of a double variable's values, flattened.
   function swan_get_double(this,  name, dest) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   double precision, intent(inout) :: dest(:)
   integer :: bmi_status

   if (name=='coupling_timestep') then
      if (this%model%coupled_status/='No coupling') then
        call get_coupling_timestep(dest)
        bmi_status = BMI_SUCCESS
      else
        bmi_status = BMI_FAILURE
      end if
   else if (name=='simulation_duration') then
     dest = this%model%t_end-this%model%t_start
     bmi_status = BMI_SUCCESS
   end if

   end function swan_get_double

   ! Get a reference to an integer-valued variable, flattened.
   function swan_get_ptr_int(this, name, dest_ptr) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, pointer, intent(inout) :: dest_ptr(:)
   integer :: bmi_status

   dest_ptr = [1]
   bmi_status = BMI_SUCCESS
   end function swan_get_ptr_int

   ! Get a reference to a real-valued variable, flattened.
   function swan_get_ptr_float(this, name, dest_ptr) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   real, pointer, intent(inout) :: dest_ptr(:)
   integer :: bmi_status

   dest_ptr = [1.]
   bmi_status = BMI_SUCCESS
   end function swan_get_ptr_float

   ! Get a reference to an double-valued variable, flattened.
   function swan_get_ptr_double(this, name, dest_ptr) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   double precision, pointer, intent(inout) :: dest_ptr(:)
   integer :: bmi_status

   dest_ptr = [1.d0]
   bmi_status = BMI_SUCCESS
   end function swan_get_ptr_double

   ! Get values of an integer variable at the given locations.
   function swan_get_at_indices_int(this, name, dest, inds) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(inout) :: dest(:)
   integer, intent(in) :: inds(:)
   integer :: bmi_status
   integer :: i

   do i = 1,  size(inds)
       dest(i) = i
   end do

   bmi_status = BMI_SUCCESS
   end function swan_get_at_indices_int

   ! Get values of a real variable at the given locations.
   function swan_get_at_indices_float(this, name, dest, inds) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   real, intent(inout) :: dest(:)
   integer, intent(in) :: inds(:)
   integer :: bmi_status
   integer :: i

   do i = 1,  size(inds)
       dest(i) = 1.0
   end do

   bmi_status = BMI_SUCCESS
   end function swan_get_at_indices_float


   ! Get values of a double variable at the given locations.
   function swan_get_at_indices_double(this, name, dest, inds) result(bmi_status)
   class(bmi_swan), intent(in) :: this
   character (len=*), intent(in) :: name
   double precision, intent(inout) :: dest(:)
   integer, intent(in) :: inds(:)
   integer :: bmi_status
   integer :: i

   do i = 1,  size(inds)
       dest(i) = 1.d0
   end do

   bmi_status = BMI_SUCCESS
   end function swan_get_at_indices_double

   ! Set new integer values.
   function swan_set_int(this, name, src) result(bmi_status)
   class(bmi_swan), intent(inout) :: this
   character (len=*), intent(in) :: name
   integer, intent(in) :: src(:)
   integer :: bmi_status

   bmi_status = BMI_SUCCESS
   end function swan_set_int

   ! Set new real values.

   function swan_set_float(this, name, src) result(bmi_status)
   class(bmi_swan), intent(inout) :: this
   character (len=*), intent(in) :: name
   real, dimension(:), intent(in) :: src
   integer :: bmi_status

   ! if (name=="sea_water_flow__x_component_of_velocity") then
   !   print *, src
   ! end if
   call set_input_field(this%model, src, name)

   bmi_status = BMI_SUCCESS
   end function swan_set_float

   ! Set new double values.
   function swan_set_double(this, name, src) result(bmi_status)
   class(bmi_swan), intent(inout) :: this
   character (len=*), intent(in) :: name
   double precision, intent(in) :: src(:)
   integer :: bmi_status


   bmi_status = BMI_SUCCESS
   end function swan_set_double

   ! Set integer values at particular locations.
   function swan_set_at_indices_int(this, name, inds, src) &
       result(bmi_status)
   class(bmi_swan), intent(inout) :: this
   character (len=*), intent(in) :: name
   integer, intent(in) :: inds(:)
   integer, intent(in) :: src(:)
   integer :: bmi_status

   bmi_status = BMI_SUCCESS
   end function swan_set_at_indices_int

   ! Set real values at particular locations.
   function swan_set_at_indices_float(this, name, inds, src) &
      result(bmi_status)
   class(bmi_swan), intent(inout) :: this
   character (len=*), intent(in) :: name
   integer, intent(in) :: inds(:)
   real, intent(in) :: src(:)
   integer :: bmi_status

   bmi_status = BMI_SUCCESS
   end function swan_set_at_indices_float

   ! Set double values at particular locations.
   function swan_set_at_indices_double(this, name, inds, src) &
       result(bmi_status)
   class(bmi_swan), intent(inout) :: this
   character (len=*), intent(in) :: name
   integer, intent(in) :: inds(:)
   double precision, intent(in) :: src(:)
   integer :: bmi_status

   bmi_status = BMI_SUCCESS
   end function swan_set_at_indices_double


   !! A non-BMI procedure for model introspection.
   !subroutine print_model_info(this)
   !  class (bmi_prms_surface), intent(in) :: this
   !
   !  call print_info(this%model)
   !end subroutine print_model_info

 end module bmiswan
