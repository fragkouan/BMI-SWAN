!***********************************************************************

  module m_swan

    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    character (len=80):: basename

    type :: t_wlev
      integer :: arr_size
    end type t_wlev

    type :: t_cur
      integer :: x_comp_arr_size
      integer :: y_comp_arr_size
    end type t_cur

    type :: t_thetis
      integer :: grid_size
    end type t_thetis


    type :: swan_model
      type(t_wlev) :: water_elev
      type(t_cur) :: cur_vel
      type(t_thetis) :: thetis_model
      !type(t_output_var) :: hs_var
      ! type(t_output_var) :: dir
      ! type(t_output_var) :: qb
      ! type(t_output_var) :: wlen
      integer :: t_step_last
      real*8 :: t_start, t_end, dt
      character (len=14):: coupled_status
      integer :: output_var_size

    end type swan_model

  contains


  subroutine get_basename(config_file)
    !!!
    ! Get the basename, i.e. the name of the INPUT file without '.swn'
    ! Inputs:
    ! config_file : CHARACTER; The filepath of the SWAN configuration
    !               file
    ! Returns:
    ! None
    ! Variables:
    ! no : INTEGER; The number of charactes the config file has without
    !      the file ending
    !!!
    character (len=*), intent(in) :: config_file
    integer :: no

    ! Get the number of characters till the character '.'
    no = scan(trim(config_file), ".", BACK= .true.)
    ! Get the name of the file without '.swn'
    basename = config_file(1:no)

  end subroutine get_basename

  subroutine get_last_time_step(model)
    type(swan_model), intent(out) :: model

    call SwanGetLastTimeStep(model%t_step_last)

    return
  end subroutine get_last_time_step

  subroutine set_last_time_step(model, t_step_last)
    type(swan_model), intent(out) :: model
    integer, intent(in) :: t_step_last

    model%t_step_last = t_step_last
    call SwanSetLastTimeStep(model%t_step_last)

    return
  end subroutine set_last_time_step

  subroutine set_input_field(model, arr, field_name)
    type(swan_model), intent(inout) :: model
    real, intent(in), dimension(:) :: arr
    character, intent(in) :: field_name*(*)
    integer :: arrsize

    if (field_name=='sea_water_surface__elevation') then
      arrsize = model%water_elev%arr_size
    else if (field_name=='sea_water_flow__x_component_of_velocity') then
      arrsize = model%cur_vel%x_comp_arr_size
    else if (field_name=='sea_water_flow__y_component_of_velocity') then
      arrsize = model%cur_vel%y_comp_arr_size
    end if

    call SwanSetInputField(arr, arrsize, field_name)

    return
  end subroutine set_input_field

  subroutine get_input_field_size(model, grid, size)
    !!!
    ! Get the array size of the input fields. The size is defined from
    ! the command of INPgrid in the SWAN configuration file
    ! Inputs :
    ! grid : Integer containing the grid identifier of the variable
    !
    ! Outputs:
    ! model : The BMI-SWAN model
    ! size  : Integer containing the number of points, i.e. the array
    !         size of the input variable according to the INPgrid comm-
    !         and
    !
    ! Variables:
    ! igr1 : Integer containing the SWAN variable/grid internal identif-
    !        ier
    !!!
    type(swan_model), intent(out):: model
    integer, intent(in):: grid
    integer, intent(out):: size
    integer :: igr1 !Pointer for input field in SWAN

    !! Convert BMI grid ID's to SWAN ones
    ! If water elevation, a.k.a. eta, a.k.a. WLEV
    if (grid==0) then
      igr1 = 7

    ! If u velocity, a.k.a. the first velocity component for CUR, a.k.a.
    ! VX
    else if (grid==1) then
      igr1 = 2

    ! If v velocity, a.k.a. the 2nd velocity component for CUR, a.k.a.
    ! VY
    else if (grid==2) then
      igr1 = 3

    end if

    ! Get the size of the input variable, i.e. their number of points
    call SwanGetInputFieldSize(igr1, size)

    !! Pass the array size into the class
    ! If water elevation, a.k.a. eta, a.k.a. WLEV
    if (grid==0) then
      model%water_elev%arr_size=size

    ! If u velocity, a.k.a. the first velocity component for CUR, a.k.a.
    ! VX
    else if (grid==1) then
      model%cur_vel%x_comp_arr_size = size

    ! If v velocity, a.k.a. the 2nd velocity component for CUR, a.k.a.
    ! VY
    else if (grid==2) then
      model%cur_vel%y_comp_arr_size = size
    end if

    return
  end subroutine get_input_field_size



  subroutine get_thetis_coupling_status(model)
    !!!
    ! Get the coupling between SWAN and Thetis. The possible status are:
    ! 'Fully coupled'; 'Thetis-to-SWAN'; 'SWAN-to-Thetis'; and 'No cou-
    ! pling'
    ! Inputs:
    ! None
    !
    ! Outputs:
    ! model : The BMI-SWAN model
    !
    ! Class variables:
    ! model%coupled_status : Character containing the coupling status
    !                        between SWAN and Thetis
    !!!
    type(swan_model), intent(out):: model

    call SwanGetThetisCouplingStatus(model%coupled_status)

  end subroutine get_thetis_coupling_status



  subroutine get_thetis_grid(model)
    !!!
    ! Inputs:
    ! None
    !
    ! Outputs:
    ! model : The BMI-SWAN model
    !
    ! Class variables:
    ! model%thetis_model%grid_size :
    !!!     
    type(swan_model), intent(out) :: model

    call SwanGetThetisGrid(model%thetis_model%grid_size)

    return
  end subroutine get_thetis_grid

  subroutine set_output_field_size(model)
    type(swan_model), intent(out) :: model

    ! The size of the output variables' arrays
    model%output_var_size = model%thetis_model%grid_size

    return
  end subroutine set_output_field_size

  subroutine get_output_field(name, dest)
    character, intent(in) :: name*(*)
    real, intent(inout) :: dest(:)


    if (name=='sea_surface_water_wave__height') then
      call SwanGetOutputField("HS", dest)
    else if (name=='sea_surface_water_wave__direction') then
      call SwanGetOutputField("DIR", dest)
    else if (name=='sea_surface_water_wave__wavelength') then
      call SwanGetOutputField("WLEN", dest)
    else
      call SwanGetOutputField("QB", dest)
    end if

    return
  end subroutine get_output_field

  subroutine convert_coupling_stat_to_int(stat, int_stat)
    character, intent(in) :: stat*(*)
    integer, intent(inout) :: int_stat(:)


    if (stat=='Fully coupled') then
      int_stat = 2
    else if (stat=='Thetis-to-SWAN') then
      int_stat = 1
    else if (stat=='SWAN-to-Thetis') then
      int_stat = 0
    else
      int_stat = -1
    end if

    return
  end subroutine convert_coupling_stat_to_int

  subroutine get_coupling_timestep(dt)
    double precision, intent(inout) :: dt(:)

    call SwanGetCouplingTimestep(dt)

    return
  end subroutine get_coupling_timestep


  subroutine get_simulation_times(model)
    !!!
    ! Get the start time, end time and timestep of the simulation
    ! Inputs:
    ! model :
    !
    ! Returns:
    ! model :
    !
    ! Varibales:
    ! None
    !!!
    type(swan_model), intent(inout) :: model

    ! Get the start, end times and the computationsl timestep
    call SwanGetSimulationTimes(model%t_start, &
                                model%t_end, &
                                model%dt)

    return

  end subroutine get_simulation_times









  ! subroutine rename_file(input_file, output_file)
  !   character (len=*), intent(in) :: input_file, output_file
  !   ! type(swan_model), target, intent(out) :: model
  !   integer :: status
  !
  !   status = rename( input_file, output_file)
  !
  ! end subroutine rename_file




  end module m_swan
