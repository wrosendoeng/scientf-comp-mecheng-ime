program trajectory_4dof_simulator
    use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL
    use constants, only: PI => CST_PI_DP
    use mpmtm

    implicit none

    character(len=fnl) :: input_launch, input_environment, input_propellant, input_prodas, output_launch
    
    integer(intlength) :: i, launchunit, envunit, propunit, prodasunit, resultunit, rc1, rc2, rc3, rc4, rc5
    
    real(wp) :: x_pos, y_pos, z_pos, time_step, muzzle_vel, twist, elev_angle, az_angle, initial_center_gravity, & 
    initial_inertia_moment, initial_mass, final_center_gravity, final_inertia_moment, final_mass, ref_diameter
    
    real(wp) :: time, muzzle_setup(16), env_data(14), aero_coefs(30,19), prop_time(1402), mass_flow_rate(1402), &
    boat_diameter, initial_position(3), initial_velocity(3), initial_yaw_repose(3), initial_muzzle_spin, array_muzzle(13)

    real(wp) :: propellant_data(1402,2), propellant_mass, real_mass, p_sea_level, temp_sea_level,univ_gas_const, &
    heat_cap_ratio, accel_gravity, temp_gradient, earth_ang_vel, earth_radius, rj_latitude, suth_visc_const, & 
    suth_temp_const, magnus_factor, drag_factor, initial_time

    ! Inputs from command-line arguments 
    call get_command_argument(1,input_launch)       ! Muzzle parameters
    call get_command_argument(2,input_environment)  ! Environmental parameters
    call get_command_argument(3,input_prodas)       ! Propellant parameters
    call get_command_argument(4,input_propellant)   ! Launch result
    call get_command_argument(5,output_launch)      ! Launch result

    ! Open files to read and write
    open(newunit=launchunit, file=trim(input_launch), action='read', iostat=rc1)
    open(newunit=envunit, file=trim(input_environment), action='read', iostat=rc2)
    open(newunit=prodasunit, file=trim(input_prodas), action='read', iostat=rc3)
    open(newunit=propunit, file=trim(input_propellant), action='read', iostat=rc4)
    open(newunit=resultunit, file=trim(output_launch), action='write', iostat=rc5)

    if (rc1 .ne. 0 .or. rc2 .ne. 0 .or. rc3 .ne. 0 .or. rc4 .ne. 0 .or. rc5 .ne. 0) then     
        print *, "At least one file was read/written wrongly."
        stop
    end if

    ! Read data from command-line arguments
    muzzle_setup = read_muzzle_setup()
    env_data = read_env_data()
    aero_coefs = read_prodas()
    propellant_data = read_propellant()

    ! Importing properties from initial firing conditions 
    x_pos = muzzle_setup(1)
    y_pos = muzzle_setup(2)
    z_pos = muzzle_setup(3)
    time_step = dble(muzzle_setup(4))
    muzzle_vel = muzzle_setup(5)
    twist = muzzle_setup(6)
    elev_angle = muzzle_setup(7)
    az_angle = muzzle_setup(8)
    initial_inertia_moment = muzzle_setup(9)
    initial_center_gravity = muzzle_setup(10)
    final_inertia_moment = muzzle_setup(11)
    final_center_gravity = muzzle_setup(12)
    initial_mass = muzzle_setup(13)
    final_mass = muzzle_setup(14)
    ref_diameter = muzzle_setup(15)
    boat_diameter = muzzle_setup(16)

    ! Importing data from static test
    prop_time = propellant_data(:,1)
    mass_flow_rate = propellant_data(:,2)
    propellant_mass = simpson3(prop_time,mass_flow_rate,1402) 
    real_mass = final_mass + propellant_mass

    ! Initial system conditions:
    initial_position = (/x_pos,y_pos,z_pos/)

    initial_velocity = (/ &
        muzzle_vel*cos(elev_angle*PI/1.8d2)*cos(az_angle*PI/1.8d2), &
        muzzle_vel*sin(elev_angle*PI/1.8d2)*cos(az_angle*PI/1.8d2), &
        muzzle_vel*sin(az_angle*PI/1.8d2) &
    /)

    initial_yaw_repose = 0.0d0
    
    initial_muzzle_spin = 2.0d0*PI*muzzle_vel/(twist*ref_diameter)

    ! Constructing an array that includes all the necessary data to the MPMTM
    array_muzzle = (/ initial_position, initial_velocity, initial_yaw_repose, &
    real_mass, initial_inertia_moment, initial_center_gravity, initial_muzzle_spin/)

    initial_time = 0.0d0

    call rk4_mpmtm(initial_time,array_muzzle,time_step) !,muzzle_setup,env_data,aero_coefs,propellant_data)

    close(launchunit)
    close(envunit)
    close(prodasunit)
    close(propunit)
    close(resultunit)

contains

    function read_muzzle_setup() result(muzzle_setup)

        real(wp) :: muzzle_setup(16)

        read(launchunit,*) ! # x_position            # y_position
        read(launchunit,*) x_pos, y_pos
        read(launchunit,*) ! # z_position            # time_step
        read(launchunit,*) z_pos, time_step
        read(launchunit,*) ! # muzzle velocity       # twist
        read(launchunit,*) muzzle_vel, twist
        read(launchunit,*) ! # elevation angle       # azimuthal angle
        read(launchunit,*) elev_angle, az_angle
        read(launchunit,*) ! # Ix0                   # CG0
        read(launchunit,*) initial_inertia_moment, initial_center_gravity
        read(launchunit,*) ! # Ix1                   # CG1
        read(launchunit,*) final_inertia_moment, final_center_gravity
        read(launchunit,*) ! # initial mass          # final mass
        read(launchunit,*) initial_mass, final_mass
        read(launchunit,*) ! # reference diameter    # boattail diameter
        read(launchunit,*) ref_diameter, boat_diameter

        muzzle_setup = (/x_pos, y_pos, z_pos, time_step, muzzle_vel, twist, elev_angle, az_angle, &
        initial_inertia_moment, initial_center_gravity, final_inertia_moment, final_center_gravity, &
        initial_mass, final_mass, ref_diameter, boat_diameter/)

    end function read_muzzle_setup

    function read_env_data() result(env_data)
        
        real(wp) :: env_data(14)

        read(envunit,*) !# pressure at sea level     # temperature at sea level
        read(envunit,*) p_sea_level, temp_sea_level
        read(envunit,*) ! # universal gas constant    # heat capacity ratio
        read(envunit,*) univ_gas_const, heat_cap_ratio
        read(envunit,*) ! # acceleration of gravity   # temperature gradient
        read(envunit,*) accel_gravity, temp_gradient
        read(envunit,*) ! # earth angular velocity    # earth radius
        read(envunit,*) earth_ang_vel, earth_radius
        read(envunit,*) ! # Rio de Janeiro latitude   # azimuthal angle
        read(envunit,*) rj_latitude, az_angle
        read(envunit,*) ! # Sutherland's visc. const. # Sutherland's temp. const.
        read(envunit,*) suth_visc_const, suth_temp_const
        read(envunit,*) ! # magnus force factor       # drag force factor
        read(envunit,*) magnus_factor, drag_factor

        env_data = (/p_sea_level, temp_sea_level,univ_gas_const, heat_cap_ratio, accel_gravity, temp_gradient, &
        earth_ang_vel, earth_radius, rj_latitude, az_angle, suth_visc_const, suth_temp_const, magnus_factor, drag_factor/)

    end function read_env_data

    function read_prodas() result(aero_coefs)

        real(wp) :: aero_coefs(30,19)

        read(prodasunit,*)
        do i = 1, 30, 1
            read(prodasunit,*) aero_coefs(i,:)
        end do

    end function read_prodas

    function read_propellant() result(propellant_data)

        real(wp) :: propellant_data(1402,2)
        
        read(propunit,*)
        do i = 1, 1402, 1
            read(propunit,*) propellant_data(i,:)
        end do

    end function read_propellant

    function simpson3(x,y,n) result(integral_simpson3)
        integer(intlength) :: n
        real(wp) :: isr, ispar, isimpar, x(n), dx, y(n), integral_simpson3

        isr = y(1) + y(n)
        ispar = 0.0d0; isimpar = ispar
        dx = abs(y(2) - y(1))/(n - 1)

        do i = 2, n-1, 1
            if(mod(i,2) == 0) then
                ispar = ispar + y(i)
            else
                isimpar = isimpar + y(i)
            end if
        end do

        integral_simpson3 = dx/3.0d0*(isr+4.0d0*ispar+2.0d0*isimpar)

    end function simpson3

    subroutine rk4_mpmtm(x,y,dx)
            
        real(wp) :: x, dx
        real(wp), dimension(13) :: k1, k2, k3, k4, y

        write(resultunit,"(*(a15))") 'time','x','y','z','vx','vy','vz',&
        'yox','yoy','yoz','mi','Ix0','CG0','p0'

        i = 1
        do while (i <= 100000)
            write(resultunit,'(*(f15.8))') x, y
            k1 = dx*traj4DOF(x,y,muzzle_setup,env_data,aero_coefs,propellant_data)
            k2 = dx*traj4DOF(x+0.5d0*dx,y+0.5d0*k1,muzzle_setup,env_data,aero_coefs,propellant_data)
            k3 = dx*traj4DOF(x+0.5d0*dx,y+0.5d0*k2,muzzle_setup,env_data,aero_coefs,propellant_data)
            k4 = dx*traj4DOF(x+1.0d0*dx,y+1.0d0*k3,muzzle_setup,env_data,aero_coefs,propellant_data)

            y = y + (k1+2.0d0*(k2+k3)+k4)/6.0d0
            if (y(2) <= 0.0d0) then
                stop
            else
                x = x + dx
                i = i + 1
            end if	
        end do

    end subroutine rk4_mpmtm

end program trajectory_4dof_simulator