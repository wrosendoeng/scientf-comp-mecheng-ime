program trajectory_4dof_simulator
    use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL
    use constants, only: PI => CST_PI_DP
    use mpmtm

    implicit none

    character(len=fnl) :: input_launch, input_environment, input_propellant, input_prodas, output_launch
    integer(intlength) :: i, launchunit, envunit, propunit, prodasunit, resultunit
    real(wp) :: muzzle_vel, twist, elev_angle, az_angle, initial_center_gravity, initial_inertia_moment, &
    initial_mass, final_center_gravity, final_inertia_moment, final_mass, ref_diameter, boat_diameter, initial_position(3), &
    initial_velocity(3), initial_yaw_repose(3), initial_muzzle_spin, array_muzzle(13), time, env_data(14), aero_coefs(30,7)

    ! Inputs from command-line arguments 
    call get_command_argument(1,input_launch)       ! Muzzle parameters
    call get_command_argument(2,input_environment)  ! Environmental parameters
    call get_command_argument(3,input_prodas)   ! Propellant parameters
    call get_command_argument(4,input_propellant)      ! Launch result
    call get_command_argument(5,output_launch)      ! Launch result

    ! Open files to read and write
    open(newunit=launchunit, file=trim(input_launch), action='read', iostat=rc1)
    open(newunit=envunit, file=trim(input_environment), action='read', iostat=rc2)
    open(newunit=prodasunit, file=trim(input_prodas), action='read', iostat=rc3)
    open(newunit=propunit, file=trim(input_propellant), action='read', iostat=rc4)
    open(newunit=resultunit, file=trim(input_propellant), action='write', iostat=rc5)

    if (rc1 .ne. 0 .or. rc2 .ne. 0 .or. rc3 .ne. 0 .or. rc4 .ne. 0 .or. rc5 .ne. 0) then     
        print *, "At least one file was read/written wrongly."
        stop
    end if

    ! Read data from command-line arguments
    call read_muzzle_setup
    call read_environmental_properties(env_data)
    call read_prodas(aero_coefs)
    call read_propellant

    propellant_mass = simpson3(time,mass_flow_rate,1402) 
    
    real_mass = final_mass + propellant_mass

    function trapz(time,y,n,dt)

    ! Initial system conditions:
    initial_position = 0.0d0

    initial_velocity = (/
        muzzle_vel*cos(elev_angle*PI/1.8d2)*cos(az_angle*PI/1.8d2), &
        muzzle_vel*sin(elev_angle*PI/1.8d2)*cos(az_angle*PI/1.8d2), &
        muzzle_vel*sin(az_angle*PI/1.8d2) &
    /)

    initial_yaw_repose = 0.0d0

    initial_muzzle_spin = 2.0d0*PI*muzzle_vel/(twist*ref_diameter)

    ! Constructing an array that includes all the necessary data to the MPMTM
    array_muzzle = (/ initial_position, initial_velocity, initial_yaw_repose, &
    initial_mass, initial_inertia_moment, initial_center_gravity, initial_muzzle_spin /)

    call rk4_mpmtm(initial_time,solver,time_step)

    deallocate(system_time)

    close(launchunit)
    close(envunit)
    close(prodasunit)
    close(propunit)
    close(resultunit)

contains

subroutine read_muzzle_setup

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

end subroutine read_muzzle_setup

subroutine read_environmental_properties(env_data)
    
    real(wp), intent(out) :: env_data(14)

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

    env_data =(/p_sea_level, temp_sea_level,univ_gas_const, heat_cap_ratio, accel_gravity, temp_gradient, &
    earth_ang_vel, earth_radius, rj_latitude, az_angle, suth_visc_const, suth_temp_const, magnus_factor, drag_factor/)

end subroutine read_environmental_properties

subroutine read_prodas(aero_coefs)

    real, intent(out) :: aero_coefs(30,7)
    real(wp), dimension(30) :: mach_prodas, cx0, cx2, cd2, cx4, cla, cna, &
    cna3, cmag_f, cma, cma3, cma5, cmq, cmq2, cspin, cld, cxf, cxb, cpn

    read(prodasunit,*)
    do i = 1, 30, 1
        read(prodasunit,*) mach_prodas(i),cx0(i),cx2(i),cd2(i),cx4(i), &
        cla(i), cna(i),cna3(i),cmag_f(i), cma(i), cma3(i), &
        cma5(i), cmq(i), cmq2(i), cspin(i), cld(i), cld(i), cxf(i), &
        cxb(i), CPN(i)
    end do

    aero_coefs = (/mach_prodas, cx0, cxb, cd2, cla, cmag_f, cma, cspin/)

end subroutine read_prodas

subroutine read_propellant

    real(wp), dimension(1043) :: time, mass_flow_rate

    read(propunit,*)
    do i = 1, 30, 1
        read(propunit,*) time(i), mass_flow_rate(i)
    end do

end subroutine read_propellant

function simpson3(x,y,n) result(integral_simpson3)
    integer(intlength) :: n
    real(wp) :: isr, ispar, isimpar, x(n), dx, y(n)

    isr = y(1) + y(n)
    ispar = 0.0d0; isimpar = ispar
    dt = abs(y(2) - y(1))/(n - 1)

    do i = 2, n-1, 1
        if(mod(i,2) == 0) then
            ispar = ispar + y(i)
        else
            isimpar = isimpar + y(i)
        end if
    end do

    integral_trapezoidal = dx/3.0d0*(isr+4.0d0*ispar+2.0d0*isimpar)

end function simpson3

subroutine rk4(x,y,dx)
        
    real(wp) :: x, dx
    real(wp), dimension(6) :: k1, k2, k3, k4, y

    write(unidadeescrita,*) 'time     x     y     z     vx     vy     vz     yox     yoy     yoz     mi     
    Ix0     CG0     p0'

    ! function traj4DOF(time,muzzle_input,environmental_properties,propellant_data) result(muzzle_output)
    do while (i <= 100000 .or. y(2) <= 0.0d0)
        write(unidadeescrita,'(f8.4,3x,f15.8)') time, y
        k1 = dx*traj4DOF(x,y)
        k2 = dx*traj4DOF(x+0.5d0*dx,y+0.5d0*k1)
        k3 = dx*traj4DOF(x+0.5d0*dx,y+0.5d0*k2)
        k4 = dx*traj4DOF(x+1.0d0*dx,y+1.0d0*k3)

        y = y + dx/6.0d0*(k1+2.0d0*(k2+k3)+k4)
        x = x + dx	
    end do

end subroutine rk4
    
end program trajectory_4dof_simulator