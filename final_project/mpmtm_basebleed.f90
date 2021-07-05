module mpmtm
    use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL
    use constants, only: PI => CST_PI_DP
    use fgsl

    implicit none

    contains

    function traj4DOF(time,muzzle_input,environmental_properties,aero_coefs,propellant_data) result(muzzle_output)
        ! Preparing variable for FGSL Interpolation
        integer(fgsl_int) :: status, cd0_status, cdb_status, cd2_status, cla_status, cmag_status, cma_status, cspin_status
        type(fgsl_interp_accel) :: alloc_accel, prop_accel
        type(fgsl_spline) :: cspline, prop_cspline
        
        ! Inputs and outputs
        real(wp), intent(in) :: environmental_properties(12),  muzzle_input(13), propellant_data(30,8), time
        real(wp)             :: muzzle_output(13)

        ! Declaring variables from environmental properties text file 
        real(wp) :: p_sea_level, temp_sea_level, univ_gas_const, heat_cap_ratio, accel_gravity, &
        earth_ang_vel, earth_radius, rj_latitude, suth_temp_const, suth_visc_const, magnus_factor, &
        drag_factor

        ! Declaring variables imported from the PRODAS file
        real(wp), dimension(30) :: mach_prodas, cx0_vector, cxb_vector, cd2_vector, cla_vector, cmag_f_vector, cma_vector, cspin_vector

        ! Variables used along the subroutine
        real(wp) :: x_pos, y_pos, z_pos, vx, vy, vz, vel_module, velocity(3), aex, aey, aez, yaw_repose_module, &
        yaw_repose_vector(3), proj_mass, axial_inertia, center_gravity, spin, gravity, temperature, pressure, &
        density, dyn_viscosity, mach_number, reynolds_number, cd0, cdb, cd2, cla, cmag, cma, cspin, coriolis_accel(3), &
        drag_accel(3), grav_accel(3), lift_accel(3), mfx, mfy, mfz, magnus_vector(3), magnus_accel(3), basebleed_accel(3), &
        ideal_injection, mass_flow_rate, injection_parameter, acceleration(3), yox, yoy, yoz, yov(3), yaw_repose_adjust(3), &
        mass_properties(3), spin_rate
        
        ! Assigning variables according to enviromental properties
        p_sea_level = environmental_properties(1)
        temp_sea_level = environmental_properties(2)
        univ_gas_const = environmental_properties(3)
        heat_cap_ratio = environmental_properties(4)
        accel_gravity = environmental_properties(5)
        earth_ang_vel = environmental_properties(6)
        earth_radius = environmental_properties(7)
        rj_latitude = environmental_properties(8)
        suth_temp_const = environmental_properties(9)
        suth_visc_const = environmental_properties(10)
        magnus_factor = environmental_properties(11)
        drag_factor = environmental_properties(12)
        
        ! Projectile coordinates wrt the ground:
        x_pos = muzzle_input(1)
        y_pos = muzzle_input(2)
        z_pos = muzzle_input(3)

        ! Velocity wrt the ground:
        vx = muzzle_input(4)
        vy = muzzle_input(5)
        vz = muzzle_input(6)
        velocity = (/vx,vy,vz/)
        vel_module = sqrt(vx**2+vy**2+vz**2)
        
        ! Yaw of repose:
        aex = muzzle_input(7)
        aey = muzzle_input(8)
        aez = muzzle_input(9)
        yaw_repose_vector = (/aex,aey,aez/)
        yaw_repose_module = sqrt(aex**2+aey**2+aez**2)
            
        ! Projectile mass:
        proj_mass = muzzle_input(10)
        
        ! Initial axial moment of inertia and center of gravity:
        axial_inertia = muzzle_input(11)
        center_gravity = muzzle_input(12)
        
        ! Spin rate:
        spin = muzzle_input(13)   
    
        ! Gravitational acceleration in spherical approximation: 
        gravity = accel_gravity*(1 - 2.6d-3*cos(-2.0d0*rj_latitude*PI/1.8d2))
            
        if (y_pos <= 1.1d04) then
            temperature = temp_sea_level - temp_gradient*y_pos
            pressure = p_sea_level*(1.0d0 - temp_gradient*y_pos/temp_sea_level)**(accel_gravity/temp_gradient/univ_gas_const)
        else
            temperature = 2.1665d02
            pressure = (2.2620d04)*exp(-accel_gravity/univ_gas_const/temperature*(y_pos-1.1d04))
        end if

        ! Free air stream density:
        density = pressure/univ_gas_const/temperature 
        ! Sutherland's law of Dynamic Viscosity:
        dyn_viscosity = suth_visc_const*temperature**(1.5d0)/(temperature+suth_temp_const)
        ! Mach number
        mach_number = vel/sqrt(heat_cap_ratio*univ_gas_const*temperature)
        ! Associated Reynolds number with respect to proj. refer. diameter:
        reynolds_number = density*ref_diameter*vel/dyn_viscosity   
        
        ! POLYNOMIAL FITTING FOR AERODYNAMIC COEFFICIENTS:
        ! cubic-spline interpolation:
        mach_prodas = aero_coefs(1)
        cx0_vector = aero_coefs(2)
        cxb_vector = aero_coefs(3)
        cd2_vector = aero_coefs(4)
        cla_vector = aero_coefs(5)
        cmag_f_vector = aero_coefs(6)
        cma_vector = aero_coefs(7)
        cspin_vector = aero_coefs(8)

        alloc_accel = fgsl_interp_accel_alloc()   
        cspline = fgsl_spline_alloc(fgsl_interp_cspline, 30)  
        
        ! Drag coefficient at 0 angle of attack
        cd0_status = fgsl_spline_init(cspline, mach_prodas, cx0_vector) 
        cd0 = fgsl_spline_eval(cspline,mach_number,alloc_accel)

        ! Boat-tail drag coefficient 
        cdb_status = fgsl_spline_init(cspline, mach_prodas, cxb_vector) 
        cdb = fgsl_spline_eval(cspline,mach_number,alloc_accel)
        
        ! Drag coefficient with yaw of repose
        cd2_status = fgsl_spline_init(cspline, mach_prodas, cd2_vector) 
        cd2 = fgsl_spline_eval(cspline,mach_number,alloc_accel)
        
        ! Lift coefficient
        cla_status = fgsl_spline_init(cspline, mach_prodas, cla_vector) 
        cla = fgsl_spline_eval(cspline,mach_number,alloc_accel)
        
        ! Magnus coefficient
        cmag_status = fgsl_spline_init(cspline, mach_prodas, cmag_f_vector) 
        cmag = fgsl_spline_eval(cspline,mach_number,alloc_accel)

        ! Pitching moment coefficient
        cma_status = fgsl_spline_init(cspline, mach_prodas, cma_vector) 
        cma = fgsl_spline_eval(cspline, mach_number,alloc_accel)
        
        ! Pitching moment coefficient
        cspin_status = fgsl_spline_init(cspline, mach_prodas, cspin_vector) 
        cspin = fgsl_spline_eval(cspline, mach_number,alloc_accel)
                   
        ! Coriolis effect         
        coriolis_accel = 2.0d0*earth_ang_vel*(/-vy*cos(-2.0d0*rj_latitude*PI/1.8d2)*sin(az_angle*PI/1.8d2) &
        - vz*sin(-2.0d0*rj_latitude*PI/1.8d2), vx*cos(-2.0d0*rj_latitude*PI/1.8d2)*sin(az_angle*PI/1.8d2) + &
        vz*cos(-2.0d0*rj_latitude*PI/1.8d2)*cos(az_angle*PI/1.8d2), vx*sin(-2.0d0*rj_latitude*PI/1.8d2) &
        - vy*cos(-2.0d0*rj_latitude*PI/1.8d2)*cos(az_angle*PI/1.8d2)/)    
        
        ! Drag acceleration
        drag_accel = -(pi*density*(ref_diameter**2.0d0)*(cd0 + cd2*(drag_factor*ae)**2)*vel_module*velocity)/(8.0d0*proj_mass)
        
        ! Gravitational acceleration using spherical approximation        
        grav_accel = gravity*(/-x_pos/earth_radius,-1.0d0 + 2.0d0*y_pos/earth_radius,-z_pos/earth_radius/)
        
        ! Lift acceleration
        lift_accel = (pi*density*(ref_diameter**2.0)*cla*vel_module**2*yaw_repose_vector)/(8.0d0*proj_mass)
        
        ! Magnus acceleration
        mfx = yaw_repose_vector(2)*velocity(3) - yaw_repose_vector(3)*velocity(2)
        mfy = yaw_repose_vector(3)*velocity(1) - yaw_repose_vector(1)*velocity(3)
        mfz = yaw_repose_vector(1)*velocity(2) - yaw_repose_vector(2)*velocity(1)
        magnus_vector = (/mfx,mfy,mfz/)
        magnus_accel = -(pi*density*(ref_diameter**3)*magnus_factor*spin*cmag_f)/(8.0d0*proj_mass)*magnus_vector
            
        ! Base bleed acceleration
        ! POLYNOMIAL FITTING FOR HOT MASS FLUX INJECTION:   
        ! list_cols = ['Time (s)', 'mass_flow_rate/dt (kg/s)', 'Mach', 'Vt (m/s)', 'Tt (K)']
        ! In that case, propellant data == v2013B1120        
        if (time <= propellant_data(ubound(propellant_data,1),1)) then
            ideal_injection = 5.0d-3
            prop_accel = fgsl_interp_accel_alloc()
            prop_cspline = fgsl_spline_alloc(fgsl_interp_cspline,ubound(propellant_data,1))
            prop_status = fgsl_spline_init(prop_cspline, propellant_data(:,1), propellant_data(:,2))    
            mass_flow_rate = fgsl_spline_eval(prop_cspline,time,prop_accel)
            injection_parameter = 4.0d0*mass_flow_rate/(pi*density*vel_module*boat_diameter) 
        else
            mass_flow_rate = 0.0d0
            injection_parameter = 1.0d0
        end if
                    
        basebleed_accel = pi*density*(ref_diameter*vel_module)**2*cdb*injection_parameter*(velocity*cos(yaw_repose_module)/vel_module+aev)/(8*proj_mass)
                
        ! acceleration vector
        acceleration = coriolis_accel+drag_accel+grav_accel+lift_accel+magnus_accel+basebleed_accel
        
        ! yaw of repose (Stanag 4355, eq 10)
        yox = velocity(2)*acceleration(3) - velocity(3)*acceleration(2)
        yoy = velocity(3)*acceleration(1) - velocity(1)*acceleration(3)
        yoz = velocity(1)*acceleration(2) - velocity(2)*acceleration(1)
        yov = (/yox,yoy,yoz/) 
        yaw_repose_adjust = -8.0d0*axial_inertia*spin*yov/(pi*density*ref_diameter**3*cma*vel_module**4)
        ! mass flow vector:
        mass_properties = (/-mass_flow_rate,(axial_inertia0-axial_inertia1)*(m-m0)/(m0-m1),(CG0-CG1)*(m-m0)/(m0-m1)/)
        ! spin rate vector:
        spin_rate = pi*density*(ref_diameter**4)*spin*vel_module*cspin/(8.0d0*axial_inertia)
        muzzle_output = (/velocity,acceleration,yaw_repose_adjust,mass_properties,spin_rate/)  

        call fgsl_spline_free(cspline)     
        call fgsl_interp_accel_free(accel)
        call fgsl_spline_free(prop_cspline)     
        call fgsl_interp_accel_free(prop_accel)
        
    end function traj4DOF

end module mpmtm