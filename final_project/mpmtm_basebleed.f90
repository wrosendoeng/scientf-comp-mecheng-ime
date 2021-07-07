module mpmtm
    use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL
    use constants, only: PI => CST_PI_DP
    use fgsl

    implicit none

    contains

    function traj4DOF(time,muzzle_input,firing_data,environmental_properties,aero_coefs, &
        propellant_data) result(muzzle_output)
        
        ! Inputs and outputs
        real(wp), intent(in) :: time, firing_data(16), muzzle_input(13), environmental_properties(14), &
        aero_coefs(30,19), propellant_data(1402,2)
        real(wp) :: muzzle_output(15)

        ! Declaring variables from environmental properties text file 
        real(wp) :: p_sea_level, temp_sea_level, univ_gas_const, heat_cap_ratio, accel_gravity, &
        earth_ang_vel, earth_radius, rj_latitude, suth_temp_const, suth_visc_const, magnus_factor, &
        drag_factor

        ! Declaring variables imported from the PRODAS file
        real(wp), dimension(30) :: mach_prodas, cx0_vector, cxb_vector, cd2_vector, cla_vector, &
        cmag_f_vector, cma_vector, cspin_vector

        ! Variables used along the subroutine
        real(wp) :: x_pos, y_pos, z_pos, vx, vy, vz, vel_module, velocity(3), aex, aey, aez, yaw_repose_module, &
        yaw_repose_vector(3), proj_mass, axial_inertia, center_gravity, spin, gravity, temperature, pressure, &
        density, dyn_viscosity, mach_number, reynolds_number, cd0, cdb, cd2, cla, cmag_f, cma, cspin, coriolis_accel(3), &
        drag_accel(3), grav_accel(3), lift_accel(3), mfx, mfy, mfz, magnus_vector(3), magnus_accel(3), basebleed_accel(3), &
        ideal_injection, mass_flux, injection_parameter, acceleration(3), yox, yoy, yoz, yov(3), yaw_repose_adjust(3), &
        mass_properties(3), spin_rate, az_angle, temp_gradient, vel_sound, inj, axial_inertia_rate, center_gravity_rate
        
        ! Variables imported from setup 
        real(wp) :: initial_inertia_moment, initial_center_gravity, final_inertia_moment, final_center_gravity, &
        initial_mass, final_mass, ref_diameter, boat_diameter

        ! Assigning variables according to geometry properties
        initial_inertia_moment = firing_data(9)
        initial_center_gravity = firing_data(10)
        final_inertia_moment = firing_data(11)
        final_center_gravity = firing_data(12)
        initial_mass = firing_data(13)
        final_mass = firing_data(14)
        ref_diameter = firing_data(15)
        boat_diameter = firing_data(16)

        ! Assigning variables according to enviromental properties
        p_sea_level = environmental_properties(1)
        temp_sea_level = environmental_properties(2)
        univ_gas_const = environmental_properties(3)
        heat_cap_ratio = environmental_properties(4)
        accel_gravity = environmental_properties(5)
        temp_gradient = -environmental_properties(6)
        earth_ang_vel = environmental_properties(7)
        earth_radius = environmental_properties(8)
        rj_latitude = environmental_properties(9)
        az_angle = environmental_properties(10)
        suth_temp_const = environmental_properties(11)
        suth_visc_const = environmental_properties(12)
        magnus_factor = environmental_properties(13)
        drag_factor = environmental_properties(14)
        
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
        gravity = accel_gravity*(1 - 0.0026*cos(-2.0d0*rj_latitude*PI/1.8d2))
            
        if (y_pos <= 1.1d04) then
            temperature = temp_sea_level + temp_gradient*y_pos
            pressure = p_sea_level*(1.0d0 + temp_gradient*y_pos/temp_sea_level)**(-accel_gravity/(temp_gradient*univ_gas_const))
        else
            temperature = 2.1665d02
            pressure = (2.2620d04)*exp(-accel_gravity/(univ_gas_const*temperature)*(y_pos-1.1d04))
        end if

        ! Free air stream density:
        density = pressure/(univ_gas_const*temperature)
        ! Sutherland's law of Dynamic Viscosity:
        dyn_viscosity = suth_visc_const*temperature**(1.5d0)/(temperature+suth_temp_const)
        ! Mach number
        vel_sound = sqrt(heat_cap_ratio*univ_gas_const*temperature)
        mach_number = vel_module/vel_sound
        ! Associated Reynolds number with respect to proj. refer. diameter:
        reynolds_number = density*ref_diameter*vel_module/dyn_viscosity   
        
        ! POLYNOMIAL FITTING FOR AERODYNAMIC COEFFICIENTS
        ! cubic-spline interpolation
        mach_prodas = aero_coefs(:,1)
        cx0_vector = aero_coefs(:,2)
        cxb_vector = aero_coefs(:,18)
        cd2_vector = aero_coefs(:,4)
        cla_vector = aero_coefs(:,6)
        cmag_f_vector = aero_coefs(:,9)
        cma_vector = aero_coefs(:,10)
        cspin_vector = aero_coefs(:,15)

        ! Drag coefficient at 0 angle of attack
        cd0 = prodas_interp(mach_prodas,cx0_vector,mach_number,size(mach_prodas))
        ! cd0 = prodas_aprox(mach_prodas,cx0_vector,mach_number,size(mach_prodas))

        ! Boat-tail drag coefficient 
        cdb = prodas_interp(mach_prodas,cxb_vector,mach_number,size(mach_prodas))
        ! cdb = prodas_aprox(mach_prodas,cxb_vector,mach_number,size(mach_prodas))

        ! Drag coefficient with yaw of repose
        cd2 = prodas_interp(mach_prodas,cd2_vector,mach_number,size(mach_prodas))
        ! cd2 = prodas_aprox(mach_prodas,cd2_vector,mach_number,size(mach_prodas))

        ! Lift coefficient
        cla = prodas_interp(mach_prodas,cla_vector,mach_number,size(mach_prodas))
        ! cla = prodas_aprox(mach_prodas,cla_vector,mach_number,size(mach_prodas))
        
        ! Magnus coefficient
        cmag_f = prodas_interp(mach_prodas,cmag_f_vector,mach_number,size(mach_prodas))
        ! cmag_f = prodas_aprox(mach_prodas,cmag_f_vector,mach_number,size(mach_prodas))

        ! Pitching moment coefficient
        cma = prodas_interp(mach_prodas,cma_vector,mach_number,size(mach_prodas))
        ! cma = prodas_aprox(mach_prodas,cma_vector,mach_number,size(mach_prodas))
        
        ! Pitching moment coefficient
        cspin = prodas_interp(mach_prodas,cspin_vector,mach_number,size(mach_prodas))
        ! cspin = prodas_aprox(mach_prodas,cspin_vector,mach_number,size(mach_prodas))
                   
        ! Coriolis effect         
        coriolis_accel = 2.0d0*earth_ang_vel*(/-vy*cos(-2.0d0*rj_latitude*PI/1.8d2)*sin(az_angle*PI/1.8d2) &
        - vz*sin(-2.0d0*rj_latitude*PI/1.8d2), vx*cos(-2.0d0*rj_latitude*PI/1.8d2)*sin(az_angle*PI/1.8d2) + &
        vz*cos(-2.0d0*rj_latitude*PI/1.8d2)*cos(az_angle*PI/1.8d2), vx*sin(-2.0d0*rj_latitude*PI/1.8d2) &
        - vy*cos(-2.0d0*rj_latitude*PI/1.8d2)*cos(az_angle*PI/1.8d2)/)    
        
        ! Drag acceleration
        drag_accel = -PI*density*ref_diameter**2.0d0*vel_module*velocity*(cd0 &
         + cd2*(drag_factor*yaw_repose_module)**2)/8.0d0/proj_mass
        
        ! Gravitational acceleration using spherical approximation        
        grav_accel = -gravity*(/x_pos/earth_radius,1.0d0 - 2.0d0*y_pos/earth_radius,z_pos/earth_radius/)
        
        ! Lift acceleration
        lift_accel = (pi*density*(ref_diameter**2.0)*cla*vel_module**2*yaw_repose_vector)/(8.0d0*proj_mass)
        
        ! Magnus acceleration
        mfx = yaw_repose_vector(2)*velocity(3) - yaw_repose_vector(3)*velocity(2)
        mfy = yaw_repose_vector(3)*velocity(1) - yaw_repose_vector(1)*velocity(3)
        mfz = yaw_repose_vector(1)*velocity(2) - yaw_repose_vector(2)*velocity(1)
        magnus_vector = (/mfx,mfy,mfz/)
        magnus_accel = -(pi*density*(ref_diameter**3)*magnus_factor*spin*cmag_f)/(8.0d0*proj_mass)*magnus_vector
            
        ! Base bleed acceleration     
        if (time <= propellant_data(ubound(propellant_data,1),1)) then
            mass_flux = prodas_interp(propellant_data(:,1), propellant_data(:,2), time, size(propellant_data,1))
        else
            mass_flux = 0.0d0
        end if

        axial_inertia_rate = (initial_inertia_moment-final_inertia_moment)*(proj_mass-initial_mass)/(initial_mass-final_mass)
        center_gravity_rate = (initial_center_gravity-final_center_gravity)*(proj_mass-initial_mass)/(initial_mass-final_mass)
    
        ideal_injection = 5.0d-3
        injection_parameter = 4.0d0*mass_flux/(pi*density*vel_module*boat_diameter**2)

        if (injection_parameter < ideal_injection) then       
            inj = injection_parameter/ideal_injection 
        else
            inj = 1.0d0
        end if

        basebleed_accel = pi*density*(ref_diameter*vel_module)**2*cdb*inj*&
        (yaw_repose_vector + velocity/vel_module*cos(yaw_repose_module))/(8.0d0*proj_mass)
                
        ! acceleration vector
        acceleration = coriolis_accel+drag_accel+grav_accel+lift_accel+magnus_accel+basebleed_accel
        
        ! yaw of repose (Stanag 4355, eq 10)
        yox = velocity(2)*acceleration(3) - velocity(3)*acceleration(2)
        yoy = velocity(3)*acceleration(1) - velocity(1)*acceleration(3)
        yoz = velocity(1)*acceleration(2) - velocity(2)*acceleration(1)
        yov = (/yox,yoy,yoz/) 
        yaw_repose_adjust = -8.0d0*axial_inertia*spin*yov/(PI*density*ref_diameter**3*cma*vel_module**4)
        ! mass flow vector:
        mass_properties = (/-mass_flux,axial_inertia_rate,center_gravity_rate/)
        ! spin rate vector:
        spin_rate = pi*density*(ref_diameter**4)*spin*vel_module*cspin/(8.0d0*axial_inertia)
        muzzle_output = (/velocity,acceleration,yaw_repose_adjust,mass_properties,spin_rate,cd0,cdb/)
        
    end function traj4DOF

    function prodas_interp(x,y,x_interp,n) result(y_interp)
        implicit none
        
        integer(intlength) :: status, n
        real(wp) :: x(n), y(n), x_interp, y_interp
        type(fgsl_interp_accel) :: acc
        type(fgsl_spline) :: cspline
    
        acc = fgsl_interp_accel_alloc()
        cspline = fgsl_spline_alloc(fgsl_interp_cspline, int(n,fgsl_size_t))
        status = fgsl_spline_init(cspline, x, y)
        y_interp = fgsl_spline_eval(cspline,x_interp,acc)
        
        call fgsl_spline_free(cspline)
        call fgsl_interp_accel_free(acc)
        
    end function prodas_interp

    function prodas_aprox(x,y,x_aprox,n) result(y_aprox)
        implicit none

        integer(intlength) :: i, j, k, n 
        real(wp), intent(in) :: x(n), y(n), x_aprox
        real(wp) :: a(5,5), b(5), g(n,5), vector(5), y_aprox

        do k = 1, n
            g(k,1) = 1.0d0
            g(k,2) = x(k)
            g(k,3) = x(k)**2
            g(k,4) = x(k)**3
            g(k,5) = x(k)**4
        end do

        do k = 1, n
            do i = 1, 5
                do j = 1 , 5
                    a(i,j) = a(i,j) + g(k,i)*g(k,j)
                end do 
                b(i) = b(i) + y(k)*g(k,i)
            end do
        end do

        call lufactorization(a,b,vector,size(a,1))
        
        y_aprox = vector(1) + vector(2)*x_aprox + vector(3)*x_aprox**2 + &
        vector(4)*x_aprox**3 + vector(5)*x_aprox**4

    end function prodas_aprox

    subroutine lufactorization(a,b,x,n)

        integer(intlength) :: aux, i, j, k, l_pivot, n, p(n)
        real(wp) :: m, pivot, troca, a(n,n), b(n), c(n), y(n)
        real(wp), intent(out) :: x(n)
    
        !Pivoting
        do i = 1, n 
            p(i) = i 
        end do
    
        do k = 1, n-1
            pivot = a(k,k)
            l_pivot = k 
            do i = (k+1),n
                if (abs(a(i,k)) > abs(pivot)) then 
                    pivot = a(i,k)
                    l_pivot = i 
                end if
            end do
            if (pivot == 0.0) exit
            if (l_pivot /= k) then
                troca = b(k)
                b(k) = b(l_pivot)
                b(l_pivot) = troca
                do j = 1, n
                    troca = a(k,j)
                    a(k,j) = a(l_pivot,j)
                    a(l_pivot,j) = troca
                end do
            end if
            do i = (k+1),n
                m = a(i,k)/a(k,k)
                a(i,k) = 0.0
                do j = (k+1),n
                    a(i,j) = a(i,j) - m*a(k,j)
                end do
                b(i) = b(i) - m*b(k)
            end do 
        end do
    
        ! c = Pb
        do i = 1, n 
            aux = p(i)
            c(i) = b(aux)
        end do
    
        ! Ly = Pb
        y = 0.0_fgsl_double
        do i = 1, n
            do j = 1, (n-1)
                y(i) = y(i) + a(i,j)*y(j)
            end do 
            y(i) = c(i) - y(i)
        end do
    
        ! Ux = y
        x = 0.0_fgsl_double
        do i = n,1,-1   
            do j = (i+1),n
                x(i) = x(i) + a(i,j)*x(j)
            end do
            x(i) = (y(i)-x(i))/a(i,i)
        end do

    end subroutine lufactorization

end module mpmtm