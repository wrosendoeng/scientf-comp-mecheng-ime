program main
	use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL

	implicit none

	character(len=fnl) :: iname, oname		
	integer(intlength) :: divx, divy, i, info, j, k, nodesx, nodesy, nodes_total, numb_pts
	real(wp) :: thermal_conductivity, convec_constant, conv_factor, cond_factor, &
	genterm_quad, hx, hy, plate_length, plate_width, heat_flux, Temp_free_path, Temp_right_side

	! Representing nodes as derived-v_divpe
	type :: node
		real(wp), pointer :: x, y, temp ! horizontal and vertical positions & Temperature
		integer(intlength) :: numb		! Number of node in matrix(i,j)
	end type node 
	
	! Allocatable variables and Pointers
	integer(intlength), allocatable :: ipiv(:)	! Permitting permutation
	real(wp), allocatable :: x(:), y(:), temp_coefs(:,:), temp_solutions(:) ! Corresponding to A and b of Ax=b system.
	type(node), pointer :: mdfnodes(:,:)
	target :: x, y, temp_solutions

	! Read command-line interface for input 
	call get_command_argument(1,iname)	!e.g: "properties.x_squaredt_div"
	! Read command-line interface for output
	call get_command_argument(2,oname)	!e.g: "results.dat"		
	! Read thermal properties from x_squaredt_div file
	call therm_properties

	nodesx = divx + 1 ! number of nodes in x coordinates
	nodesy = divy + 1 ! number of nodes in y coordinates
	nodes_total = nodesx*nodesy
	
	allocate(mdfnodes(nodesx,nodesy), x(nodesx), y(nodesy), ipiv(nodes_total), &
	temp_coefs(nodes_total,nodes_total), temp_solutions(nodes_total))

	! Developing mesh
	call constructing_nodes
	! Creating matrix of coefficients for implicit method
	temp_solutions = 0.0e0_wp
	temp_coefs = 0.0e0_wp
	call arrange_coefficient_matrix

	! Calculating linear system with OpenBlas/LAPACK GESV (Ax = b) 
	select case (wp)
	case (8)
		call dgesv(nodes_total,1,temp_coefs,nodes_total,ipiv,temp_solutions,nodes_total,info)
	case default
		call sgesv(nodes_total,1,temp_coefs,nodes_total,ipiv,temp_solutions,nodes_total,info)
	end select

	if(info .gt. 0) then 
		write(*,*)'The diagonal element of the triangular factor of temp_coefs,'
		write(*,*)'U(',info,',',info,') is zero, so that'
		write(*,*)'temps_coefs is singular; the solution could not be computed.'
		stop
	end if

	call temperature_field

	deallocate(mdfnodes, x, y, ipiv, temp_coefs, temp_solutions)
 
	contains

	subroutine therm_properties
		integer(intlength) :: iunit, ioserror

		open(newunit=iunit,status='old',file=trim(iname), iostat= ioserror,&
		action='read',form='formatted',access='sequential',position='rewind') 

		read(iunit,*)
		read(iunit,*) heat_flux, thermal_conductivity
		read(iunit,*)
		read(iunit,*) convec_constant, genterm_quad
		read(iunit,*)
		read(iunit,*) Temp_free_path, Temp_right_side
		read(iunit,*)
		read(iunit,*) plate_length, plate_width
		read(iunit,*)
		read(iunit,*) divx, divy

		close(iunit)

	end subroutine therm_properties

	subroutine constructing_nodes()
		
		hx = plate_length/real(divx,kind=wp)
		hy = plate_width/real(divy,kind=wp)
	
		do i = 1, nodesx, 1
			x(i) = hx*real(i-1,kind=wp)
		end do
		do j = 1, nodesy, 1
			y(j) = hy*real(j-1,kind=wp)
		end do
	
		numb_pts = 0
		do i = 1, nodesx
			do j = 1, nodesy
				numb_pts = numb_pts + 1
				mdfnodes(i,j)%x => x(i) 
				mdfnodes(i,j)%y => y(j)
				mdfnodes(i,j)%temp => temp_solutions(numb_pts)
				mdfnodes(i,j)%numb = numb_pts
			end do
		end do
	end subroutine constructing_nodes
	
	subroutine arrange_coefficient_matrix()
		real(wp) :: term_quad, h_div, v_div

		mdfnodes(i,j)%numb = 0
		conv_factor = convec_constant/thermal_conductivity
		cond_factor = heat_flux/thermal_conductivity
		h_div = 1.0e0_wp/hx**2
		v_div = 1.0e0_wp/hy**2
		term_quad = h_div + v_div

		do i = 1, nodesy, 1
			do j = 1, nodesx, 1
				! BOTTOM PLATE SIDE (INSULATED)
				if (i == 1 .and. j > 1 .and. j < nodesx) then
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = 1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j-1)%numb) = 1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i+1,j)%numb) = -1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j+1)%numb) = -1.0e0_wp
					temp_solutions(mdfnodes(i,j)%numb) = 2.0e0_wp*cond_factor*hx*hy
				! LEFT-BOTTOM CORNER (INSULATED WITH HEAT FLUX)
				else if (i == 1 .and. j == 1) then
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = 2.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j+1)%numb) = -1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i+1,j)%numb) = -1.0e0_wp
					temp_solutions(mdfnodes(i,j)%numb) = cond_factor*hx
				! RIGHT-CORNER PLATE (INSULATED WITH PRESCRIBED TEMPERATURE)
				else if (j == nodesx) then
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = 1.0e0_wp
					temp_solutions(mdfnodes(i,j)%numb) = Temp_right_side
				! RIGHT-SIDE PLATE (INSULATED WITH PRESCRIBED TEMPERATURE)
				else if (i > 1 .and. i < nodesy .and. j == nodesx) then
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = 1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb-1) = -1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb-1) = -1.0e0_wp
					temp_solutions(mdfnodes(i,j)%numb) = Temp_right_side
				! LEFT PLATE SIDE (HEAT FLUX)
				else if (i > 1 .and. i < nodesy .and. j == 1) then
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = 1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j+1)%numb) = 1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i+1,j)%numb) = -1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i-1,j)%numb) = -1.0e0_wp
					temp_solutions(mdfnodes(i,j)%numb) = -cond_factor*hx
				!LEFT TOP CORNER 
				else if (i == nodesy .and. j == 1) then
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = 2.0e0_wp + conv_factor*hy
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j+1)%numb) = -1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i-1,j)%numb) = -1.0e0_wp
					temp_solutions(mdfnodes(i,j)%numb) = cond_factor*hx + conv_factor*hx*Temp_free_path
				! TOP PLATE SIDE (HEAT FLUX AND CONVECTION)
				else if (i == nodesy .and. j > 1 .and. j < nodesx) then
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = 1.0e0_wp + conv_factor*hy
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i-1,j)%numb) = -1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j-1)%numb) = 1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j+1)%numb) = -1.0e0_wp
					temp_solutions(mdfnodes(i,j)%numb) = 2.0e0_wp*cond_factor*hx + conv_factor*hy*Temp_free_path
				else if (i == nodesy .and. j == nodesx) then
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = conv_factor*hy
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb-nodesx) = -1.0e0_wp
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb-1) = 1.0e0_wp
					temp_solutions(mdfnodes(i,j)%numb) = cond_factor*hx + conv_factor*hy*Temp_free_path
				else
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb) = -2.0e0_wp*term_quad
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb+1) = h_div
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb-1) = h_div
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb+nodesx) = v_div
					temp_coefs(mdfnodes(i,j)%numb,mdfnodes(i,j)%numb-nodesx) = v_div
					temp_solutions(mdfnodes(i,j)%numb) = -genterm_quad/thermal_conductivity
				end if
			end do
		end do

	end subroutine arrange_coefficient_matrix

	subroutine temperature_field
		integer(intlength) :: ounit

		open(newunit=ounit,status='replace',file=trim(oname),action='write', &
		form='formatted',access='sequential') 

		do i = 1, nodesy, 1
			do j = 1, nodesx, 1
				write(ounit,'(i3,3X,f7.2,3X,f7.2,3X,f7.2)') mdfnodes(i,j)%numb,mdfnodes(i,j)%x*1.0e2,mdfnodes(i,j)%y*1.0e2,mdfnodes(i,j)%temp
			end do
			write(ounit,*) ' '
		end do
		close(unit=ounit)
	end subroutine temperature_field

end program main