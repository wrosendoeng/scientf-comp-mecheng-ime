program main
	use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL ! double-precision
	use module_node
	use constants

	implicit none
	character(fnl) :: iname, oname
	integer(intlength) :: i, j, k, lcorner, numb_pts, nodesx=10, nodesy=10, nodes_total
	real(wp) :: hx, hy, g = 10.0, qcond, condterm, conv, Tinf, Tamb, plate_length, plate_width
	type(node), pointer :: mdfnodes(:,:)
	real(wp), allocatable :: x(:), y(:), T(:) 
	target :: x, y, T

	! Read command-line interface for input 
	call get_command_argument(1,iname)	!iname = "properties.txt"
	! Read command-line interface for output
	call get_command_argument(2,oname)	!oname = "results.dat"		
	! Read thermal properties from txt file
	call therm_properties()

	hx = plate_length/real(nodesx-1,kind=wp)
	hy = plate_width/real(nodesy-1,kind=wp)
	nodes_total = nodesx*nodesy
	
	allocate(mdfnodes(nodesx,nodesy),x(nodesx),y(nodesy),T(nodes_total))

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
			mdfnodes(i,j)%temp => T(numb_pts)
			mdfnodes(i,j)%numb = numb_pts
		end do
	end do

	do i = 1, nodesx, 1
		mdfnodes(i,1)%temp = 0.0e4
		mdfnodes(i,nodesy)%temp = 3.0e2
	end do
	
	do j = 1, nodesy, 1
		T(j+1) = T(j) - qcond*hx/condterm
	end do

	lcorner = (nodesx-1)*nodesy
	do k = lcorner, nodes_total, 1
		T(k) = T(k-nodesy) - conv*hx*(T(k)-Tinf)
	end do

	do j = 2, nodesy-1, 1
		do i = 2, nodesx-1, 1
 			mdfnodes(i,j)%temp = 0.5*(hx**2*hy**2)*((mdfnodes(i+1,j)%temp+mdfnodes(i-1,j)%temp)/hx**2&
			+(mdfnodes(i,j+1)%temp+mdfnodes(i,j-1)%temp)/hy**2-g/condterm)/(hx**2+hy**2)
		end do
	end do
	
	call temperature_field()
	
	contains

	subroutine therm_properties()
		integer(intlength) :: iunit

		open(newunit=iunit,status='old',file=trim(iname), action='read',form='formatted',access='sequential',position='rewind') 
		
		read(iunit,*)
		read(iunit,*) qcond, condterm, conv
		read(iunit,*)
		read(iunit,*) Tinf, Tamb
		read(iunit,*)
		read(iunit,*) plate_length, plate_width

		close(iunit)

	end subroutine therm_properties

	subroutine temperature_field()
		integer(intlength) :: ounit
		real(wp), allocatable :: x(:), y(:), T(:)

		open(newunit=ounit,status='replace',file=trim(oname),action='write',form='formatted',access='sequential',position='rewind') 

		allocate(x(nodesx),y(nodesy),T(nodes_total))

		do j = 1, nodesy, 1
			do i = 1, nodesx
				write(ounit,*) mdfnodes(i,j)%numb, mdfnodes(i,j)%x, mdfnodes(i,j)%y, mdfnodes(i,j)%temp
			end do
			write(ounit,*) ''
		end do

		close(ounit)

		deallocate(mdfnodes,x,y,T)
		
	end subroutine temperature_field

end program main