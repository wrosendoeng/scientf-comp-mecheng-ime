program main
	use parameters, only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL ! double-precision
	use constants

	implicit none
	character(PARM_SCL) :: iname, oname
	integer(intlength) :: i, j, k

	! Read command-line interface for input 
	call get_command_argument(1,iname)	!iname = "properties.txt"
	! Read command-line interface for output
	call get_command_argument(2,oname)	!oname = "results_mdf.txt"		
	! Read thermal properties from txt file
	call therm_properties

	allocate(x(nodesx))
	allocate(y(nodesy))
	allocate(T(nodesx*nodesy))

	infdx = plate_length/(nodesx-1)
	infdy = plate_width/(nodesy-1)

	do i = 1, nodesx, 1
		x(i+1) = x(i) + (nodesx-1)*infdx
	end do

	do j = 1, nodesy, 1
		y(j+1) = y(j) + (nodesy-1)*infdy
	end  do

	do i = 1, nodesx, 1
		do j = 1, nodesy, 1
		if (i == 1) then
			T(i,j+1) =  T(i,j) - 20*hx/K 
		else if (j == 1) then
			T(i+1,j) = T(i,j)
		else if (i == nx) then
			T(i,j) = 300
		else if (j == ny) then
			T(i,j) = T(i,j-1) - h*hx(T(i,j)-310)
		else 
			T(i,j) = 0.5*(hx**2*hy**2)/(hx**2+hy**2)*((T(i+1,j)-T(i-1,j))/hx**2+(T(i,j+1)+T(i,j-1))/hy**2-g/K)
		end do
	end do

end program main

contains

subroutine therm_properties(input_name)
	integer(intlength) :: funit
	real(wp) :: nodesx, nodesy, qcond, condterm, conv, Tinf, Tamb, plate_length, plate_width

	open(new_unit=funit,status='old',file=trim(input_name), &
	form='formatted',access='sequential',position='rewind') 
	
	read(funit,*)
	read(funit,*) qcond, condterm, conv
	read(funit,*)
	read(funit,*) Tinf, Tamb
	read(funit,*)
	read(funit,*) plate_length, plate_width
	read(funit,*)
	read(funit,*) nodesx, nodesy

end subroutine therm_properties

subroutine 