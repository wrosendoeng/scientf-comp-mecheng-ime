program main
	use parameters, wp -> dp
	use nodes
	use lapack
	use fgsl
	implicit none
	real(wp) :: nx, ny
	real(wp) :: q = 20.0, k = 600.0, tinf = 310, tamb = 300, base = 6, l = 3, h = 350 
	integer(intlength) :: i, j

	! call get_command_argument(1,arg1)			


	
	type(node), pointer :: nodes(:)
	real(wp), allocatable, target :: x(:), y(:), T(:)

	allocate(nodes(nx,nz))
	allocate(x(nx))
	allocate(y(ny))
	allocate(T(nx*nz))

	nodes(i,j) % x => x(i)
	nodes(i,j) % y => y(j)
	nodes(i,j) % t => temp(i*j)

	do i = 1, nx
		do j = 1, n	
	nodes(2) => x(:)
	end do
	

end program main
