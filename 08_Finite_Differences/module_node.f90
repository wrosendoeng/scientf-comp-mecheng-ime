module nodes
	use parameters
	implicit none
	contains

	type(node)
		real(wp), pointer :: x, y, temp
		integer(intlength) :: numb
	end type
	
	type(node), pointer :: nodes
end module nodes
