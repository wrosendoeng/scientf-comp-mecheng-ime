module nodes
	use parameters,  only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL
	implicit none
	
	contains

	type :: node
		real(wp), pointer :: x(3), y(3), temp(9)
		! integer(intlength) :: numb
	end type node 

end module nodes