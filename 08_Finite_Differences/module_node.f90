module module_node
	use parameters,  only: wp => PARM_DP, intlength => PARM_SI, fnl => PARM_SCL
	implicit none

	type :: node
		real(wp), pointer :: x, y, temp
		integer(intlength) :: numb
	end type node 

end module module_node