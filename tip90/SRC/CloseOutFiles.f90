subroutine CloseOutFiles()
! Purpose: close output files
	use module_ioport
	use module_parameter
	implicit none
	integer i

	if(Disp_flag) then
		do i=1,n_Disp_dof
			close(unit=oport_disp(i))
		end do
	endif
		
	if(Velo_flag)   then
		do i=1,n_Velo_dof
			close(unit=oport_velo(i))
		end do
	endif

	if(Acce_flag)	then
		do i=1,n_Acce_dof
			close(unit=oport_acce(i))
		end do
	endif

end subroutine CloseOutFiles