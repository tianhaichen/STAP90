
! ---------------------------------------------------------------------
! -                          E F E P                                  -
! -               Explicit Finite Element Program                     -
! -              for high velocity impact analysis                    -
! -                                                                   -
! -          Copyright (C) Xiong Zhang                                -
! -                        Computational Dynamics Group               -
! -                        School of Aerospace                        -
! -            (2005)      Tsinghua Univerity                         -
! -                                                                   -
! - Program features:                                                 -
! -   1. Explicit time integration with variable step size.           -
! -   2. Hexahedral element with one point gauss qudrature.           -
! -   3. Hourglass control (standard and Flangan-Belytschko methods). -
! -   4. Material model (elasticity, elastic-perfectly plastic,       -
! -      isotropic hardening and Johnson-Cook plasticity models).     -
! -   5. Mie-Gruneisen equation of state.                             -
! -   6. Export results to TecPlot for creating and animation and     -
! -      time history curve.                                          -
! -   7. Macro based programming.                                     -
! -   8. Impact on a rigid plane                                      -
! -   9. External nodal load                                          -
! -  10. Jaumman rate of stress                                       -
! -                                                                   -
! ---------------------------------------------------------------------

	program main
	use DataIn
	use DataOut 
	use FFI
	implicit none
	real:: time_begin, time_end
!
	call cpu_time( time_begin )
!	
	call InputPara()
!
	call cpu_time( time_end )
	print *, '** Time for preprocessing is', time_end - time_begin, ' seconds'

	call cpu_time( time_begin )
	print *, 'solving...'

	call Integration()	
	
	call cpu_time( time_end )
	print *, '** Time for computation is  ', time_end - time_begin, ' seconds'

	close(ior)
	close(iow1)
	close(iow2)

	close(iores)

	end program main

	subroutine integration()
! -------------------------------------------------------------------
! -                                                                 -
!   Integrate momentum equations
! -                                                                 -
! -------------------------------------------------------------------
	use Simulation
	use ElementData
	use ParticleData
	use DataOut
	use TimeFunctionData
	implicit none

	real(8) :: prt = 0.0
	real(8) :: plt = 0.0

	real(8) :: tf

	do while (CurrentTime .le. EndTime)

		DTk  = DTk1
		DTk1 = 1.0d6

!	Calculate kinetic energy
		call KineticE()

!	Time function at the current time step
		tf = CurrentTMFunction(CurrentTime)		
			
		call FEForce(tf)

		call UpdateFEGeometry()
		
		if (CurrentTime.ge.prt) then
			prt = prt + OutTimeStep
			call OutCurve(CurrentTime)
			call OutAnim(CurrentTime)
		end if

		if (CurrentTime.ge.plt) then
			plt = plt + RepTimeStep
			write(*,100) istep, CurrentTime, dtk1, EleDistortion, EngKinetic
100			format(1x, "Step=", i6, 1p, "  T=", e9.3, "  DT=", e9.3,  &
					   "  EDist=", e9.3, "  K.E.=", e9.3)
		end if

		istep = istep+1
		CurrentTime = CurrentTime + DTk1

	end do

	end subroutine integration
