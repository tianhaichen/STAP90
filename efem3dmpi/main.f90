
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
      use mpi
	use commnodes
       implicit none

	integer ierr                !addmpi
	real(8):: time_begin, time_end
!addmpi
  	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
      time_begin=0
      time_end=0
!
      write(*,*)"Myid=",myid
      write(*,*)"nprocs=",nprocs
      
	time_begin=MPI_WTIME()
!	
	call InputPara()
!
      if(myid==0)then
          time_end= MPI_WTIME()
             print *, '** Time for preprocessing is',time_end &
                                       - time_begin,'seconds'

          time_begin= MPI_WTIME()
             print *, 'solving...'
      end if       

      call Integration()	
      
      if(myid==0)then
          time_end= MPI_WTIME()
             print *, '** Timefor computation is',time_end&
                                         -time_begin,' seconds'
      end if
	close(ior)
	close(iow1)
	close(iow2)

	close(iores)
    
     call MPI_FINALIZE(ierr)
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
        
        include "mpif.h"
        integer mypid,ierr
        
        real(8) :: prt = 0.0
        integer :: plt = 0
        real(8) :: tf

        call MPI_COMM_RANK(MPI_COMM_WORLD,mypid,ierr)
        
        do while (CurrentTime .le. EndTime)

		DTk  = DTk1
		DTk1 = 1.0d6

!	Calculate kinetic energy
		call KineticE()

!	Time function at the current time step
         		tf = CurrentTMFunction(CurrentTime)		
			
		call FEForce(tf)

!   Update force between N procs

        call updateforce(istep,DTK1) 

		call UpdateFEGeometry()
		
		
        if (CurrentTime.ge.prt) then
			prt = prt + OutTimeStep
			call OutCurve(CurrentTime)
			call OutAnim(CurrentTime)
		end if

		if (istep .ge. plt) then
			plt = plt + ReportSteps
            if(mypid==0)then
			write(*,100) istep, CurrentTime, dtk1, EleDistortion, EngKinetic
100			format(1x, "Step=", i6, 1p, "  T=", e9.3, "  DT=", e9.3,  &
					   "  EDist=", e9.3, "  K.E.=", e9.3)
		    end if
        end if
     istep = istep+1
		CurrentTime = CurrentTime + DTk1

	end do

	end subroutine integration
