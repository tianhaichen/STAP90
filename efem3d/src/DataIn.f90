
! ---------------------------------------------------------------------
! -                                                                   -
! -  Data input procedures                                            -
! -                                                                   -
! ---------------------------------------------------------------------

module DataIn

use ElementData
use ParticleData
use MaterialData
use DataOut
use Simulation
use FFI

contains

	subroutine InputPara()
! ---------------------------------------------------------------------
! -                                                                   -
! -  Purpose                                                          -
! -     Input and initialize data                                     -
! -                                                                   -
! ---------------------------------------------------------------------
	use DFLIB ! for NARGS()
	implicit none

	if(NARGS().ne.2) then
		stop 'Usage: mpm3d InputFileName'
	else
		call GETARG(1,FileInp)
	end if

	call OpenFile()	! Open input/output files

	call GetData()	! Read data from input file

	allocate(nElem(nb_node))	! Allocate storage for stress smoothing

	end subroutine InputPara


	subroutine GetData()
! ---------------------------------------------------------------------
! -                                                                   -
! -  Purpose                                                          -
! -     Input data from input file using FFI module                   -
! -                                                                   -
! ---------------------------------------------------------------------
	use TimeFunctionData
	implicit none

	integer key, i

	integer,parameter:: nbkw = 20
	character(4),parameter:: kw(nbkw) = (/  &	! list of keywords
		'endi', 'efep', 'nbno', 'repo', 'hour', 'time', &
		'endt', 'outt', 'nmat', 'mate', 'node', 'jaum', &
		'ncom', 'load', 'velo', 'curv', 'elem', 'nbel', &
        'rigi', 'dtsc'                                  &
	/)

	do while(.true.)
		key = keyword(kw,nbkw)
		select case(key)

		case(1)			! end input
			exit		! Terminates execution of Do loop
			
		case(2)			! epef (title)
			call GetString(Title)
			
		case(3)			! nbno
			nb_node = GetInt()
			print *, 'nb_node = ',nb_node

			allocate(Acc(nDim, nb_node))
			allocate(Pos(nDim, nb_node))
			allocate(Vel(nDim, nb_node))
			allocate(Fp(nDim, nb_node))
			allocate(Mp(nb_node))

			Vel = 0.0d0
			Fp  = 0.0d0
			Mp  = 0.0d0

		case(4)			! RepTimeStep
			RepTimeStep = GetReal()

		case(5)			! hourglass
			HourGlass%method = GetInt()
			HourGlass%Qhg = GetReal()
		
		case(6)		! timefunction
			LenTMFunction = GetInt()
			allocate(TMFunction(LenTMFunction))
			do i = 1, LenTMFunction
				TMFunction(i)%time  = GetReal()
				TMFunction(i)%value = GetReal()
			end do
			
		case(7)		! endtime
			EndTime = GetReal()
			print *, 'EndTime = ',EndTime

		case(8)		! outtimestep
			OutTimeStep = GetReal()

		case(9)		! nmat
			nb_mat = GetInt()
			print *, 'nb_mat = ',nb_mat

			allocate(mat_list(nb_mat))

		case(10)		! material
			call SetMaterial()

		case(11)		! node
			call SetNode()

		case(12)		! Jaum
			Jaum = SetOnOff()

		case(13)		! nb_comp
			nb_comp = GetInt()
			print *, 'nb_comp = ', nb_comp

			if (nb_node.eq.0)   &
			    stop '*** Error *** nb_node must be defined first!'

			allocate(CompLen(nb_comp))
			allocate(CompMember(nb_comp,nb_node))
			CompLen = 0
			CompMember = 0

		case(14)		! load
			call SetLoad()

		case(15)		! initial velocity
			call SetVelocity()

		case(16)		! curve
			nCurves = nCurves + 1

			if (nCurves .gt. MaxCurves)   &
				stop '*** Error *** Too many curves defined !'

			CurveOption(nCurves) = SetResOption()

			if(nb_word.gt.0) then
				CurvePoint(nCurves) = GetInt()
			else 
				CurvePoint(nCurves) = 1	  ! Default curve point
			end if

		case(17)        ! Read elements
			call ElememtIn()

		case(18)		! nbel
			nb_element = GetInt()
			print *, 'nb_element = ', nb_element

			allocate(element_list(nb_element))

		case(19)		! define rigid plane
			plane%nDir = GetInt()
			plane%coor = GetReal()

		case(20)		! DTSCale
			DTScale = GetReal()

		case default	! error
			stop 'error GetData'
			
		end select
	end do
		
	end subroutine GetData


	subroutine SetMaterial()
! ---------------------------------------------------------------------
! -                                                                   -
! -  Purpose                                                          -
! -     Input and initialize material data                            -
! -                                                                   -
! ---------------------------------------------------------------------
	implicit none

	integer i, t
	real(8) :: E, mu, rho

	integer,parameter:: nbkw = 4
	character(4),parameter:: kw(4) = (/'elas','pla1','pla2','john'/)

	if (nb_mat.eq.0)   &
		stop '*** Error *** nb_mat must be defined in advance!'
		
	i = 0
	do while(i.lt.nb_mat)
		i = GetInt()
		if(i.gt.nb_mat) then
			call ErrorMsg()
			print *, '*** Error *** Too many material sets'
			print *, 'required : ',nb_mat
			print *, 'inputed  : ',i
			stop 
		end if

		t = KeyWord(kw,nbkw)

		select case(t)

		case(1)	! elastic
			mat_list(i)%MatType = 1
			mat_list(i)%Density = GetReal()
			mat_list(i)%Young = GetReal()
			mat_list(i)%Poisson = GetReal()

		case(2) ! pla1: elastic-perfectly plastic
			mat_list(i)%MatType = 2
			mat_list(i)%Density = GetReal()
			mat_list(i)%Young = GetReal()
			mat_list(i)%Poisson = GetReal()
			mat_list(i)%Yield0 = GetReal()

		case(3) ! pla2: isotropic hardening
			mat_list(i)%MatType = 3
			mat_list(i)%Density = GetReal()
			mat_list(i)%Young = GetReal()
			mat_list(i)%Poisson = GetReal()
			mat_list(i)%Yield0 = GetReal()
			mat_list(i)%TangMod = GetReal()

		case(4) ! johnson-cook
			mat_list(i)%MatType = 4
			mat_list(i)%Density = GetReal()
			mat_list(i)%Young = GetReal()
			mat_list(i)%Poisson = GetReal()
			mat_list(i)%Yield0 = GetReal()
			mat_list(i)%B_jnc = GetReal()
			mat_list(i)%n_jnc = GetReal()
			mat_list(i)%C_jnc = GetReal()

		case default
			call ErrorMsg()
			stop '*** Error *** Invalid Material Type!'

		end select

		E  = mat_list(i)%Young
		mu = mat_list(i)%Poisson
		rho= mat_list(i)%Density
	
		mat_list(i)%WaveSpeed = sqrt(E*(1-mu)/((1+mu)*(1-2*mu)*rho))

	end do

	end subroutine SetMaterial


	subroutine SetNode()
! ---------------------------------------------------------------------
! -                                                                   -
! -  Purpose                                                          -
! -     Input and initialize Node data                                -
! -                                                                   -
! ---------------------------------------------------------------------
	implicit none

	integer i, j, p, comp, icell

	if (nb_node.eq.0)   &
		stop '*** Error *** nbmp must be defined in advance!'

	i = 0
	do while(i.ne.nb_node)
		
		i = GetInt()

		if(i.gt.nb_node) then
			call ErrorMsg()
			print *, '*** Error *** Too many nodes'
			print *, 'required : ',nb_node
			print *, 'inputed  : ',i
			stop 
		end if

		comp = GetInt()

		if(comp.gt.0 .and. comp.le.nb_comp) then
			CompLen(comp) = CompLen(comp) + 1
			CompMember(comp,CompLen(comp)) = i
		else if(comp.gt.nb_comp) then
			call ErrorMsg()
			stop '*** Error *** component number greater than nb_comp'
		end if

		do j = 1, nDim
			Pos(j,i) = GetReal()
		end do

	end do

	end subroutine SetNode


	subroutine ElememtIn()
! ---------------------------------------------------------------------
! -                                                                   -
! -  Purpose                                                          -
! -     Input and initialize element data                             -
! -     calculate particle mass and set cutoff mass                   -
! -                                                                   -
! ---------------------------------------------------------------------
	implicit none

	integer i, j, k, nh, m, p

	real(8):: vol, mass
	real(8):: xyz(3,8), ijac(3,3)
	real(8):: xms = 0.0

	if (nb_element .eq. 0)   &
		stop '*** Error *** nbel must be defined in advance!'

	if (.not. allocated(history_list)) then
		allocate(history_list(nb_element))
		call InitHistory()
	end if

	do while (i .ne. nb_element)
			
		i = GetInt()

		if(i.gt.nb_element) then
			call ErrorMsg()
			print *, '*** Error *** Too many elements'
			print *, 'required : ',nb_element
			print *, 'inputed  : ',i
			stop 
		end if

		LenHistoryList = LenHistoryList+1
		element_list(i)%nHistory = LenHistoryList

		element_list(i)%mat = GetInt()
		history_list(LenHistoryList)%sig_y = mat_list(element_list(i)%mat)%Yield0

		do j = 1, 8
			element_list(i)%nNode(j) = GetInt()
		enddo

	end do

!	Calculte the particle masses and volumes
	do i = 1, nb_element
		do j = 1, 8
			p = element_list(i)%nNode(j)
			xyz(:,j) = Pos(:,p)
		end do

		call Jacobian(xyz, vol, ijac)

		history_list(element_list(i)%nHistory)%VOL = vol  ! initial element volume
		mass = mat_list(element_list(i)%mat)%Density * vol

		do j = 1, 8
			p = element_list(i)%nNode(j)
			Mp(p) = Mp(p) + mass/8.0
		enddo

	end do

	end subroutine ElememtIn


	subroutine SetLoad()
! ---------------------------------------------------------------------
! -                                                                   -
! -  Purpose                                                          -
! -     Input and initialize external load                            -
! -                                                                   -
! ---------------------------------------------------------------------
	implicit none

	integer k, inode, icomp, cpl, i, j
	real(8):: ff(nDim)

	integer,parameter:: nbkw = 3
	character(4),parameter:: kw(3) = (/'endl','node','comp'/)

	if (nb_node.eq.0)   &
		stop '*** Error *** nbmp must be defined !'

	do while(.true.)
		k = keyword(kw,nbkw)
		select case(k)
		case(1)	! endload
			exit

		case(2)	! by node
			inode = GetInt()
			do j = 1, nDim
				Fp(j, inode) = GetReal()
			end do

		case(3) ! by component
			icomp = GetInt()	! component number

			do j = 1, nDim
				ff(j) = GetReal()
			end do

			cpl = CompLen(icomp)! component length
			do i = 1, cpl
				inode = CompMember(icomp,i)	! node number of this component
				Fp(:,inode) = ff
			end do
				
		case default	! error
			call ErrorMsg()
			stop 'error GetLoad'
		
		end select

	end do

	end subroutine SetLoad


	subroutine SetVelocity()
! ---------------------------------------------------------------------
! -                                                                   -
! -  Purpose                                                          -
! -     Input and initialize velocities                               -
! -                                                                   -
! ---------------------------------------------------------------------
	implicit none

	integer k, inode, icomp, cpl, i, j
	real(8):: vxp(nDim)

	integer,parameter:: nbkw = 3
	character(4),parameter:: kw(3) = (/'endv','node','comp'/)

	if (nb_node.eq.0)   &
		stop '*** Error *** nbmp must be defined !'

	do while(.true.)
		k = keyword(kw,nbkw)
		select case(k)
		case(1)	! endload
			exit

		case(2)	! by node
			inode = GetInt()

			do j = 1, nDim
				Vel(j, inode) = GetReal()
			end do

		case(3) ! by component
			icomp = GetInt()	! component number
			do j = 1, nDim
				vxp(j) = GetReal()
			end do

			cpl = CompLen(icomp)  ! component length
			do i = 1, cpl
				inode = CompMember(icomp,i)	! node number of this component
				Vel(:, inode) = vxp
			end do
				
		case default	! error
			call ErrorMsg()
			stop 'error GetVelocity'
		
		end select

	end do

	end subroutine SetVelocity


	logical function SetOnOff()
! ---------------------------------------------------------------------
! -                                                                   -
! -  Purpose                                                          -
! -     Read and set on/off switch                                    -
! -                                                                   -
! ---------------------------------------------------------------------
	implicit none
	integer:: k

	integer,parameter:: nbkw = 2
	character(4),parameter:: kw(2) = (/'on  ','off '/)

	k = keyword(kw,nbkw)

	select case(k)

	case(1)	! on
		SetOnOff = .true.

	case(2)	! off
		SetOnOff = .false.

	case default	! error
		call ErrorMsg()
		stop 'switch should be ON/OFF'
	
	end select

	end function SetOnOff


end module DataIn
