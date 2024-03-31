!===================================����Ϊģ�鶨��===================================
module module_system

	!=========================����Ϊ���ݶ���=========================
	use module_body
	use module_hinge
	use module_mark
	implicit none
	integer::nb,nh,nm                      ! �������µ������ο�����   
	integer::nq                            ! ϵͳ�Ĺ���������
	integer::nlam                          ! ���ϳ��Ӹ���
	integer,dimension(:),pointer::locq     ! ��������λ�����飺����ÿһ�������������ϵͳ���������е�λ��
	integer,dimension(:),pointer::locc     ! Լ��λ�����飺����ÿһ��Լ����ϵͳԼ�������е�λ��
	real(kind=8),dimension(:),pointer::q,qdt   ! ��������,�����ٶ�
	real(kind=8),dimension(:),pointer::lam     ! ���ϳ���
	real(kind=8),dimension(3)::grd             !�����ݶ�


	!=====================����Ϊ�������ӳ���ӿڿ�=======================

end module module_system


!==============================����Ϊ�������ӳ�������==============================

!----------------------------------------------------------------------------
!   ConstrSys:����ϵͳģ��
!             �����ڴ�ռ� ; ������ģ�� ; �����ģ�� ; ����ο���ģ�� 
!          ��    Ԫ: 
!          ���ñ���: module_system(module_body,module_hinge,module_mark)
!          ���ù���: 1.ReadIm      - ����������
!                    2.ReadStr     - �ַ���
!                    3.ConstrRigid - �������
!                    4.ConstrRevo  - ��������
!                    5.ConstrMak   - ����ο���
!          ��    ��: 1.����ռ����-�˳�����
!                    2.��š��ºš��ο���Ų�ƥ��-�˳�����
!                    3.�塢�����ʹ���-�˳�����
!----------------------------------------------------------------------------
subroutine ConstrSys()
	use module_system
	implicit none
	integer::stat_alloc,i,j
	integer::qstart,qend
	character(len=10)::str


	!------------------------����ϵͳ���˲���------------------------		
	call ReadIm(nb,1,'NBODY')  ! ����
	call ReadIm(nh,1,'NHINGE') ! ����
	call ReadIm(nm,1,'NMARK')  ! �ο�����

	
	!------------------------�����ڴ�ռ�------------------------
	allocate(locq(nb),locc(nh),stat=stat_alloc)     ! ��������λ������ 
	if(stat_alloc>0) then ! ����ռ����
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif

	allocate(body(nb),stat=stat_alloc)	  ! ��
	if(stat_alloc>0) then ! ����ռ����
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif
	
	allocate(hinge(nh),stat=stat_alloc)    ! ��
	if(stat_alloc>0) then ! ����ռ����
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif
	
	if(nm>=1) allocate(mark(nm),stat=stat_alloc)    ! �ο���
	if(stat_alloc>0) then ! ����ռ����
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif

	!------------------------������------------------------
	do i=1,nb,1
		call ReadIm(j,1,'NOB') ! �����
		if(j /= i) then !��Ų�ƥ��
			write(*,*) 'Input error: NOB',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)		
		end if  
		call ReadIm(j,1,'BTYPE') ! ��������
		if(j /= 0 .and. j /= 1) then !�����ʹ���
			write(*,*) 'Input error: BTYPE',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)		
		end if 
		select case(j)
		case(0) ! �������
			call ConstrRigid(i) 
		case(1) ! ����������
!			call ConstrFlex(i)
		end select
	end do
	
	!------------------------������------------------------
	do i=1,nh,1
		call ReadIm(j,1,'NOH') ! ���º�
		if(j /= i) then !�ºŲ�ƥ��
			write(*,*) 'Input error: NOH',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)		
		end if  
		call ReadStr(str,'TYPE') ! ��������
		
		select case(str)
		case('REVOLUTE') ! ����
			call ConstrRevo(i)	
		case default  ! δ֪������
			write(*,*) 'Input error: HTYPE',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)					
		end select
	end do

	!------------------------�����ο���------------------------
	do i=1,nm,1
		call ReadIm(j,1,'NOM') ! ���ο���
		if(j /= i) then !�ο��Ų�ƥ��
			write(*,*) 'Input error: NOM',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)		
		end if  
		call ConstrMark(i) !����ο���	
	end do

	!------------------------���������ٶ�g------------------------
	call ReadFm(grd,3,'GRVGR') ! ���ο���


	!----------  ����ϵͳ���ɶ���;ȷ����Ĺ���������ϵͳ�����������е�λ��:locq ----------
	nq=6+body(1)%nflex
	locq(1)=1
	do i=2,nb
		locq(i)=locq(i-1)+body(i-1)%nflex+6  ! ȷ������λ��
		nq=nq+6+body(i)%nflex                ! ϵͳ�����ɶ�  
	end do

	!----------------------------------  �������ϳ��Ӹ��� ----------------------------------
	nlam=0
	locc(1)=1
	select case(hinge(1)%htype)
	case('REVOLUTE') ! ����
		nlam=nlam+5
	case('FIXED') !�̶�Լ��
			! ����Լ��
	end select
	do i=2,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! ����
			nlam=nlam+5
			locc(i)=locc(i-1)+5
		case('FIXED') !�̶�Լ��
			! ����Լ��
		end select
	end do

	allocate(q(nq),qdt(nq),lam(nlam),stat=stat_alloc)     ! �������ꡢ�����ٶȡ����ϳ��� 
	if(stat_alloc>0) then ! ����ռ����
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif
	
	!-----------------------------  ��Ĺ�������=>ϵͳ�������� -----------------------------
	do i=1,nb,1
		qstart=locq(i)
		qend=locq(i)+5+body(i)%nflex
		! ---------- ��������任����� ----------
		q(qstart:qend)=body(i)%disp    ! ��Ĺ�������=>ϵͳ��������
		qdt(qstart:qend)=body(i)%velo  ! ��Ĺ����ٶ�=>ϵͳ�����ٶ�
	end do
	
	return
end subroutine ConstrSys

!----------------------------------------------------------------------------
!   DestrSys:����ϵͳģ��
!            �ͷ��ڴ�ռ� ; ������ģ�� ; ������ģ�� ; �����ο���ģ��
!          ��    Ԫ: 
!          ���ñ���: module_system(module_body,module_hinge,module_mark)
!          ���ù���: 1.DestrRigid - ��������
!                    2.DestrRevo  - ��������
!                    3.DestrMak   - �����ο���
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine DestrSys()
	use module_system
	implicit none
	integer::i

	!------------------------�����ο���------------------------
	do i=1,nm,1
		call DestrMark(i) !�����ο���	
	end do

	!------------------------������------------------------
	do i=1,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! ��������
			call DestrRevo(i)	
		case('FIXED')  ! �����̶�Լ��
!			call DestrFixed(i) 	
		end select
	end do


	!------------------------������------------------------
	do i=1,nb,1
		select case(body(i)%btype)
		case(0) ! ��������
			call DestrRigid(i) 
		case(1) ! ����������
!			call DestrFlex(i)
		end select
	end do


	deallocate(locq,locc)     ! ��������λ������ 
	deallocate(body)	  ! ��
	deallocate(hinge)    ! ��
	if(nm>=1) deallocate(mark)    ! �ο���

	deallocate(q,qdt,lam)     ! �������ꡢ�����ٶȡ����ϳ��� 
end subroutine DestrSys



!----------------------------------------------------------------------------
!   GenM(m_sys):����ϵͳ����ѧ���̵�������
!               ����ÿһ�������������װϵͳ�����󣬴�����Ԫm_sys��
!          ��    Ԫ: m_sys
!          ���ñ���: module_system(module_body)
!          ���ù���: 1.GenRigidM(i) - ��i�����������(����)
!                    2.GenFlexM(i)  - ��i�����������(����)
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine GenM(m_sys)
	use module_system
	implicit none
	real(kind=8),dimension(nq,nq),intent(out)::m_sys
	integer::i,rstart,rend,cstart,cend

	m_sys=0.0
	do i=1,nb,1
		! ---------- ����������ϵͳ�������λ�� ----------
		rstart=locq(i)
		rend=locq(i)+5+body(i)%nflex
		cstart=rstart
		cend=rend
		! ---------- ��������任����� ----------
		body(i)%disp=q(rstart:rend)    ! ��ϵͳ�������긳ֵ����Ĺ�������
		body(i)%velo=qdt(rstart:rend)  ! ��ϵͳ�����ٶȸ�ֵ����Ĺ����ٶ�
		call Rotat(i)      ! ��������任�����
		! ---------- ������������� ----------
		select case(body(i)%btype)
		case(0)
			call GenRigidM(i)
		case(1)
!			call GenFlexM(i)
		end select				
		m_sys(rstart:rend,cstart:cend)=body(i)%m	
	end do

end subroutine genM

!----------------------------------------------------------------------------
!   GenF(f_sys):����ϵͳ����ѧ���̵��Ҷ���
!               ����ÿһ������������,����,��װϵͳ���������Ԫf_sys��
!          ��    Ԫ: f_sys
!          ���ñ���: module_system(module_body)
!          ���ù���: 1.GenRigidF(i) - ��i�������(����)
!                    2.GenFlexF(i)  - ��i�������(����)
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine GenF(f_sys)
	use module_system
	implicit none
	real(kind=8),dimension(nq),intent(out)::f_sys
	integer::i,nob,rstart,rend
	real(kind=8),dimension(3,3)::ske

	f_sys=0.0

	do i=1,nb,1
		! ---------- ��������ϵͳ�����λ�� ----------
		rstart=locq(i)
		rend=locq(i)+5+body(i)%nflex
		! ---------- ����������� ----------
		select case(body(i)%btype)
		case(0)
			call GenRigidF(i)
		case(1)
!			call GenFlexF(i)
		end select				
		f_sys(rstart:rend)=body(i)%f
	end do
	
	! -------------------- �������� --------------------
	do i=1,nm,1
		call ExterForce
		nob=mark(i)%bom
		
		if(body(nob)%nflex==1) then
!			call SolMarkR(i) ! ����ο����λ��			 
!			���㵯���񶯵Ĺ�������
		endif

		call skew(mark(i)%r,ske)
		body(nob)%f(1:3)=body(nob)%f(1:3)+matmul(body(nob)%A,mark(i)%f(1:3))
		body(nob)%f(4:6)=body(nob)%f(4:6)-matmul(transpose(body(nob)%G), &
		                 matmul(transpose(ske),mark(i)%f(1:3)))
		mark(i)%f(4:6)=mark(i)%f(4:6)+matmul(body(nob)%A,mark(i)%f(4:6))
	end do

	return
end subroutine genF

!----------------------------------------------------------------------------
!         SolCq(Cq):����ϵͳԼ�����̵��ſɱ���
!          ��    Ԫ: Cq
!          ���ñ���: module_system(module_hinge)
!          ���ù���: 1.SolCqRevo(i) - �����i����(����)
!                    2. ...
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine SolCq(Cq)
	use module_system
	implicit none
	real(kind=8),dimension(nlam,nq)::Cq
	real(kind=8),dimension(nlam)::C,Dc,Gamma
	integer::i,nb1,nb2,lb1,lb2


	! -------------------- ����ÿһ����  --------------------
	do i=1,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! ����
			call SolCqRevo(i)	 
			! -------------------- ��װϵͳCq  --------------------
			if(hinge(i)%inb /=0 ) then
				nb1=hinge(i)%inb
				lb1=locq(nb1)+body(nb1)%nflex+5
				Cq(locc(i):locc(i)+4,locq(nb1):lb1)=hinge(i)%cqi
			end if
			if(hinge(i)%exb /=0 ) then
				nb2=hinge(i)%exb
				lb2=locq(nb2)+body(nb2)%nflex+5
				Cq(locc(i):locc(i)+4,locq(nb2):lb2)=hinge(i)%cqj
			end if
		case('FIXED') ! �̶�Լ��
		! 
		end select
	end do
	
	return
end subroutine SolCq


!----------------------------------------------------------------------------
!   SolGamma(Gamma):����ϵͳ���ٶ�Լ�����̵��Ҷ��Gamma
!          ��    Ԫ: Gamma
!          ���ñ���: module_system(module_hinge)
!          ���ù���: 1.SolGammaRevo(i) - �����i����(����)
!                    2. ...
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine SolGamma(Gamma)
	use module_system
	implicit none
	real(kind=8),dimension(nlam)::Gamma
	integer::i


	! -------------------- ����ÿһ����  --------------------
	do i=1,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! ����
			call SolGammaRevo(i)	 
			Gamma(locc(i):locc(i)+4)=hinge(i)%gamma	!  ��װϵͳgamma 
		case('FIXED') ! �̶�Լ��
		! 
		end select
	end do
	
	return
end subroutine SolGamma


!----------------------------------------------------------------------------
!   SolCDc(C,Dc):����Լ�����̺��ٶ�Լ�����̵�ֵ:Dc,C
!          ��    Ԫ: Dc,C
!          ���ñ���: module_system(module_hinge)
!          ���ù���: 1.SolCDcRevo(i) - �����i����(����)
!                    2. ...
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine SolCDc(C,Dc)
	use module_system
	implicit none
	real(kind=8),dimension(nlam)::C,Dc
	integer::i


	! -------------------- ����ÿһ����  --------------------
	do i=1,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! ����
			call SolCDcRevo(i)	 
			C(locc(i):locc(i)+4)=hinge(i)%C	   !  ��װϵͳC 
			Dc(locc(i):locc(i)+4)=hinge(i)%Dc	!  ��װϵͳDc
		case('FIXED') ! �̶�Լ��
		! 
		end select
	end do
	
	return
end subroutine SolCDc

