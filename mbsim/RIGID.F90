!----------------------------------------------------------------------------
!   ConstrRigid(n):�����n����(����)
!				       �����������ֵ
!          ��    Ԫ: n - ���  
!          ���ñ���: module_body
!          ���ù���: 1.ReadFm      - ��ʵ������
!          ��    ��: 1.����ռ����-�˳�����
!                    2.�����ݴ���  -�˳�����
!----------------------------------------------------------------------------
subroutine ConstrRigid(n)
	use module_body
	implicit none
	integer,intent(in)::n
	integer::stat_alloc ,i
	real(kind=8),dimension(6)::v

	body(n)%numb=n
	body(n)%btype=0
	body(n)%nflex=0        ! ��N����ĵ���λ����Ϊ0
	allocate(body(n)%f(6),body(n)%m(6,6),stat=stat_alloc) !������,������ 
	if(stat_alloc>0) then ! ����ռ����
		write(*,*) 'subroutine ConstrRigid error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif
	

   !-----------------------  ��������͹��� -----------------------
	call ReadFm(v(1:4),4,'MASRG')
	body(n)%mc=V(2:4) 
	
	body(n)%i1=0
	body(n)%i1(1,1)=v(1);   body(n)%i1(2,2)=v(1);   body(n)%i1(3,3)=v(1)            !  M

!	v(2:4)=v(2:4)*v(1)
!	call skew(v(2:4),body(n)%i2)   !Mrc ������ϵԭ�㲻Ϊ����ʱ
	body(n)%i2=0  !Mrc ������ϵԭ��Ϊ����ʱ

	call ReadFm(v,6,'JXYZ') ! ��������͹��Ի�:Jxx,Jyy,Jzz,Jxy,Jxz,Jyz 

	body(n)%i7(1,1)=v(1);   body(n)%i7(2,2)=v(2);   body(n)%i7(3,3)=v(3)    ! �����������ϵ��ת������
	body(n)%i7(1,2)=v(4);   body(n)%i7(1,3)=v(5);   body(n)%i7(2,3)=v(6)
	body(n)%i7(2,1)=v(4);   body(n)%i7(3,1)=v(5);   body(n)%i7(3,2)=v(6)


   !-----------------------  ��ĳ�ʼλ�ƺͳ�ʼ�ٶ� -----------------------	
	
	allocate(body(n)%disp(6),stat=stat_alloc)     ! Ϊ�������꿪�ٿռ�	
	if(stat_alloc>0) then ! ����ռ����
		write(*,*) 'subroutine ConstrRigid error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif	
	allocate(body(n)%velo(6),stat=stat_alloc)     ! Ϊ�����ٶȿ��ٿռ�		
	if(stat_alloc>0) then ! ����ռ����
		write(*,*) 'subroutine ConstrRigid error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif	
	
	call ReadFm(body(n)%disp,6,'BDDIS') !  �����ʼƽ��λ�ƺͳ�ʼת��λ��,1-2-3
	call ReadFm(body(n)%velo,6,'BDVOL') !  �����ʼƽ���ٶȺͳ�ʼת���ٶ�,��Թ���ϵ

	!-----------------------  ����������ָ���ÿ� -----------------------	
	nullify(body(n)%i3,body(n)%i6,body(n)%i4,body(n)%b1,body(n)%b2,body(n)%b3,body(n)%b4,body(n)%b5,body(n)%b6)
	nullify(body(n)%c11,body(n)%c22,body(n)%c33,body(n)%c12,body(n)%c13,body(n)%c23,body(n)%cxy,body(n)%cyz,body(n)%cxz)
	nullify(body(n)%k,body(n)%c)

end subroutine ConstrRigid

!----------------------------------------------------------------------------
!   DestrRigid(n):������n����(����),�ͷ��ڴ�
!          ��    Ԫ: n - ���  
!          ���ñ���: module_body
!          ���ù���: 
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine DestrRigid(n)
	use module_body
	implicit none
	integer,intent(in)::n
	integer::stat_alloc ,i
	real(kind=8),dimension(6)::v

	deallocate(body(n)%f,body(n)%m) !������,������ 
	deallocate(body(n)%disp)        ! ��������
	deallocate(body(n)%velo)        ! �����ٶ�		

end subroutine DestrRigid


!----------------------------------------------------------------------------
!    GenRigidM(n):���ɸ�����������
!          ��    Ԫ: n - ��� 
!          ���ñ���: module_body
!          ���ù���: 
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine	GenRigidM(n)
	use module_body
	implicit none
	integer,intent(in)::n

	body(n)%m(1:3,1:3)=body(n)%i1   ! Mrr
	body(n)%m(4:6,4:6)=body(n)%i7   ! Mcc=J0

!	body(n)%m(1:3,4:6)=-body(n)%i2  ! Mcr=S0
!	body(n)%m(1:3,4:6)=matmul(matmul(body(n)%A,body(n)%m(1:3,4:6)),body(n)%G)   ! Mrc=A*Irc*G
	body(n)%m(1:3,4:6)=0

	body(n)%m(4:6,4:6)=matmul(matmul(transpose(body(n)%G),body(n)%m(4:6,4:6)),&  ! Mcc=G^t*Icc*G
	                   body(n)%G)
	body(n)%m(4:6,1:3)=TRANSPOSE(body(n)%m(1:3,4:6))     ! Mcr
	
	return	
end subroutine GenRigidM

!----------------------------------------------------------------------------
!    GenRigidF(n):���ɸ�������
!          ��    Ԫ: n - ��� 
!          ���ñ���: module_body,module_mark
!          ���ù���: 
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine	GenRigidF(n)
	use module_body;	use module_system,only:grd
	implicit none
	integer,intent(in)::n
	real(kind=8),dimension(3,3)::ske,temp
	real(kind=8),dimension(3)::v
	real(kind=8),dimension(6)::f

	body(n)%f=0.0
	! -------------------- ���������,Qf -------------------- 		   
	call skew(body(n)%omega,ske)	
	!-------- Qfr1=A*OMEGA~*S0^~*OMEGA+A*S0^~*GDTD ----------
	body(n)%f(1:3)=matmul(matmul(matmul(body(n)%A,ske),body(n)%i2),body(n)%omega) &  
                  +matmul(matmul(body(n)%A,body(n)%i2),body(n)%gdtd)
		!---------------- Qfc1=-G^t*OMEGA~*Icc0*OMEGA-G^t*Icc0*GDTD ---------------------------------
	body(n)%f(4:6)=matmul(matmul(transpose(-body(n)%G),ske),matmul(body(n)%i7,body(n)%omega)) &
                  -matmul(matmul(transpose(body(n)%G),body(n)%i7),body(n)%gdtd)

	
	! -------------------- ��������,Qg  -------------------- 
	body(n)%f(1:3)=body(n)%f(1:3)+body(n)%m(1,1)*grd
	body(n)%f(4:6)=body(n)%f(4:6)+matmul(body(n)%m(4:6,1:3),grd) ! G^t*S0^t*A^t*GRD

	return	
end subroutine GenRigidF



