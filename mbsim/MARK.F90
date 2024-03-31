!===================================����Ϊģ�鶨��===================================
module module_mark
	!==========================����Ϊ���ݶ���=========================
	implicit none
	type type_mark ! �ο���,���Ͽ�ʩ������
		integer::numm,bom                           ! �ο����,�ο����������
		integer::non                                ! �ο������ڽڵ��,������
		real(kind=8),dimension(3)::r0,r             ! �ο���λ��
		real(kind=8),dimension(:,:),pointer::hat    ! �ο����ƽ��ģ̬
		real(kind=8),dimension(:,:),pointer::sig    ! �ο����ת��ģ̬ 
		real(kind=8),dimension(6)::f                ! �ο������������,������ϵ
	END TYPE TYPE_mark
	type(type_mark),dimension(:),pointer::mark       ! ����NM���ο���

	!=====================����Ϊ�������ӳ���ӿڿ�=======================

end module module_mark


!==============================����Ϊ�������ӳ�������==============================

!----------------------------------------------------------------------------
!   ConstrMark(n):�����n���ο���, ���ο������
!          ��    Ԫ: n - �ο����
!          ���ñ���: module_mark
!          ���ù���: 1.ReadFm      - ��ʵ������
!          ��    ��: 1.����ռ����-�˳�����
!                    2.�����ݴ���  -�˳�����
!----------------------------------------------------------------------------
subroutine ConstrMark(n)
	use module_mark; use module_body;	use module_system,only:nb


	mark(n)%numm=n
	call ReadIm(mark(n)%bom,1,'BODY') ! ���
	if(mark(n)%bom<=0 .or.mark(n)%bom>nb) then ! ��Ŵ���
		write(*,*) 'subroutine ConstrMark error: Body',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)		
	end if	

	if(body(mark(n)%bom)%btype == 0) then ! ��Ϊ����
		call ReadFm(mark(n)%r,3,'LOCA')  ! ����ο��������λ��(������ϵ)
		nullify(mark(n)%hat,mark(n)%sig) ! ����Ҫ����ģ̬
	else ! �ڽ���Ϊ������
		call ReadIm(mark(n)%non,1,'LOCA') ! ����ο�������Ľڵ��
	end if
	
	return
end subroutine ConstrMark

!----------------------------------------------------------------------------
!   DestrMark(n):������n���ο���,�ͷ��ڴ�
!          ��    Ԫ: n - �ο����
!          ���ñ���: module_mark;module_body
!          ���ù���: 
!          ��    ��: 
!----------------------------------------------------------------------------
subroutine DestrMark(n)
	use module_mark;	use module_body
	
	if(body(mark(n)%bom)%btype==1) then !����ο������ڵ���Ϊ�����壬�ͷ�ģָ̬��
		deallocate(mark(n)%hat,mark(n)%sig)
	endif 

	return
end subroutine DestrMark
