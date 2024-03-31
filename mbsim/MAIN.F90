program MBSim
	
	use module_file;	use module_sol_af; use module_system
	use module_time

	implicit none
	integer::method	! ��ⷽ��:1-���㷨
	character(len=6)::char
	integer::i
	real(kind=8),dimension(6)::temf
	integer,dimension(6)::temi


	open (unit=NIT,file='INPUT.DAT')
   open (unit=NOTD,file='OUTD.DAT')
	open (unit=NOTV,file='OUTV.DAT')
	open (unit=TAPE1,file='NTAPE1',form='unformatted')
	open (UNIT=TAPE2,file='NTAPE2',form='unformatted')

!---------------------------------------------------------------------
!  ���ַ�����ʱ�䲽�� 	
!---------------------------------------------------------------------
	call	ReadIm(temi(1:1),1,'METHOD')	! ������ⷽ�� 
	method=temi(1)
	
	call ReadFm(temf(1:3),3,'TIME') ! ����ʱ��	
	tstart=temf(1)
	tend=temf(2)
	tdeltat=temf(3)

!---------------------------------------------------------------------
!  ��ʼ�� 	
!---------------------------------------------------------------------
	call ConstrSys()	!�������ϵͳ�������ڴ�ռ䣬������	
	select case(method)	! ��ʼ����ͬ����ⷽ��
	case(1)
			call ConstrSolAf()	!Ϊ���㷨��������ڴ�ռ�,������ֲ��������ó�ֵ
	case(2)
		!	��������		      
	end select


!---------------------------------------------------------------------
!  ʱ�����
!---------------------------------------------------------------------
	
	t=tstart
	do while(t<tend)
		select case(method)	  ! ���ݲ�ͬ���㷨����һ��
		case(1)
				CALL StepSolAf() !���㷨����һ��
		case(2)
			!	��������		      
		end select
	end do

   close (NIT)
   close (NOTD)
	close (NOTV)
	close (TAPE1,STATUS='DELETE')
	close (TAPE2,STATUS='DELETE')
	
	select case(method)	  ! ������ⷽ��	
	case(1)
		call DestrSolAf(t,tdeltat)	! �������㷨���ͷſռ�
	case(2)
		!	��������		      
	end select
	call DestrSys()	! ��������ϵͳ���ͷſռ�
	
	stop
end program MBSim