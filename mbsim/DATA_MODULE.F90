module module_file !������������ļ�
	implicit none
	integer,parameter::NIT=11   ! �����ļ���
	integer,parameter::NOTD=12  ! ����ļ���:λ��
	integer,parameter::NOTV=13  ! ����ļ���:�ٶ�
	integer,parameter::TAPE1=19 ! �м�����ļ�,��Ŷ����ģ̬�ļ�
	integer,parameter::TAPE2=29 ! �м�����ļ�
end module module_file

module module_time
	implicit none
	real(kind=8)::t,tstart,tend,tdeltat
end module module_time

!module module_grvgrd
!	implicit none
!	real(kind=8),dimension(3)::grd !�����ݶ�
!end module module_grvgrd
