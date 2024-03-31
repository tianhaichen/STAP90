
!----------------------------------------------------------------------------
!   ReadIm:��������ı��⣬��������һά����
!              ��Ԫ: n-�������鳤��;
!                    im-��������;
!                    char_check-����Ӧ��������
!          ���ñ���: module_file(NIT)-���������ļ���
!              �쳣: 1.IO����-�˳�����
!                    2.�������Ʋ�ƥ��-�˳�����
!                    3.���ݸ�ʽ����-�˳�����
!----------------------------------------------------------------------------
subroutine ReadIm(im,n,char_check)
	use module_file,only:NIT
	implicit none
	integer,intent(in)::n 
	integer,dimension(n),intent(out)::im
	character(len=*),intent(in)::char_check
	integer::status
	character(len=6)::char

	do !��֤������ȷ
		read(NIT,*,iostat=status) char
		if(status >0 ) then ! IO����
			write(*,*) 'INPUT ERROR: IO',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)
		endif
		if(char(1:1) /= '!' .and. char/=char_check) then !����ע�����ұ��ⲻƥ��
			 write(*,*) 'INPUT ERROR: ',char_check,'  , ENTER <CR> TO EXIT'
			pause
			call exit(0)
		endif
		if(char == char_check) then !���������ȷ,����һ��
			backspace(NIT)
			exit
		end if
	end do

	! ������	
	read(NIT,*,iostat=status) char,im
	if(status >0 ) then ! IO����
		write(*,*) 'INPUT ERROR: DATA',' , ENTER <CR> TO EXIT'
		pause
		call exit(0)
	endif	
		
end subroutine ReadIm


!----------------------------------------------------------------------------
!   ReadFm:��������ı��⣬����ʵ��һά����
!              ��Ԫ: n-ʵ�����鳤��;
!                    im-ʵ������;
!                    char_check-����Ӧ��������
!          ���ñ���: module_file(NIT)-���������ļ���
!              �쳣: 1.IO����-�˳�����
!                    2.�������Ʋ�ƥ��-�˳�����
!                    3.���ݸ�ʽ����-�˳�����
!----------------------------------------------------------------------------
subroutine ReadFm(fm,n,char_check)
	use module_file,only:NIT
	implicit none
	integer,intent(in)::n 
	real(kind=8),dimension(n),intent(out)::fm
	character(len=*),intent(in)::char_check
	integer::status
	character(len=6)::char

	do !��֤������ȷ
		read(NIT,*,iostat=status) char
		if(status >0 ) then ! IO����
			write(*,*) 'INPUT ERROR: IO',' , ENTER <CR> TO EXIT'
			pause
			call exit(0)
		endif
		if(char(1:1) /= '!' .and. char/=char_check) then !����ע�����ұ��ⲻƥ��
			 write(*,*) 'INPUT ERROR: ',char_check,' , ENTER <CR> TO EXIT'
			pause
			call exit(0)
		endif
		if(char == char_check) then !���������ȷ,����һ��
			backspace(NIT)
			exit
		end if
	end do
	
	! ������
	read(NIT,*,iostat=status) char,fm
	if(status >0 ) then ! IO����
		write(*,*) 'INPUT ERROR: DATA',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif	
		
end subroutine ReadFm


!----------------------------------------------------------------------------
!   ReadStr:��������ı��⣬�����ַ���
!              ��Ԫ: str-�ַ���;
!                    char_check-����Ӧ��������
!          ���ñ���: module_file(NIT)-���������ļ���
!              �쳣: 1.IO����-�˳�����
!                    2.�������Ʋ�ƥ��-�˳�����
!                    3.���ݸ�ʽ����-�˳�����
!----------------------------------------------------------------------------
subroutine ReadStr(str,char_check)
	use module_file,only:NIT
	implicit none
	character(len=*),intent(out)::str
	character(len=*),intent(in)::char_check
	integer::status
	character(len=6)::char

	do !��֤������ȷ
		read(NIT,*,iostat=status) char
		if(status >0 ) then ! IO����
			write(*,*) 'INPUT ERROR: IO',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)
		endif
		if(char(1:1) /= '!' .and. char/=char_check) then !����ע�����ұ��ⲻƥ��
			 write(*,*) 'INPUT ERROR: ',char_check,'  , ENTER <CR> TO EXIT'
			pause
			call exit(0)
		endif
		if(char == char_check) then !���������ȷ,����һ��
			backspace(NIT)
			exit
		end if
	end do

	! ������	
	read(NIT,*,iostat=status) char,str
	if(status >0 ) then ! IO����
		write(*,*) 'INPUT ERROR: DATA',' , ENTER <CR> TO EXIT'
		pause
		call exit(0)
	endif	
	
	return		
end subroutine ReadStr

