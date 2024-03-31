
!----------------------------------------------------------------------------
!   ReadIm:检验输入的标题，读入整形一维数组
!              哑元: n-整形数组长度;
!                    im-整形数组;
!                    char_check-所对应的数组名
!          引用变量: module_file(NIT)-输入数据文件号
!              异常: 1.IO错误-退出程序
!                    2.数组名称不匹配-退出程序
!                    3.数据格式错误-退出程序
!----------------------------------------------------------------------------
subroutine ReadIm(im,n,char_check)
	use module_file,only:NIT
	implicit none
	integer,intent(in)::n 
	integer,dimension(n),intent(out)::im
	character(len=*),intent(in)::char_check
	integer::status
	character(len=6)::char

	do !验证标题正确
		read(NIT,*,iostat=status) char
		if(status >0 ) then ! IO错误
			write(*,*) 'INPUT ERROR: IO',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)
		endif
		if(char(1:1) /= '!' .and. char/=char_check) then !不是注释行且标题不匹配
			 write(*,*) 'INPUT ERROR: ',char_check,'  , ENTER <CR> TO EXIT'
			pause
			call exit(0)
		endif
		if(char == char_check) then !如果标题正确,回退一行
			backspace(NIT)
			exit
		end if
	end do

	! 读数据	
	read(NIT,*,iostat=status) char,im
	if(status >0 ) then ! IO错误
		write(*,*) 'INPUT ERROR: DATA',' , ENTER <CR> TO EXIT'
		pause
		call exit(0)
	endif	
		
end subroutine ReadIm


!----------------------------------------------------------------------------
!   ReadFm:检验输入的标题，读入实形一维数组
!              哑元: n-实形数组长度;
!                    im-实形数组;
!                    char_check-所对应的数组名
!          引用变量: module_file(NIT)-输入数据文件号
!              异常: 1.IO错误-退出程序
!                    2.数组名称不匹配-退出程序
!                    3.数据格式错误-退出程序
!----------------------------------------------------------------------------
subroutine ReadFm(fm,n,char_check)
	use module_file,only:NIT
	implicit none
	integer,intent(in)::n 
	real(kind=8),dimension(n),intent(out)::fm
	character(len=*),intent(in)::char_check
	integer::status
	character(len=6)::char

	do !验证标题正确
		read(NIT,*,iostat=status) char
		if(status >0 ) then ! IO错误
			write(*,*) 'INPUT ERROR: IO',' , ENTER <CR> TO EXIT'
			pause
			call exit(0)
		endif
		if(char(1:1) /= '!' .and. char/=char_check) then !不是注释行且标题不匹配
			 write(*,*) 'INPUT ERROR: ',char_check,' , ENTER <CR> TO EXIT'
			pause
			call exit(0)
		endif
		if(char == char_check) then !如果标题正确,回退一行
			backspace(NIT)
			exit
		end if
	end do
	
	! 读数据
	read(NIT,*,iostat=status) char,fm
	if(status >0 ) then ! IO错误
		write(*,*) 'INPUT ERROR: DATA',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif	
		
end subroutine ReadFm


!----------------------------------------------------------------------------
!   ReadStr:检验输入的标题，读入字符串
!              哑元: str-字符串;
!                    char_check-所对应的数组名
!          引用变量: module_file(NIT)-输入数据文件号
!              异常: 1.IO错误-退出程序
!                    2.数组名称不匹配-退出程序
!                    3.数据格式错误-退出程序
!----------------------------------------------------------------------------
subroutine ReadStr(str,char_check)
	use module_file,only:NIT
	implicit none
	character(len=*),intent(out)::str
	character(len=*),intent(in)::char_check
	integer::status
	character(len=6)::char

	do !验证标题正确
		read(NIT,*,iostat=status) char
		if(status >0 ) then ! IO错误
			write(*,*) 'INPUT ERROR: IO',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)
		endif
		if(char(1:1) /= '!' .and. char/=char_check) then !不是注释行且标题不匹配
			 write(*,*) 'INPUT ERROR: ',char_check,'  , ENTER <CR> TO EXIT'
			pause
			call exit(0)
		endif
		if(char == char_check) then !如果标题正确,回退一行
			backspace(NIT)
			exit
		end if
	end do

	! 读数据	
	read(NIT,*,iostat=status) char,str
	if(status >0 ) then ! IO错误
		write(*,*) 'INPUT ERROR: DATA',' , ENTER <CR> TO EXIT'
		pause
		call exit(0)
	endif	
	
	return		
end subroutine ReadStr


