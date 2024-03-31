program MBSim
	
	use module_file;	use module_sol_af; use module_system
	use module_time

	implicit none
	integer::method	! 求解方法:1-增广法
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
!  积分方法及时间步长 	
!---------------------------------------------------------------------
	call	ReadIm(temi(1:1),1,'METHOD')	! 读入求解方法 
	method=temi(1)
	
	call ReadFm(temf(1:3),3,'TIME') ! 读入时间	
	tstart=temf(1)
	tend=temf(2)
	tdeltat=temf(3)

!---------------------------------------------------------------------
!  初始化 	
!---------------------------------------------------------------------
	call ConstrSys()	!构造多体系统：配置内存空间，读参数	
	select case(method)	! 初始化不同的求解方法
	case(1)
			call ConstrSolAf()	!为增广法求解配置内存空间,读入积分参数，设置初值
	case(2)
		!	其他方法		      
	end select


!---------------------------------------------------------------------
!  时间积分
!---------------------------------------------------------------------
	
	t=tstart
	do while(t<tend)
		select case(method)	  ! 根据不同的算法积分一步
		case(1)
				CALL StepSolAf() !增广法积分一步
		case(2)
			!	其他方法		      
		end select
	end do

   close (NIT)
   close (NOTD)
	close (NOTV)
	close (TAPE1,STATUS='DELETE')
	close (TAPE2,STATUS='DELETE')
	
	select case(method)	  ! 析构求解方法	
	case(1)
		call DestrSolAf(t,tdeltat)	! 析构增广法：释放空间
	case(2)
		!	其他方法		      
	end select
	call DestrSys()	! 析构多体系统：释放空间
	
	stop
end program MBSim