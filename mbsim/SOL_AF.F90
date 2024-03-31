
module module_sol_af

!===================================以下为数据定义===================================
	implicit none
	integer::ny  !	状态变量维数
	integer::neq ! 增广方程维数
	real(kind=8),dimension(:),pointer::y,ydt         ! 状态变量及其时间导数
	real(kind=8),dimension(:,:),pointer::M           ! 增广方程质量阵
	real(kind=8),dimension(:),pointer::F             ! 增广方程右端项 
	real(kind=8),dimension(:),pointer::C,Dc,Gamm     ! 约束方程右端项
	real(kind=8),dimension(:),pointer::y2d           ! 增广方程的解 
	real(kind=8),parameter::ALPH=40                  ! 增广法的参数\alpha,\beta
	real(kind=8),parameter::BET=40                   ! 增广法的参数\alpha,\beta
	real(kind=8),dimension(:),pointer::ytem          ! Runge-Kutta法的中间数组
!===============================以下为函数和子程序声明===============================

end module module_sol_af


!==============================以下为函数和子程序代码段==============================

!----------------------------------------------------------------------------
!   ConstrSolAf():构造增广法求解模块,赋初值
!          哑    元: 
!          引用变量: module_sol_af,module_system
!          调用过程: 
!          异    常: 1.分配空间错误-退出程序
!----------------------------------------------------------------------------
subroutine ConstrSolAf()
	
	use module_system;	use module_sol_af
	implicit none
	integer::stat_alloc

	
	ny=2*nq
	neq=nq+nlam
	allocate(y(ny),ydt(ny),ytem(ny),M(neq,neq),F(neq),y2d(neq),stat=stat_alloc) ! 为状态变量分配空间
	if(stat_alloc>0) then ! 分配空间错误
		write(*,*) 'subroutine ConstrSolAf error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif

	allocate(C(nlam),Dc(nlam),Gamm(nlam),stat=stat_alloc) ! 为约束方程分配空间
	if(stat_alloc>0) then ! 分配空间错误
		write(*,*) 'subroutine ConstrSolAf error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif

	! -------------------- 赋初值 --------------------
	ydt=0.0
	y(1:nq)=q;	y(nq+1:ny)=qdt
	
	return		
end subroutine ConstrSolAf		

!----------------------------------------------------------------------------
!   DestrSolAf():析构增广法模块,释放内存空间
!          哑    元: 
!          引用变量: module_sol_af
!          调用过程: 
!          异    常:
!----------------------------------------------------------------------------
subroutine DestrSolAf(t,tdeltat)
	use module_sol_af
	IMPLICIT NONE
	real(kind=8),intent(in)::t,tdeltat
	real(kind=8)::tend
	
	deallocate(y,ydt,ytem,M,F,y2d,C,Dc,Gamm)

	return
end subroutine DestrSolAf

!----------------------------------------------------------------------------
!   StepSolAf():增广法积分一步,利用定步长Rugne-Kutta法
!          哑    元: 
!          引用变量: module_sol_af
!          调用过程: SolYdt
!          异    常:
!----------------------------------------------------------------------------
subroutine StepSolAf()
	use module_time;	   use module_sol_af
	use module_system;	use module_file
	implicit none
	integer::k                                   ! 
	real(kind=8),dimension(4)::A
	real(kind=8)::tt
	
	A(1)=tdeltat/2.0
	A(2)=A(1)
	A(3)=tdeltat
	A(4)=tdeltat
	tt=t

	CALL SolYdt	
	ytem=y
	DO K=1,3,1
		y=y+A(k)*ydt
		ytem=ytem+A(k+1)*ydt/3.0
	   t=tt+A(k)
	  CALL SolYdt
	end do
	y=ytem+tdeltat*ydt/6.0

	write(*,*) t
	!--------------------- 输出结果到数据文件---------------------
	write(NOTD,FMT='(<nq+1>E17.8)') T,(y(k),k=1,nq)
	write(NOTV,FMT='(<nq+1>E17.8)') T,(y(k),k=nq+1,ny)
	
	return
end subroutine StepSolAf


!--------------------------------------------------------------------------------------------
!   SolYdt():计算ydt
!          哑    元:
!          引用变量: module_sol_af
!          调用过程: 
!          异    常:
!          附    注:调用IMSL中求解线性方程组子程序
!----------------------------------------------------------------------------------------------
subroutine SolYdt 
	use imsl
	use module_sol_af
	use module_time
	use module_system
	implicit none
	integer::n
	
	! -------------------- 传递数值至系统变量 -------------------- 		 
	q=y(1:nq)          ! y=>q  
	qdt=y(nq+1:ny)     ! ydt=>qdt

	! -------------------- 组装增广方程 -------------------- 		 
	M=0.0; 
	call GenM(M(1:nq,1:nq))       ! 动力学方程的质量阵部分
	call SolCq(M(nq+1:neq,1:nq))  ! 计算约束方程的雅可比矩阵
	call SolGamma(Gamm )      ! 计算加速度约束方程的右端项Gamma
	call SolCDc(C,Dc)         ! 计算约束方程和速度约束方程的值
	M(1:nq,nq+1:neq)=transpose(M(nq+1:neq,1:nq)) !CqT 
	call GenF(F(1:nq))         ! 动力学方程中的力项  
	F(nq+1:neq)=Gamm-2.0*ALPH*Dc-BET*BET*C ! 约束方程的右端
	! -------------------- 计算x2d和lam -------------------- 		 
	y2d=0.0
	call erset(0,1,-1) ! IMSL异常处理
	call dlsarg(neq,M,neq,F,1,y2d)
	! -------------------- 写入ydt -------------------- 		 
	ydt(1:nq)=y(nq+1:ny)
	ydt(nq+1:ny)=y2d(1:nq)

	return
end subroutine SolYdt 