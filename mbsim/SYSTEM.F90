!===================================以下为模块定义===================================
module module_system

	!=========================以下为数据定义=========================
	use module_body
	use module_hinge
	use module_mark
	implicit none
	integer::nb,nh,nm                      ! 体数、铰点数、参考点数   
	integer::nq                            ! 系统的广义坐标数
	integer::nlam                          ! 拉氏乘子个数
	integer,dimension(:),pointer::locq     ! 广义坐标位置数组：定义每一个体广义坐标在系统广义坐标中的位置
	integer,dimension(:),pointer::locc     ! 约束位置数组：定义每一个约束在系统约束方程中的位置
	real(kind=8),dimension(:),pointer::q,qdt   ! 广义坐标,广义速度
	real(kind=8),dimension(:),pointer::lam     ! 拉氏乘子
	real(kind=8),dimension(3)::grd             !重力梯度


	!=====================以下为函数和子程序接口块=======================

end module module_system


!==============================以下为函数和子程序代码段==============================

!----------------------------------------------------------------------------
!   ConstrSys:构造系统模块
!             分配内存空间 ; 构造体模块 ; 构造铰模块 ; 构造参考点模块 
!          哑    元: 
!          引用变量: module_system(module_body,module_hinge,module_mark)
!          调用过程: 1.ReadIm      - 读整形数组
!                    2.ReadStr     - 字符串
!                    3.ConstrRigid - 构造刚体
!                    4.ConstrRevo  - 构造柱铰
!                    5.ConstrMak   - 构造参考点
!          异    常: 1.分配空间错误-退出程序
!                    2.体号、铰号、参考点号不匹配-退出程序
!                    3.体、铰类型错误-退出程序
!----------------------------------------------------------------------------
subroutine ConstrSys()
	use module_system
	implicit none
	integer::stat_alloc,i,j
	integer::qstart,qend
	character(len=10)::str


	!------------------------读入系统拓扑参数------------------------		
	call ReadIm(nb,1,'NBODY')  ! 体数
	call ReadIm(nh,1,'NHINGE') ! 铰数
	call ReadIm(nm,1,'NMARK')  ! 参考点数

	
	!------------------------分配内存空间------------------------
	allocate(locq(nb),locc(nh),stat=stat_alloc)     ! 广义坐标位置数组 
	if(stat_alloc>0) then ! 分配空间错误
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif

	allocate(body(nb),stat=stat_alloc)	  ! 体
	if(stat_alloc>0) then ! 分配空间错误
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif
	
	allocate(hinge(nh),stat=stat_alloc)    ! 铰
	if(stat_alloc>0) then ! 分配空间错误
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif
	
	if(nm>=1) allocate(mark(nm),stat=stat_alloc)    ! 参考点
	if(stat_alloc>0) then ! 分配空间错误
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif

	!------------------------构建体------------------------
	do i=1,nb,1
		call ReadIm(j,1,'NOB') ! 读体号
		if(j /= i) then !体号不匹配
			write(*,*) 'Input error: NOB',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)		
		end if  
		call ReadIm(j,1,'BTYPE') ! 读体类型
		if(j /= 0 .and. j /= 1) then !体类型错误
			write(*,*) 'Input error: BTYPE',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)		
		end if 
		select case(j)
		case(0) ! 构造刚体
			call ConstrRigid(i) 
		case(1) ! 构造柔性体
!			call ConstrFlex(i)
		end select
	end do
	
	!------------------------构建铰------------------------
	do i=1,nh,1
		call ReadIm(j,1,'NOH') ! 读铰号
		if(j /= i) then !铰号不匹配
			write(*,*) 'Input error: NOH',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)		
		end if  
		call ReadStr(str,'TYPE') ! 读铰类型
		
		select case(str)
		case('REVOLUTE') ! 柱铰
			call ConstrRevo(i)	
		case default  ! 未知铰类型
			write(*,*) 'Input error: HTYPE',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)					
		end select
	end do

	!------------------------构建参考点------------------------
	do i=1,nm,1
		call ReadIm(j,1,'NOM') ! 读参考号
		if(j /= i) then !参考号不匹配
			write(*,*) 'Input error: NOM',' , ENTER <CR>  TO EXIT'
			pause
			call exit(0)		
		end if  
		call ConstrMark(i) !构造参考点	
	end do

	!------------------------读重力加速度g------------------------
	call ReadFm(grd,3,'GRVGR') ! 读参考号


	!----------  计算系统自由度数;确定体的广义坐标在系统广义坐标阵中的位置:locq ----------
	nq=6+body(1)%nflex
	locq(1)=1
	do i=2,nb
		locq(i)=locq(i-1)+body(i-1)%nflex+6  ! 确定向量位置
		nq=nq+6+body(i)%nflex                ! 系统总自由度  
	end do

	!----------------------------------  计算拉氏乘子个数 ----------------------------------
	nlam=0
	locc(1)=1
	select case(hinge(1)%htype)
	case('REVOLUTE') ! 柱铰
		nlam=nlam+5
	case('FIXED') !固定约束
			! 其它约束
	end select
	do i=2,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! 柱铰
			nlam=nlam+5
			locc(i)=locc(i-1)+5
		case('FIXED') !固定约束
			! 其它约束
		end select
	end do

	allocate(q(nq),qdt(nq),lam(nlam),stat=stat_alloc)     ! 广义坐标、广义速度、拉氏乘子 
	if(stat_alloc>0) then ! 分配空间错误
		write(*,*) 'subroutine ConstrSys error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif
	
	!-----------------------------  体的广义坐标=>系统广义坐标 -----------------------------
	do i=1,nb,1
		qstart=locq(i)
		qend=locq(i)+5+body(i)%nflex
		! ---------- 计算坐标变换矩阵等 ----------
		q(qstart:qend)=body(i)%disp    ! 体的广义坐标=>系统广义坐标
		qdt(qstart:qend)=body(i)%velo  ! 体的广义速度=>系统广义速度
	end do
	
	return
end subroutine ConstrSys

!----------------------------------------------------------------------------
!   DestrSys:析构系统模块
!            释放内存空间 ; 析构体模块 ; 析构铰模块 ; 析构参考点模块
!          哑    元: 
!          引用变量: module_system(module_body,module_hinge,module_mark)
!          调用过程: 1.DestrRigid - 析构刚体
!                    2.DestrRevo  - 析构柱铰
!                    3.DestrMak   - 析构参考点
!          异    常: 
!----------------------------------------------------------------------------
subroutine DestrSys()
	use module_system
	implicit none
	integer::i

	!------------------------析构参考点------------------------
	do i=1,nm,1
		call DestrMark(i) !析构参考点	
	end do

	!------------------------析构铰------------------------
	do i=1,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! 析构柱铰
			call DestrRevo(i)	
		case('FIXED')  ! 析构固定约束
!			call DestrFixed(i) 	
		end select
	end do


	!------------------------析构体------------------------
	do i=1,nb,1
		select case(body(i)%btype)
		case(0) ! 析构刚体
			call DestrRigid(i) 
		case(1) ! 析构柔性体
!			call DestrFlex(i)
		end select
	end do


	deallocate(locq,locc)     ! 广义坐标位置数组 
	deallocate(body)	  ! 体
	deallocate(hinge)    ! 铰
	if(nm>=1) deallocate(mark)    ! 参考点

	deallocate(q,qdt,lam)     ! 广义坐标、广义速度、拉氏乘子 
end subroutine DestrSys



!----------------------------------------------------------------------------
!   GenM(m_sys):生成系统动力学方程的质量阵
!               计算每一个体的质量阵，组装系统质量阵，存入哑元m_sys中
!          哑    元: m_sys
!          引用变量: module_system(module_body)
!          调用过程: 1.GenRigidM(i) - 第i个体的质量阵(刚体)
!                    2.GenFlexM(i)  - 第i个体的质量阵(柔体)
!          异    常: 
!----------------------------------------------------------------------------
subroutine GenM(m_sys)
	use module_system
	implicit none
	real(kind=8),dimension(nq,nq),intent(out)::m_sys
	integer::i,rstart,rend,cstart,cend

	m_sys=0.0
	do i=1,nb,1
		! ---------- 体质量阵在系统质量阵的位置 ----------
		rstart=locq(i)
		rend=locq(i)+5+body(i)%nflex
		cstart=rstart
		cend=rend
		! ---------- 计算坐标变换矩阵等 ----------
		body(i)%disp=q(rstart:rend)    ! 由系统广义坐标赋值给体的广义坐标
		body(i)%velo=qdt(rstart:rend)  ! 由系统广义速度赋值给体的广义速度
		call Rotat(i)      ! 计算坐标变换矩阵等
		! ---------- 分类计算质量阵 ----------
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
!   GenF(f_sys):生成系统动力学方程的右端项
!               计算每一个体广义惯性力,外力,组装系统力项，存入哑元f_sys中
!          哑    元: f_sys
!          引用变量: module_system(module_body)
!          调用过程: 1.GenRigidF(i) - 第i个体的力(刚体)
!                    2.GenFlexF(i)  - 第i个体的力(柔体)
!          异    常: 
!----------------------------------------------------------------------------
subroutine GenF(f_sys)
	use module_system
	implicit none
	real(kind=8),dimension(nq),intent(out)::f_sys
	integer::i,nob,rstart,rend
	real(kind=8),dimension(3,3)::ske

	f_sys=0.0

	do i=1,nb,1
		! ---------- 体力项在系统力项的位置 ----------
		rstart=locq(i)
		rend=locq(i)+5+body(i)%nflex
		! ---------- 分类计算力项 ----------
		select case(body(i)%btype)
		case(0)
			call GenRigidF(i)
		case(1)
!			call GenFlexF(i)
		end select				
		f_sys(rstart:rend)=body(i)%f
	end do
	
	! -------------------- 计算外力 --------------------
	do i=1,nm,1
		call ExterForce
		nob=mark(i)%bom
		
		if(body(nob)%nflex==1) then
!			call SolMarkR(i) ! 计算参考点的位置			 
!			计算弹性振动的广义外力
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
!         SolCq(Cq):生成系统约束方程的雅可比阵
!          哑    元: Cq
!          引用变量: module_system(module_hinge)
!          调用过程: 1.SolCqRevo(i) - 计算第i个铰(柱铰)
!                    2. ...
!          异    常: 
!----------------------------------------------------------------------------
subroutine SolCq(Cq)
	use module_system
	implicit none
	real(kind=8),dimension(nlam,nq)::Cq
	real(kind=8),dimension(nlam)::C,Dc,Gamma
	integer::i,nb1,nb2,lb1,lb2


	! -------------------- 计算每一个铰  --------------------
	do i=1,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! 柱铰
			call SolCqRevo(i)	 
			! -------------------- 组装系统Cq  --------------------
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
		case('FIXED') ! 固定约束
		! 
		end select
	end do
	
	return
end subroutine SolCq


!----------------------------------------------------------------------------
!   SolGamma(Gamma):生成系统加速度约束方程的右端项：Gamma
!          哑    元: Gamma
!          引用变量: module_system(module_hinge)
!          调用过程: 1.SolGammaRevo(i) - 计算第i个铰(柱铰)
!                    2. ...
!          异    常: 
!----------------------------------------------------------------------------
subroutine SolGamma(Gamma)
	use module_system
	implicit none
	real(kind=8),dimension(nlam)::Gamma
	integer::i


	! -------------------- 计算每一个铰  --------------------
	do i=1,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! 柱铰
			call SolGammaRevo(i)	 
			Gamma(locc(i):locc(i)+4)=hinge(i)%gamma	!  组装系统gamma 
		case('FIXED') ! 固定约束
		! 
		end select
	end do
	
	return
end subroutine SolGamma


!----------------------------------------------------------------------------
!   SolCDc(C,Dc):计算约束方程和速度约束方程的值:Dc,C
!          哑    元: Dc,C
!          引用变量: module_system(module_hinge)
!          调用过程: 1.SolCDcRevo(i) - 计算第i个铰(柱铰)
!                    2. ...
!          异    常: 
!----------------------------------------------------------------------------
subroutine SolCDc(C,Dc)
	use module_system
	implicit none
	real(kind=8),dimension(nlam)::C,Dc
	integer::i


	! -------------------- 计算每一个铰  --------------------
	do i=1,nh,1
		select case(hinge(i)%htype)
		case('REVOLUTE') ! 柱铰
			call SolCDcRevo(i)	 
			C(locc(i):locc(i)+4)=hinge(i)%C	   !  组装系统C 
			Dc(locc(i):locc(i)+4)=hinge(i)%Dc	!  组装系统Dc
		case('FIXED') ! 固定约束
		! 
		end select
	end do
	
	return
end subroutine SolCDc


