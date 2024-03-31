!----------------------------------------------------------------------------
!   ConstrRevo(n):构造第n个铰(柱铰), 读铰参数
!          哑    元: n - 铰号
!          引用变量: module_hinge;modle_body;module_system(nb)
!          调用过程: 1.ReadIm      - 读整数数组
!                    2.ReadFm      - 读实数数组
!          异    常: 1.分配空间错误-退出程序
!                    2.体数据错误  -退出程序
!----------------------------------------------------------------------------
subroutine ConstrRevo(n)
	use module_hinge;	use module_body;	use module_system,only:nb
	implicit none
	integer,intent(in)::n			
	integer,dimension(6)::vi
	integer::stat_alloc

	
	hinge(n)%numh=n
	hinge(n)%htype='REVOLUTE'

	call ReadIm(vi(1:2),2,'BODY') ! 内外接体号
	hinge(n)%inb=vi(1);	hinge(n)%exb=vi(2) 
	if(hinge(n)%inb<0 .or.hinge(n)%inb>nb) then ! 内接体号错误
		write(*,*) 'subroutine ConstrRevo error: Body',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)		
	end if	
	if(hinge(n)%exb<0 .or.hinge(n)%exb>nb) then ! 外接体号错误
		write(*,*) 'subroutine ConstrRevo error: Body',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)		
	end if	

	! ----------------------  内接体 ----------------------
	if(hinge(n)%inb == 0) then ! 基础
		call ReadFm(hinge(n)%rp,3,'LOCA1') ! 读入铰点在基础的位置
		nullify(hinge(n)%hatp,hinge(n)%sigp) ! 不需要弹性模态
		nullify(hinge(n)%cqi)                ! 不需要Cqi
	else ! 铰点在体上
		if(body(hinge(n)%inb)%btype == 0) then ! 内接体为刚体
			call ReadFm(hinge(n)%rp,3,'LOCA1')  ! 读入铰点在内接体的位置(体坐标系)
			nullify(hinge(n)%hatp,hinge(n)%sigp) ! 不需要弹性模态
			allocate(hinge(n)%cqi(5,6),stat=stat_alloc)   ! 刚体的Cqi
			if(stat_alloc>0) then ! 分配空间错误
				write(*,*) 'subroutine ConstrRevo error: Allocate',' , ENTER <CR>  TO EXIT'
				pause
				call exit(0)
			endif
		else ! 内接体为柔性体
			call ReadIm(hinge(n)%nin,1,'LOCA1') ! 读入铰点在内接体的节点号
!        弹性体的Cqi
		end if
	endif
	! ----------------------  外接体 ----------------------
	if(hinge(n)%exb == 0) THEN        ! 基础
		call ReadFm(hinge(n)%rq,3,'LOCA2') ! 读入铰点在基础的位置
		nullify(hinge(n)%hatq,hinge(n)%sigq) ! 不需要弹性模态
		nullify(hinge(n)%cqj)                         ! 不需要Cqj
	else ! 铰点在体上
		if(body(hinge(n)%exb)%btype == 0) then ! 外接体为刚体
			call ReadFm(hinge(n)%rq,3,'LOCA2')  ! 读入铰点在外接体的位置(体坐标系)
			nullify(hinge(n)%hatq,hinge(n)%sigq) ! 不需要弹性模态
			allocate(hinge(n)%cqj(5,6),stat=stat_alloc)   ! 刚体的Cqj
			if(stat_alloc>0) then ! 分配空间错误
				write(*,*) 'subroutine ConstrRevo error: Allocate',' , ENTER <CR>  TO EXIT'
				pause
				call exit(0)
			endif
		else ! 外接体为柔性体
			call ReadIm(hinge(n)%nex,1,'LOCA2') ! 读入铰点在外接体的节点号
!        弹性体的Cqj
		end if
	endif


	allocate(hinge(n)%c(5),hinge(n)%dc(5),hinge(n)%gamma(5),stat=stat_alloc)   ! 约束方程
	if(stat_alloc>0) then ! 分配空间错误
		write(*,*) 'subroutine ConstrRevo error: Allocate',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)
	endif
		
	
	! ----------------------  读转轴矢量 ----------------------	
	call ReadFm(hinge(n)%da,3,'DA')
	call ReadFm(hinge(n)%db1,3,'DB1')
	call ReadFm(hinge(n)%db2,3,'DB2')

	return
end subroutine ConstrRevo

!----------------------------------------------------------------------------
!   DestrRevo(n):析构第n个铰(柱铰),释放内存
!          哑    元: n - 铰号
!          引用变量: module_hinge;modle_body
!          调用过程: 
!          异    常: 
!----------------------------------------------------------------------------
subroutine DestrRevo(n)
	use module_hinge;	use module_body
	implicit none
	integer,intent(in)::n			
	
	if(hinge(n)%inb /= 0 .and. body(hinge(n)%inb)%btype==1) then ! 内接体不是基础且为弹性体
		deallocate(hinge(n)%hatp,hinge(n)%sigp) ! 不需要弹性模态
	endif

	if(hinge(n)%exb /= 0 .and. body(hinge(n)%exb)%btype==1) then ! 外接体不是基础且为弹性体
		deallocate(hinge(n)%hatq,hinge(n)%sigq) ! 不需要弹性模态
	endif
	

	return
end subroutine DestrRevo


!----------------------------------------------------------------------------
!   SolCqRevo(n):计算第n个铰(柱铰)的雅可比矩阵
!          哑    元: n - 铰号
!          引用变量: module_hinge;modle_body
!          调用过程: 
!          异    常: 
!----------------------------------------------------------------------------
subroutine SolCqRevo(n)
	use module_hinge;	use module_body
	implicit none
	integer,intent(in)::n			
	integer::i,j,nb1,nb2
	real(kind=8),dimension(3)::dap,db1p,db2p    ! DA,DB的惯性系坐标阵	
	real(kind=8),dimension(3)::ctp,ctq,v
	real(kind=8),dimension(3,3)::xm,xn,om,on
	
	
	nb1=hinge(n)%inb;	nb2=hinge(n)%exb
	! ---------------------- 计算转轴矢量  ----------------------	 
	call SolVec(nb1,hinge(n)%da,dap)
	call Solvec(nb2,hinge(n)%db1,db1p)
	call Solvec(nb2,hinge(n)%db2,db2p)

	! ---------------------- 内接体:Cqi  ----------------------	 
	if(nb1 /=0) then ! 内接体不是基础
		hinge(n)%cqi=0.0
		if(body(nb1)%btype ==0 ) then !内接体为刚体 
			!-------------------Cqi:平动[I,-A*u^~*G'] 
			hinge(n)%cqi(1,1)=1.0;	hinge(n)%cqi(2,2)=1.0;	hinge(n)%cqi(3,3)=1.0
			call skew(hinge(n)%rp,xm)
			hinge(n)%cqi(1:3,4:6)=-1.0*matmul(matmul(body(nb1)%A,xm),body(nb1)%G)  !CITA
			!-------------------Cqi转动1:-db1p^T*dap^~*G_i=-db1p^T*A*da*A^T*A*G=-db1p^T*A*da*G
			call skew(hinge(n)%da,xm)
			hinge(n)%cqi(4,4:6)=matmul(matmul(-db1p,body(nb1)%A),matmul(xm,body(nb1)%G)) 
			!-------------------Cqi:转动2-------------------
			hinge(n)%cqi(5,4:6)=matmul(matmul(-db2p,body(nb1)%A),MATMUL(xm,body(nb1)%G))
		else ! 内接体为弹性体
		! 计算铰坐标系的转角及转轴矢量的惯性系坐标列阵
		! 计算雅可比矩阵
		endif
	endif
	! ---------------------- 外接体:Cqj ----------------------	 
	if(nb2 /=0 ) then ! 外接体不是基础
		hinge(n)%cqj=0.0
		if(body(nb2)%btype ==0 ) then !外接体为刚体 
			hinge(n)%cqj=0.0
			!-------------------Cqj:平动-[I,-A*u^~*G'] 
			hinge(n)%cqj(1,1)=-1.0;	hinge(n)%cqj(2,2)=-1.0;	hinge(n)%cqj(3,3)=-1.0
			call skew(hinge(n)%rq,xn)
			hinge(n)%cqj(1:3,4:6)=1.0*matmul(matmul(body(nb2)%A,xn),body(nb2)%G)  !CITA
			!-------------------Cq:转动1-------------------
			call skew(hinge(n)%db1,xn)
			hinge(n)%cqj(4,4:6)=matmul(matmul(dap,body(nb2)%A),matmul(xn,body(nb2)%G))
			!-------------------Cq:转动2-------------------
			call skew(hinge(n)%db2,xn)
			hinge(n)%cqj(5,4:6)=matmul(matmul(dap,body(nb2)%A),matmul(xn,body(nb2)%G))
		else ! 外接体为弹性体
		! 计算铰坐标系的转角及转轴矢量的惯性系坐标列阵 
		! 计算雅可比矩阵 
		endif
	endif			

	return
end subroutine SolCqRevo


!----------------------------------------------------------------------------
!   SolGammaRevo(n):计算第n个铰(柱铰)的加速度约束方程的右端项:Gamma
!          哑    元: n - 铰号
!          引用变量: module_hinge;modle_body
!          调用过程: 
!          异    常: 
!----------------------------------------------------------------------------
subroutine SolGammaRevo(n)
	use module_hinge;	use module_body
	implicit none
	integer,intent(in)::n			
	integer::i,j,nb1,nb2
	real(kind=8),dimension(3)::dap,db1p,db2p    ! DA,DB的惯性系坐标阵	
	real(kind=8),dimension(3)::vdap,vdb1p,vdb2p ! DA,DB的速度惯性系坐标阵	
	real(kind=8),dimension(3)::ctp,ctq,v
	real(kind=8),dimension(3,3)::xm,xn,om,on
	
	
	nb1=hinge(n)%inb;	nb2=hinge(n)%exb
	hinge(n)%gamma=0.0	

	! ---------------------- 计算转轴矢量  ----------------------	 
	call SolVec(nb1,hinge(n)%da,dap)
	call Solvec(nb2,hinge(n)%db1,db1p)
	call Solvec(nb2,hinge(n)%db2,db2p)

	
	!------------------- 计算约束方程右端项 -----------------
	if(nb1==0) then ! 内接体是基础
		hinge(n)%gamma(1:3)=0.0	     ! Gamma:平动
		vdap=0.0
		hinge(n)%gamma(4)=0.0
		hinge(n)%gamma(5)=0.0
	else  ! 内接体不是基础
		if(body(nb1)%btype ==0 ) then !内接体为刚体
			!------------------- Gamma:平动 -------------------
			call skew(body(nb1)%omega,om)  !omega^~
			call skew(hinge(n)%rp,xm)      !R^~
			hinge(n)%gamma(1:3)=matmul(body(nb1)%A,matmul(xm,body(nb1)%gdtd)) & !A*R^~*GDTD
									  -matmul(matmul(body(nb1)%A,om),matmul(om,hinge(n)%rp))	!A*OMEGA^~*OMEGA^~*R
			!------------------- 转轴矢量速度:-dap^~*omega_i=-A*da^~*A^T*A*omega=-A*da^~*omega
			call skew(hinge(n)%da,xm)
			vdap=-matmul(matmul(body(nb1)%A,xm),body(nb1)%omega)
			!------------------- Gamma:转动 ------------------		
			hinge(n)%gamma(4)=-dot_product(db1p,matmul(body(nb1)%A,matmul(om,matmul(om,hinge(n)%da)))) 
			hinge(n)%gamma(5)=-dot_product(db2p,matmul(body(nb1)%A,matmul(om,matmul(om,hinge(n)%da)))) 
		else
		!计算柔性体	     
		endif
	endif

	if(nb2 ==0 ) then ! 外接体是基础
		hinge(n)%gamma(1:3)=-1.0*hinge(n)%gamma(1:3)      ! Gamma:平动
		hinge(n)%gamma(4)=-1.0*hinge(n)%gamma(4)
		hinge(n)%gamma(5)=-1.0*hinge(n)%gamma(5)
	else ! 外接体不是基础
		if(body(nb2)%btype ==0 ) then !外接体为刚体 
			!------------------- Gamma:平动 -------------------
			call skew(body(nb2)%omega,on)  !omega^~
			call skew(hinge(n)%rq,xn)      !R^~
			hinge(n)%gamma(1:3)=hinge(n)%gamma(1:3)-matmul(body(nb2)%A,matmul(xn,body(nb2)%gdtd)) & !A*R^~*GDTD
									  +matmul(matmul(body(nb2)%A,on),matmul(on,hinge(n)%rq))	!A*OMEGA^~*OMEGA^~*R
			!------------------- 转轴矢量速度 ------------------			
			call skew(hinge(n)%db1,xn)
			vdb1p=-matmul(matmul(body(nb2)%A,xn),body(nb2)%omega)
			call skew(hinge(n)%db2,xn)
			vdb2p=-matmul(matmul(body(nb2)%A,xn),body(nb2)%omega)
			!------------------- Gamma:转动 ------------------		
			hinge(n)%gamma(4)=hinge(n)%gamma(4)-dot_product(dap,matmul(body(nb2)%A,matmul(on,matmul(on,hinge(n)%db1)))) 
			hinge(n)%gamma(5)=hinge(n)%gamma(5)-dot_product(dap,matmul(body(nb2)%A,matmul(on,matmul(on,hinge(n)%db2)))) 
		else ! 外接体为弹性体
		! 计算柔性体	     
		endif
	endif			

	!------------------- Gamma:转动 ------------------			
	hinge(n)%gamma(4)=hinge(n)%gamma(4)-2.0*dot_product(vdap,vdb1p)
	hinge(n)%gamma(5)=hinge(n)%gamma(5)-2.0*dot_product(vdap,vdb2p)
		
	return
end subroutine SolGammaRevo		

!----------------------------------------------------------------------------
!   SolCDcRevo(n):计算第n个铰(柱铰)的约束方程和速度约束方程的值
!          哑    元: n - 铰号
!          引用变量: module_hinge;modle_body
!          调用过程: 
!          异    常: 
!----------------------------------------------------------------------------
subroutine SolCDcRevo(n)
	use module_hinge;	use module_body
	implicit none
	integer,intent(in)::n			
	integer::i,j,nb1,nb2
	real(kind=8),dimension(3)::dap,db1p,db2p    ! DA,DB的惯性系坐标阵	
	real(kind=8),dimension(3)::vdap,vdb1p,vdb2p ! DA,DB的速度惯性系坐标阵	
	real(kind=8),dimension(3)::ctp,ctq,v
	real(kind=8),dimension(3,3)::xm,xn,om,on
	
	
	nb1=hinge(n)%inb;	nb2=hinge(n)%exb
	hinge(n)%c=0.0;	hinge(n)%dc=0.0

	! ---------------------- 计算转轴矢量  ----------------------	 
	call SolVec(nb1,hinge(n)%da,dap)
	call Solvec(nb2,hinge(n)%db1,db1p)
	call Solvec(nb2,hinge(n)%db2,db2p)

	!------------------- 计算约束方程右端项 -----------------
	if(nb1==0) then ! 内接体是基础
		hinge(n)%c(1:3)=hinge(n)%rp  ! C:平动
		hinge(n)%Dc=0.0
	else  ! 内接体不是基础
		if(body(nb1)%btype ==0 ) then !内接体为刚体
			!------------------- C:平动 -------------------
			hinge(n)%c(1:3)=body(nb1)%disp(1:3)+matmul(body(nb1)%A,hinge(n)%rp)	
			!------------------- Dc ------------------
			hinge(n)%Dc=matmul(hinge(n)%cqi,body(nb1)%velo)
		else
		!计算柔性体	     
		endif
	endif
	if(nb2 ==0 ) then ! 外接体是基础
		hinge(n)%c(1:3)=hinge(n)%c(1:3)-hinge(n)%rq  ! C:平动
		hinge(n)%Dc=hinge(n)%Dc
	else ! 外接体不是基础
		if(body(nb2)%btype ==0 ) then !外接体为刚体 
			!------------------- C:平动 -------------------
			hinge(n)%c(1:3)=hinge(n)%c(1:3)-body(nb2)%disp(1:3)-matmul(body(nb2)%A,hinge(n)%rq)	
			!------------------- Dc ------------------
			hinge(n)%Dc=hinge(n)%Dc+matmul(hinge(n)%cqj,body(nb2)%velo)
		else ! 外接体为弹性体
		! 计算柔性体	     
		endif
	endif			
	!------------------- C:转动 -------------------
	hinge(n)%c(4)=dot_product(dap,db1p)
	hinge(n)%c(5)=dot_product(dap,db2p)

		
	return
end subroutine SolCDcRevo		
