!===================================以下为模块定义===================================
module module_hinge
	!==========================以下为数据定义=========================
	implicit none
	type type_hinge
		integer::numh,inb,exb                          !铰点号,铰点类型,内接体号,外接体号
		integer::nin,nex                               !铰点在内外接体上的节点号 
		character(len=10)::htype                       !铰点类型, 
		real(kind=8),dimension(3)::rp0,rq0,rp,rq       !铰点位置:RP=P点相对于内体系,RQ=Q点相对于外体系,RP0、RQ0未变形时的位置
		real(kind=8),dimension(3)::da,da1,db1,db2      !转轴矢量在随体坐标系中的坐标列阵
		real(kind=8),dimension(:,:),pointer::hatp,hatq !铰点的平动模态
		real(kind=8),dimension(:,:),pointer::sigp,sigq !铰点的转动模态 
		real(kind=8),dimension(:,:),pointer::cqi,cqj   !约束的雅可比
		real(kind=8),dimension(:),pointer::gamma       !约束的右端项 
		real(kind=8),dimension(:),pointer::c,dc        !约束方程和速度约束方程的值
	end type type_hinge
	type(type_hinge),dimension(:),pointer::hinge      ! 定义NH个铰

	!=====================以下为函数和子程序接口块=======================

end module module_hinge

!----------------------------------------------------------------------------
!   SolVec(n,vf,vi):计算转轴矢量在惯性系的列阵
!          哑    元: n - 体号;
!                    vf-矢量在随体系的列阵
!                    vi-矢量在惯性系的列阵
!          引用变量: module_hinge;modle_body
!          调用过程: 
!          异    常: 
!          注    释: 需已计算体的旋转矩阵等
!----------------------------------------------------------------------------
subroutine SolVec(n,vf,vi)
	use module_hinge;	use module_body
	implicit none
	integer,intent(in)::n			
	real(kind=8),dimension(3),intent(in)::vf
	real(kind=8),dimension(3),intent(out)::vi

	if(n==0) then !基础
		vi=vf
	else 
		if(body(n)%btype==0) then ! 刚体
			vi=matmul(body(n)%A,vf)
		else ! 弹性体
		!	
		endif
	endif

	return
end subroutine SolVec



