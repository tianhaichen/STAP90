!===================================以下为模块定义===================================
module module_mark
	!==========================以下为数据定义=========================
	implicit none
	type type_mark ! 参考点,其上可施加外力
		integer::numm,bom                           ! 参考点号,参考点所在体号
		integer::non                                ! 参考点所在节点号,弹性体
		real(kind=8),dimension(3)::r0,r             ! 参考点位置
		real(kind=8),dimension(:,:),pointer::hat    ! 参考点的平动模态
		real(kind=8),dimension(:,:),pointer::sig    ! 参考点的转动模态 
		real(kind=8),dimension(6)::f                ! 参考点的力和力矩,体坐标系
	END TYPE TYPE_mark
	type(type_mark),dimension(:),pointer::mark       ! 定义NM个参考点

	!=====================以下为函数和子程序接口块=======================

end module module_mark


!==============================以下为函数和子程序代码段==============================

!----------------------------------------------------------------------------
!   ConstrMark(n):构造第n个参考点, 读参考点参数
!          哑    元: n - 参考点号
!          引用变量: module_mark
!          调用过程: 1.ReadFm      - 读实数数组
!          异    常: 1.分配空间错误-退出程序
!                    2.体数据错误  -退出程序
!----------------------------------------------------------------------------
subroutine ConstrMark(n)
	use module_mark; use module_body;	use module_system,only:nb


	mark(n)%numm=n
	call ReadIm(mark(n)%bom,1,'BODY') ! 体号
	if(mark(n)%bom<=0 .or.mark(n)%bom>nb) then ! 体号错误
		write(*,*) 'subroutine ConstrMark error: Body',' , ENTER <CR>  TO EXIT'
		pause
		call exit(0)		
	end if	

	if(body(mark(n)%bom)%btype == 0) then ! 体为刚体
		call ReadFm(mark(n)%r,3,'LOCA')  ! 读入参考点在体的位置(体坐标系)
		nullify(mark(n)%hat,mark(n)%sig) ! 不需要弹性模态
	else ! 内接体为柔性体
		call ReadIm(mark(n)%non,1,'LOCA') ! 读入参考点在体的节点号
	end if
	
	return
end subroutine ConstrMark

!----------------------------------------------------------------------------
!   DestrMark(n):析构第n个参考点,释放内存
!          哑    元: n - 参考点号
!          引用变量: module_mark;module_body
!          调用过程: 
!          异    常: 
!----------------------------------------------------------------------------
subroutine DestrMark(n)
	use module_mark;	use module_body
	
	if(body(mark(n)%bom)%btype==1) then !如果参考点所在的体为柔性体，释放模态指针
		deallocate(mark(n)%hat,mark(n)%sig)
	endif 

	return
end subroutine DestrMark

