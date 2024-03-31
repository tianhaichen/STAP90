module module_file !定义输入输出文件
	implicit none
	integer,parameter::NIT=11   ! 输入文件号
	integer,parameter::NOTD=12  ! 输出文件号:位移
	integer,parameter::NOTV=13  ! 输出文件号:速度
	integer,parameter::TAPE1=19 ! 中间过程文件,存放读入的模态文件
	integer,parameter::TAPE2=29 ! 中间过程文件
end module module_file

module module_time
	implicit none
	real(kind=8)::t,tstart,tend,tdeltat
end module module_time

!module module_grvgrd
!	implicit none
!	real(kind=8),dimension(3)::grd !重力梯度
!end module module_grvgrd

