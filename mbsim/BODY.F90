!===================================以下为模块定义===================================
module module_body
	!==========================以下为数据定义=========================
	implicit none
	type type_body
		integer::numb                                 ! 体号
		integer::btype                                ! 体的类型,0-刚体,1-柔体
		integer::nflex                                ! 体的弹性自由度个数
		real(kind=8),dimension(3)::mc                 ! 质心位置
		real(kind=8),dimension(:),pointer::f          ! 力
      real(kind=8),dimension(:,:),pointer::m        ! 质量阵
		real(kind=8),dimension(3,3)::i1,i2,i7         ! 质量阵,静矩的斜方阵,惯量阵
	!---------------以下为位移及速度------------
		real(kind=8),dimension(:),pointer::disp       ! 广义位移
		real(kind=8),dimension(:),pointer::velo       ! 广义速度
	!---------------以下为旋转------------
		real(kind=8),dimension(3)::omega              ! 体的角速度矢量,相对于体坐标
		real(kind=8),dimension(3)::omega_i            ! 体的角速度矢量,相对于惯性坐标系
		real(kind=8),dimension(3,3)::A                ! A:旋转变换矩阵
		real(kind=8),dimension(3,3)::G                ! G:角速度变换矩阵:d(THETA)/dt->OMEGA
		real(kind=8),dimension(3)::gdtd               ! GDTD:G^dot*THETA^dot
		real(kind=8),dimension(3,3)::G_i              ! G:角速度变换矩阵:d(THETA)/dt->OMEGA_I	
	!---------------以下为柔性体------------
		real(kind=8),dimension(:,:),pointer::i3,i6    !
		real(kind=8),dimension(:,:),pointer::i4       !
		real(kind=8),dimension(:),pointer::b1,b2,b3,b4,b5,b6         !Mcc中弹性位移一次项
		real(kind=8),dimension(:,:),pointer::c11,c22,c33,c12,c13,c23 !Mcc中弹性位移二次项
		real(kind=8),dimension(:,:),pointer::cXY,cYZ,cXZ             !Mcf中弹性位移一次项
      real(kind=8),dimension(:,:),pointer::K        ! 刚度阵
      real(kind=8),dimension(:,:),pointer::C        ! 阻尼阵
	END TYPE TYPE_BODY
	type(type_body),dimension(:),pointer::body       ! 定义N个体 	

	!=====================以下为函数和子程序接口块=======================

end module module_body

!----------------------------------------------------------------------------
!         Rotat(n):计算坐标变换矩阵等与旋转相关的量,转动次序1-2-3
!          哑    元: n - 体号  
!          引用变量: module_body
!          调用过程: 
!          异    常: 
!----------------------------------------------------------------------------
subroutine Rotat(n)
	use module_body
	implicit none
	integer,intent(in)::n
	real(kind=8)::s1,s2,s3,c1,c2,c3

   s1=sin(body(n)%disp(4));   c1=cos(body(n)%disp(4))	
	s2=sin(body(n)%disp(5));   c2=cos(body(n)%disp(5))
	s3=sin(body(n)%disp(6));   c3=cos(body(n)%disp(6))
	
	body(n)%A(1,1)=c2*c3 ;           body(n)%A(1,2)=-c2*s3;            body(n)%A(1,3)=s2
	body(n)%A(2,1)=s1*s2*c3+c1*s3;   body(n)%A(2,2)=-s1*s2*s3+c1*c3;   body(n)%A(2,3)=-s1*c2
	body(n)%A(3,1)=-c1*s2*c3+s1*s3;  body(n)%A(3,2)=c1*s2*s3+s1*c3;    body(n)%A(3,3)=c1*c2

		
	body(n)%G(1,1)= c2*c3;   body(n)%G(1,2)= s3;   body(n)%G(1,3)= 0.0
	body(n)%G(2,1)=-c2*s3;   body(n)%G(2,2)= c3;   body(n)%G(2,3)= 0.0
	body(n)%G(3,1)= s2   ;   body(n)%G(3,2)=0.0;   body(n)%G(3,3)= 1.0

	body(n)%G_i(1,1)=1.0;   body(n)%G_i(1,2)=0.0;   body(n)%G_i(1,3)= s2
	body(n)%G_i(2,1)=0.0;   body(n)%G_i(2,2)= c1;   body(n)%G_i(2,3)=-c2*s1
	body(n)%G_i(3,1)=0.0;   body(n)%G_i(3,2)= s1;   body(n)%G_i(3,3)=c2*c1

	body(n)%omega=MATMUL(body(n)%G,body(n)%velo(4:6))
	body(n)%omega_i=MATMUL(body(n)%G_i,body(n)%velo(4:6))

	body(n)%gdtd(1)=-s2*c3*body(n)%velo(4)*body(n)%velo(5)-c2*s3*body(n)%velo(4)*body(n)%velo(6)+c3*body(n)%velo(5)*body(n)%velo(6)
	body(n)%gdtd(2)= s2*s3*body(n)%velo(4)*body(n)%velo(5)-c2*c3*body(n)%velo(4)*body(n)%velo(6)-s3*body(n)%velo(5)*body(n)%velo(6)
	body(n)%gdtd(3)= c2*body(n)%velo(4)*body(n)%velo(5)

	return
end subroutine Rotat



