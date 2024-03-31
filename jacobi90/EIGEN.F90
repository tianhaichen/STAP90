program eigen
implicit none

integer, parameter :: N = 3
real(8), parameter :: RTOL = 1.0E-12

real(8) :: K(N,N), M(N,N), V(N,N), L(N), W(N)

!data K/ 2.0, 1.0, 1.0, 2.0/
!data M/ 4.0,-3.0,-3.0, 4.0/

!data K/ 2.0,-1.0, 0.0, 0.0, &
!       -1.0, 0.0,-1.0, 0.0, &
!        0.0,-1.0, 2.0,-1.0, &
!		 0.0, 0.0,-1.0, 0.0 /
!data M/ 1.0, 0.0, 0.0, 0.0, &
!        0.0, 1.0, 0.0, 0.0, &
!        0.0, 0.0, 1.0, 0.0, &
!		 0.0, 0.0, 0.0, 1.0 /

data K/ 2.0,-1.0, 0.0, &
       -1.0, 4.0,-1.0, &
	    0.0,-1.0, 2.0 /
data M/ 0.5, 0.0, 0.0, &
        0.0, 1.0, 0.0, &
		0.0, 0.0, 0.5 /

!call JACOBI(K,M,V,L,W,N,RTOL,15,1,6)

call JACOBI90(K,M,V,L,W,N,RTOL,15,1,6)

stop
end
