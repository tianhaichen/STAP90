module ParticleData

	integer, parameter :: nDim = 3;		! Number of dimension
	integer :: nb_particle = 0			! number of particles

	real(8), allocatable :: Acc(:,:)	! particle acceleration
	real(8), allocatable :: Pos(:,:)	! particle position
	real(8), allocatable :: Vel(:,:)	! particle velocity
	real(8), allocatable :: Fp(:,:)		! external load amplitude
	real(8), allocatable :: Mp(:)		! Mass of material point

end module ParticleData
