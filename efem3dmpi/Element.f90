module ElementData

!	Type ELEMENT is used to describe a element
	type Element
		integer:: nNode(8)		!  Element connectivity data
		integer:: mat =0		! material set number (1 ~ nb_mat)
		integer:: nHistory =0	!  Index of element history in HistoryList
	end type Element

	integer:: nb_element	! number of elements
	type(Element), TARGET, allocatable:: element_list(:)  ! Element list

	real(8):: EleDistortion	! element distortion. 1 for no distortion

!	Type RIGIDPLANE is used to describe a rigid plane (wall)
	type RigidPlane
		integer:: nDir	! The unit normal vector of the rigid plane (��1,��2,��3)
						!     Must point towards the impacting body
		real(8):: coor  ! x(��1), y(��2) or z(��3) coordinate of a point on the plane
	end type RigidPlane

	type(RigidPlane) :: plane = RigidPlane(0,0)

	integer:: nb_comp = 0					! number of components
	integer, allocatable:: CompLen(:)		! nb_comp
	integer, allocatable:: CompMember(:,:)	! nb_comp * nb_particle

end module ElementData
