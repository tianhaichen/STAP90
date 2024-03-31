module commnodes
       
	   integer ncomde  !sum of commnode
	   integer nprocs    !number of processes
	   integer myid

       integer,allocatable::comnode(:,:)
	   integer,allocatable::mycnode(:)
       real(8),allocatable::bdcomnode(:)
end module
