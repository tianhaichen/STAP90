      subroutine Readcomnode()

      use commnodes
      use FFI
      implicit none 
      integer,parameter::fileld=10
      integer   orpp,impi

      open(fileld,file="Comm",status="old",iostat=orpp)

      if(orpp/=0)then
        write(*,*)"Open file error!!!"
      stop
      end if

!
      read(fileld,*) ncomde
      
      allocate(comnode(nprocs,ncomde))
      allocate(mycnode(ncomde))
      allocate(bdcomnode(3*ncomde)) 
      do impi=1,nprocs
         read(fileld,*)comnode(impi,:)
      end do
	   
	   mycnode=comnode(myid+1,:)

!      

!change title to character
      FileInp=achar(myid+97)
     ! Procs 0 read a,Procs 1 read b,Procs 2 read c ......        
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Subroutine for update use MPI_allreduce

	subroutine updateforce(istep,DTK1)

    	use commnodes
    	use ParticleData
      implicit none
      include 'mpif.h'     

	integer i,j,ierr,pp,indx

	integer istep
	real(8) DTK1,ADTK1

	real(8) dcommforce(ncomde*3)
	real(8) rcommforce(ncomde*3)
	real(8) lcommass(ncomde),gcommass(ncomde)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        Update Comm node mass  
!        Only for the frist step
      
  if (istep==1)then
!Initile
		lcommass=0
		gcommass=0
!Get part commnode mass
      do j=1,ncomde
 	      if(mycnode(j)==0)then
	             continue
          else
	             pp=mycnode(j)
		         lcommass(j)=Mp(pp)
		  end if
	   end do   
       
	   call MPI_ALLREDUCE(lcommass,gcommass,ncomde,&
 	       MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
!Write to commnode mass       
      do j=1,ncomde
	   
	      if(mycnode(j)==0)then
	             continue
          else
	             pp=mycnode(j)
		         Mp(pp)=gcommass(j)
		  end if
	   end do
  end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Read data from Acc
      do j=1,ncomde
	   
	   if(mycnode(j)==0)then
	      continue
         else
	      pp=mycnode(j)
		  
		  do i=1,3
		     dcommforce((j-1)*3+i)=Acc(i,pp)
          end do

	   end if

	end do   


      rcommforce=0

     call MPI_ALLREDUCE(dcommforce,rcommforce,3*ncomde,&
 	       MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      
       do j=1,ncomde
	
	      if(mycnode(j)==0)then
	            continue
          else
	            pp=mycnode(j)
	         do i=1,3
                   Acc(i,pp)=rcommforce((j-1)*3+i)
             end do
          end if
        end do

! Update min dt        
       call MPI_ALLREDUCE(DTK1,ADTK1,1,MPI_DOUBLE_PRECISION,MPI_MIN,&
            MPI_COMM_WORLD,ierr)
       
       DTK1=ADTK1
   
      end subroutine


