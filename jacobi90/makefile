eigen : eigen.o jacobi.o jacobi90.o
	gfortran -g eigen.o jacobi.o jacobi90.o -o eigen.exe

eigen.o    : eigen.f90
	gfortran -c -g eigen.f90

jacobi.o   : jacobi.for
	gfortran -c -g jacobi.for

jacobi90.o : jacobi90.f90
	gfortran -c -g jacobi90.f90

clean:
	del eigen.exe *.o