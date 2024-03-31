sspace90.exe : subsp.o sspace90.o
	gfortran -g subsp.o sspace90.o -o sspace90.exe

subsp.o : subsp.f90
	gfortran -c -g subsp.f90

sspace90.o : sspace90.f90
	gfortran -c -g sspace90.f90

clean :
	del sspace90.exe
	del sspace90.o