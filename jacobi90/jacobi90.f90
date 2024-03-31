      Subroutine JACOBI90 (A,B,X,EIGV,D,N,RTOL,NSMAX,IFPR,IOUT)
! .....................................................................
! .                                                                   .
! .   P R O G R A M                                                   .
! .        TO SOLVE THE GENERALIZED EIGENPROBLEM USING THE            .
! .        GENERALIZED JACOBI ITERATION                               .
! .                                                                   .
! . - - INPUT VARIABLES - -                                           .
! .        A(N,N)    = STIFFNESS MATRIX (ASSUMED POSITIVE DEFINITE)   .
! .        B(N,N)    = MASS MATRIX (ASSUMED POSITIVE DEFINITE)        .
! .        X(N,N)    = STORAGE FOR EIGENVECTORS                       .
! .        EIGV(N)   = STORAGE FOR EIGENVALUES                        .
! .        D(N)      = WORKING VECTOR                                 .
! .        N         = ORDER OF MATRICES A AND B                      .
! .        RTOL      = CONVERGENCE TOLERANCE (USUALLY SET TO 10.**-12).
! .        NSMAX     = MAXIMUM NUMBER OF SWEEPS ALLOWED               .
! .                                  (USUALLY SET TO 15)              .
! .        IFPR      = FLAG FOR PRINTING DURING ITERATION             .
! .            EQ.0    NO PRINTING                                    .
! .            EQ.1    INTERMEDIATE RESULTS ARE PRINTED               .
! .        IOUT      = UNIT NUMBER USED FOR OUTPUT                    .
! .                                                                   .
! . - - OUTPUT - -                                                    .
! .        A(N,N)    = DIAGONALIZED STIFFNESS MATRIX                  .
! .        B(N,N)    = DIGONALIZED MASS MATRIX                        .
! .        X(N,N)    = EIGENVECTORS STORED COLUMNWISE                 .
! .        EIGV(N)   = EIGENVALUES                                    .
! .                                                                   .
! .....................................................................
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N,NSMAX,IFPR,IOUT
      REAL(8), INTENT(INOUT) :: A(N,N),B(N,N),X(N,N),D(N),EIGV(N)
      REAL(8), INTENT(IN) :: RTOL
!
      INTEGER :: I,J,K,JJ,NSWEEP,NR,KP1,JM1,JP1,KM1
      REAL(8) :: EPS,EPSA,EPSB,D1,D2,AK,BK,AJ,BJ,DFN,SQCH,ABCH,AB,BB
      REAL(8) :: AJJ,AJJCH,AKK,AKKCH,DIF,TOL,CHECK,SCALE,CA,CG
      REAL(8) :: DEN,EPTOLA,EPTOLB,XJ,XK
!
!     INITIALIZE EIGENVALUE AND EIGENVECTOR MATRICES
!
      DO I=1,N
         IF (A(I,I).LE.0. .OR. B(I,I).LE.0.) THEN
            WRITE (IOUT,2020)
            STOP
         ENDIF
         D(I)=A(I,I)/B(I,I)
         EIGV(I)=D(I)
      END DO
!
      DO I=1,N
         DO J=1,N
            X(I,J)=0.
         END DO
         X(I,I)=1.
      END DO
      IF (N.EQ.1) RETURN
!
!     INITIALIZE SWEEP COUNTER AND BEGIN ITERATION
!
      NSWEEP=1
      NR=N - 1
!
      DO WHILE(NSWEEP.LE.NSMAX)
         IF (IFPR.EQ.1) WRITE (IOUT,2000) NSWEEP
!
!     CHECK IF PRESENT OFF-DIAGONAL ELEMENT IS LARGE ENOUGH TO
!     REQUIRE ZEROING
!
         EPS=(.01)**(NSWEEP*2)
         DO J=1,NR
            JJ=J + 1
            DO K=JJ,N
               EPTOLA=(A(J,K)/A(J,J))*(A(J,K)/A(K,K))
               EPTOLB=(B(J,K)/B(J,J))*(B(J,K)/B(K,K))
               IF (EPTOLA.LT.EPS .AND. EPTOLB.LT.EPS) CYCLE
!
!     IF ZEROING IS REQUIRED, CALCULATE THE ROTATION MATRIX
!     ELEMENTS CA AND CG
!
               AKK=A(K,K)*B(J,K) - B(K,K)*A(J,K)
               AJJ=A(J,J)*B(J,K) - B(J,J)*A(J,K)
               AB=A(J,J)*B(K,K) - A(K,K)*B(J,J)
               SCALE=A(K,K)*B(K,K)
               ABCH=AB/SCALE
               AKKCH=AKK/SCALE
               AJJCH=AJJ/SCALE
               CHECK=(ABCH*ABCH + 4.*AKKCH*AJJCH)/4.
               IF (CHECK .LT. 0) THEN
                  WRITE (IOUT,2020)
                  STOP
               ENDIF
               SQCH=SCALE*SQRT(CHECK)
               D1=AB/2. + SQCH
               D2=AB/2. - SQCH
               DEN=D1
               IF (ABS(D2).GT.ABS(D1)) DEN=D2
               IF (DEN .EQ. 0) THEN
                  CA=0.
                  CG=-A(J,K)/A(K,K)
               ELSE
                  CA=AKK/DEN
                  CG=-AJJ/DEN
               ENDIF
!
!     PERFORM THE GENERALIZED ROTATION TO ZERO ELEMENTS
!
               IF (N-2 .NE. 0) THEN
                  JP1=J + 1
                  JM1=J - 1
                  KP1=K + 1
                  KM1=K - 1
                  IF (JM1-1 .GE. 0) THEN
                     DO I=1,JM1
                        AJ=A(I,J)
                        BJ=B(I,J)
                        AK=A(I,K)
                        BK=B(I,K)
                        A(I,J)=AJ + CG*AK
                        B(I,J)=BJ + CG*BK
                        A(I,K)=AK + CA*AJ
                        B(I,K)=BK + CA*BJ
                     ENDDO
                  ENDIF
                  IF (KP1-N .LE. 0) THEN
                     DO I=KP1,N
                        AJ=A(J,I)
                        BJ=B(J,I)
                        AK=A(K,I)
                        BK=B(K,I)
                        A(J,I)=AJ + CG*AK
                        B(J,I)=BJ + CG*BK
                        A(K,I)=AK + CA*AJ
                        B(K,I)=BK + CA*BJ
                     ENDDO
                  ENDIF
                  IF (JP1-KM1 .LE. 0) THEN
                     DO I=JP1,KM1
                        AJ=A(J,I)
                        BJ=B(J,I)
                        AK=A(I,K)
                        BK=B(I,K)
                        A(J,I)=AJ + CG*AK
                        B(J,I)=BJ + CG*BK
                        A(I,K)=AK + CA*AJ
                        B(I,K)=BK + CA*BJ
                     ENDDO
                  ENDIF
               ENDIF
               AK=A(K,K)
               BK=B(K,K)
               A(K,K)=AK + 2.*CA*A(J,K) + CA*CA*A(J,J)
               B(K,K)=BK + 2.*CA*B(J,K) + CA*CA*B(J,J)
               A(J,J)=A(J,J) + 2.*CG*A(J,K) + CG*CG*AK
               B(J,J)=B(J,J) + 2.*CG*B(J,K) + CG*CG*BK
               A(J,K)=0.
               B(J,K)=0.
!
!     UPDATE THE EIGENVECTOR MATRIX AFTER EACH ROTATION
!
               DO I=1,N
                  XJ=X(I,J)
                  XK=X(I,K)
                  X(I,J)=XJ + CG*XK
                  X(I,K)=XK + CA*XJ
               ENDDO
            ENDDO
         ENDDO
!
!     UPDATE THE EIGENVALUES AFTER EACH SWEEP
!
         DO I=1,N
            IF (A(I,I).LE.0. .OR. B(I,I).LE.0.) THEN
               WRITE (IOUT,2020)
               STOP
            ENDIF
            EIGV(I)=A(I,I)/B(I,I)
         ENDDO
         IF (IFPR.NE.0) THEN
            WRITE (IOUT,2030)
            WRITE (IOUT,2010) (EIGV(I),I=1,N)
         ENDIF
!
!     CHECK FOR CONVERGENCE
!
         DO I=1,N
            TOL=RTOL*D(I)
            DIF=ABS(EIGV(I)-D(I))
            IF (DIF.GT.TOL) GOTO 280
         ENDDO
!
!     CHECK OFF-DIAGONAL ELEMENTS TO SEE IF ANOTHER SWEEP IS NEEDED
!
         EPS=RTOL**2
         DO J=1,NR
            JJ=J + 1
            DO K=JJ,N
               EPSA=(A(J,K)/A(J,J))*(A(J,K)/A(K,K))
               EPSB=(B(J,K)/B(J,J))*(B(J,K)/B(K,K))
               IF (EPSA.GT.EPS .OR. EPSB.GT.EPS) GOTO 280
            ENDDO
         ENDDO
!
!     CONVERGENCE REACHED
!
         EXIT
!
!     UPDATE D MATRIX AND START NEW SWEEP, IF ALLOWED
!
280      DO I=1,N
            D(I)=EIGV(I)
         ENDDO
         NSWEEP=NSWEEP + 1
      ENDDO
!
!     FILL OUT BOTTOM TRIANGLE OF RESULTANT MATRICES, SCALE EIGENVECTORS
!
      DO I=1,N
         DO J=I,N
            A(J,I)=A(I,J)
            B(J,I)=B(I,J)
         ENDDO
      ENDDO
!
      DO J=1,N
         BB=SQRT(B(J,J))
         DO K=1,N
            X(K,J)=X(K,J)/BB
         ENDDO
      ENDDO
!
      RETURN
!
 2000 FORMAT (//,' SWEEP NUMBER IN *JACOBI* = ',I8)
 2010 FORMAT (' ',6E20.12)
 2020 FORMAT (//,' *** ERROR *** SOLUTION STOP',/, &
                 ' MATRICES NOT POSITIVE DEFINITE')
 2030 FORMAT (//,' CURRENT EIGENVALUES IN *JACOBI* ARE',/)
      END
