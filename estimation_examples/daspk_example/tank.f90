program main
!Example program for DDASPK.
!C***ROUTINES CALLED
!C   UINIT, DDASPK
!C Here are necessary declarations.  The dimension statements use a
!C maximum value for the mesh parameter M, and assume ML = MU = 1.
      IMPLICIT NONE !DOUBLE PRECISION (A-H,O-Z)
      
      EXTERNAL DBANJA, DBANPS
      real(8) :: atol, rpar, rtol, rwork, t, tout, u, uprime
      integer :: lenrw, ires, ipAR, i, leniw, lenwp, &
      leniwp, idid, info, iout, iwork, lenpd, liw, liwp, lrw, &
      lwp, m, mband, ml, msave, mu, neq, nout
      integer, PARAMETER :: MAXM = 10, MAXM2 = MAXM+2, MXNEQ = MAXM2*MAXM2
      PARAMETER (LENRW = 91 + 18*MXNEQ, LENIW  = 40,  &
               LENWP  = 5*MXNEQ + 2*((MXNEQ/3) + 1), LENIWP = MXNEQ)
      DIMENSION U(MXNEQ),UPRIME(MXNEQ), &
               RWORK(LENRW+LENWP),IWORK(LENIW+LENIWP)
     
      DIMENSION INFO(20), RPAR(1), IPAR(1)

!C
!C Here set parameters for the problem being solved.  Use RPAR and IPAR
!C to communicate these to the other routines.
!C
      M = MAXM
!      DX = .1D0 !/(M+1)
      NEQ = 2 !(M+2)*(M+2)
!      COEFF = 1.0D0/(DX*DX)
!C
!      IPAR(3) = NEQ
!      IPAR(4) = M
!      RPAR(1) = DX
!      RPAR(2) = COEFF
!C
!C Here set the half-bandwidths and load them into IPAR for use by the
!C preconditioner routines.
      ML = 1
      MU = 1
!      IPAR(1) = ML
!      IPAR(2) = MU
!C
!C Here set the lengths of the preconditioner work arrays WP and IWP,
!C load them into IWORK, and set the total lengths of WORK and IWORK.
      LENPD = (2*ML + MU + 1)*NEQ 
      MBAND = ML + MU + 1
      MSAVE = (NEQ/MBAND) + 1
      LWP = LENPD + 2*MSAVE
      LIWP = NEQ
      IWORK(27) = LWP
      IWORK(28) = LIWP
      LRW = LENRW + LWP
      LIW = LENIW + LIWP
!C
!C Call subroutine UINIT to initialize U and UPRIME.
!C
!      CALL UINIT (U, UPRIME, RPAR, IPAR)
U(1)= .1 !H
U(2)= .1 !Fo

UPRIME(1)= .1 !H
UPRIME(2)= .1 !Fo

!C Here we set up the INFO array, which describes the various options
!C in the way we want DDASPK to solve the problem.
!C In this case, we select the iterative preconditioned Krylov method,
!C and we supply the band preconditioner routines DBANJA/DBANPS.
!C
!C We first initialize the entire INFO array to zero, then set select
!C entries to nonzero values for desired solution options.
!C
!C To select the Krylov iterative method for the linear systems,
!C we set INFO(12) = 1.
!C
!C Since we are using a preconditioner that involves approximate
!C Jacobian elements requiring preprocessing, we have a JAC routine,
!C namely subroutine DBANJA, and we must set INFO(15) = 1 to indicate
!C this to DDASPK.
!C
!C No other entries of INFO need to be changed for this example.
!C-----------------------------------------------------------------------

      DO I = 1,20
        INFO(I) = 0
      enddo


!C Here we set tolerances for DDASPK to indicate how much accuracy 
!C we want in the solution, in the sense of local error control.
!c For this example, we ask for pure absolute error control with a
!C tolerance of 1.0D-3.
    RTOL=1.D-6     !TOLERÂNCIA RELATIVA
    ATOL=1.D-8     !TOLERÂNCIA ABSOLUTA


!c
!c-----------------------------------------------------------------------
!c Now we solve the problem.
!c
!c DDASPK will be called to compute 11 intermediate solutions from
!c tout = 0.01 to tout = 10.24 by powers of 2.
!c
!c We pass to DDASPK the names DBANJA and DBANPS for the JAC and PSOL
!c routines to do the preconditioning.
!c
!c At each output time, we compute and print the max-norm of the
!c solution (which should decay exponentially in t).  We also print
!c some relevant statistics -- the current method order and step size,
!c the number of time steps so far, and the numbers of nonlinear and
!c linear iterations so far.
!c
!c If DDASPK failed in any way (IDID .lt. 0) we print a message and
!c stop the integration.
!c-----------------------------------------------------------------------
!c
      NOUT = 1000
      T = 0.0D0
      TOUT = .1D0
      DO IOUT = 1,NOUT
         print*, T, U(1), U(2)
         CALL DDASPK (RESH, NEQ, T, U, UPRIME, TOUT, INFO, RTOL, ATOL, &
             IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR, DBANJA, DBANPS)

         IF (IDID .LT. 0) THEN
           WRITE(*,65)T
 65        FORMAT(//' Final time reached =',E12.4//)
           GO TO 80
           ENDIF
!c
         TOUT = TOUT+.1d0
      enddo
!c
!c Here we display some final statistics for the problem.
!c The ratio of NLI to NNI is the average dimension of the Krylov
!c subspace involved in the Krylov linear iterative method.
 80   CONTINUE
!c
!c------  End of main program for DHEAT example program -----------------

 contains

      SUBROUTINE RESH (T, U, UPRIME, CJ, DELTA, IRES, RPAR, IPAR)
!c
!c This is the user-supplied RES subroutine for this example.
!c It computes the residuals for the 2-D discretized heat equation,
!c with zero boundary values.
!c
      IMPLICIT NONE !DOUBLE PRECISION (A-H,O-Z)
      integer :: ires, ipar, idid
      real(8) :: t, u, uprime, cj, DELTA, rpar
      DIMENSION U(*), UPRIME(*), DELTA(*), RPAR(1), IPAR(1)
!c

    DELTA(1)=-1*UPRIME(1)+1-U(2) !EQUAÇÃO DIFERENCIAL
    DELTA(2)=-U(2)+1*U(1)  !EQUAÇÃO ALGÉBRICA

!c------------  End of Subroutine RESH  ---------------------------------
      END

endprogram
