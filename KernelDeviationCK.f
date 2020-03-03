C$ Procedure

C$ Abstract
C
C     This code compares the attitude of OSIRIS-REx between two ck
C     kernels by outputing the angle between the boresight vectors
C     specified by the ck kernel at a specific ephemeris time.
C     An error is outputted to the terminal window if the times
C     associated with the ck kernels do not match up.
C
C$ Disclaimer
C
C     This code currently only works on ormacsrv1 2 & 3 due to the meta
C     kernel being called. If you would like this code to work in any
C     environment change the paths or contact Kristofer Drozd
C
C     ."".    ."",
C     |  |   /  /
C     |  |  /  /
C     |  | /  /
C     |  |/  ;-._
C     }  ` _/  / ;
C     |  /` ) /  /
C     | /  /_/\_/\
C     |/  /      |
C     (  ' \ '-  |
C      \    `.  /
C       |      |
C       |      |
C
C$    Gnuplot script to plot the values in the textfile this code makes:
C
C     set term X11
C     plot "SPKMagnitudeDeviation.TXT" using 0:1
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               FILSIZ
      PARAMETER            (FILSIZ=255)

      INTEGER               LBCELL
      PARAMETER            (LBCELL=-5)

      INTEGER               MAXIV
      PARAMETER            (MAXIV=100000)

      INTEGER               WINSIZ
      PARAMETER            (WINSIZ =2*MAXIV)



      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      ETS
      DOUBLE PRECISION      ETE
      DOUBLE PRECISION      ETS1
      DOUBLE PRECISION      ETE1
      DOUBLE PRECISION      ETS2
      DOUBLE PRECISION      ETE2
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      CMAT(3,3)
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      BV(3)
      DOUBLE PRECISION      VEC1(MAXIV,3)
      DOUBLE PRECISION      VEC2(MAXIV,3)
      DOUBLE PRECISION      ETA(MAXIV)
      DOUBLE PRECISION      ETB(MAXIV)
      DOUBLE PRECISION      COVER (LBCELL:WINSIZ)
      DOUBLE PRECISION      I
      DOUBLE PRECISION      ROT(3,3)
      DOUBLE PRECISION      a
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      RPD

      INTEGER               NIV
      INTEGER               J
      INTEGER               WNCARD
      INTEGER               L
      INTEGER               K
      INTEGER               INST
      INTEGER               STEP

      CHARACTER*(FILSIZ)    CK
      CHARACTER*1           ENTRY
      CHARACTER*80          CKPATH1
      CHARACTER*80          CKPATH2
      CHARACTER*80          TLSPATH
      CHARACTER*80          TSCPATH
      CHARACTER*24          UTCS
      CHARACTER*24          UTCE
      CHARACTER*5           REF
      CHARACTER*21          UTC
      CHARACTER*21          UTCS1
      CHARACTER*21          UTCE1
      CHARACTER*21          UTCS2
      CHARACTER*21          UTCE2


      LOGICAL               FOUND

C$ File_I/O
C
C     Filename                      I/O  Description
C     ----------------------------  ---  -------------------------------
C     CKAngularDeviation.TXT         O   File displaying Magnitude
C                                        deviation
C
C$ Restrictions
C
C
C$ Author_and_Institution
C
C     Kristofer Drozd    (U of A)
C
C$ Version
C
C     1.0 Birth
C     1.1 Added # to ouput file headers and added a gnuplot code example
C    
C$ SPICELIB_functions_called
C     RPD
C     VNORM
C     VSEP
C
C$ SPICELIB_subroutines_called
C     MXV
C     UCRSS
C     UTC2ET
C     VSUB
C     FURNSH
C     SSIZED
C     SCARDD
C     WNFETD
C     CKCOV
C     WNFETD
C     TIMOUT
C     CKUPF
C     SCE2C
C     CKGP
C
C$ Called_by_SPC_Programs
C     AUTOREGISTER
C     GEOMETRY
C     LITHOS
C
C$ History
C     2016_06_29  Birth


      ! Setting up boresight vector.
      BV( 1 ) =  0.D0
      BV( 2 ) =  0.D0
      BV( 3 ) =  1.D0

      ! Terminal window directions.
      WRITE(*,*) 'Enter in path to reference CK kernel (.bc).'
      READ(*,*) CKPATH1
      WRITE(*,*)
      WRITE(*,*) 'Enter in path to second CK kernel (.bc).'
      READ(*,*) CKPATH2
      WRITE(*,*)
      WRITE(*,*) '  Note that the meta kernel KernelDeviationMeta.mk con
     .taining all other necessary kernels is loaded in automatically. Th
     .is meta kernel is located in the /opt/local/spc/metakernel direct
     .ory on ormacsrv1 2 & 3.'
      WRITE(*,*)
      WRITE(*,*) '  Enter in the step size.'
      READ(*,*) STEP
      WRITE(*,*)

      ! Calling in Kernels.
      CALL FURNSH('/opt/local/spc/metakernel/KernelDeviationMeta.mk')

      ! Seting up definitions.
      REF='J2000'
      INST=-64000

      ! Opening up file to be written to.
      OPEN(UNIT=1,FILE='CKAngularDeviation.TXT',STATUS='UNKNOWN')


      ! Finding the coverage for the first CK kernel.
      CALL FURNSH(CKPATH1)
      CALL SSIZED (WINSIZ,COVER)
      CALL SCARDD(0,COVER)
      CALL CKCOV(CKPATH1,-64000,.FALSE.,'INTERVAL',0.D0,'TDB',COVER)
      NIV=WNCARD(COVER)
      CALL WNFETD(COVER,1,ETS1,a)
      CALL WNFETD(COVER,NIV,a,ETE1)
      CALL TIMOUT(ETS1,'YYYY Mon DD, HR:MN:SC ::UTC',UTCS1)
      CALL TIMOUT(ETE1,'YYYY Mon DD, HR:MN:SC ::UTC',UTCE1)

      WRITE(1,*) '# First CK Kernel:'
      WRITE(1,*) '# '//CKPATH1
      WRITE(1,*) '# '//UTCS1//'   to   '//UTCE1
      WRITE(1,*) '#'


      ! Finding coverage for the second CK kernel.
      CALL CKUPF(CKPATH1)
      CALL FURNSH(CKPATH2)
      CALL SSIZED (WINSIZ,COVER)
      CALL SCARDD(0,COVER)
      CALL CKCOV(CKPATH2,-64000,.FALSE.,'INTERVAL',0.D0,'TDB',COVER)
      NIV=WNCARD(COVER)
      CALL WNFETD(COVER,1,ETS2,a)
      CALL WNFETD(COVER,NIV,a,ETE2)
      CALL TIMOUT(ETS2,'YYYY Mon DD, HR:MN:SC ::UTC',UTCS2)
      CALL TIMOUT(ETE2,'YYYY Mon DD, HR:MN:SC ::UTC',UTCE2)

      WRITE(1,*) '# Second CK Kernel:'
      WRITE(1,*) '# '//CKPATH2
      WRITE(1,*) '# '//UTCS2//'   to   '//UTCE2
      WRITE(1,*) '#'
      WRITE(1,*) '# Step Size (s)'
      WRITE(1,*) '# ',STEP
      WRITE(1,*) '#'


      ! Determining the time interval to perform calculation.
      IF(ETS1.LE.ETS2) THEN
        ETS=ETS2
      ELSE
        ETS=ETS1
      ENDIF

      IF(ETE1.LE.ETE2) THEN
        ETE=ETE1
      ELSE
        ETE=ETE2
      ENDIF

      ! Setting up error message.
      IF(ETE1.LE.ETS2) THEN
        WRITE(*,*) 'Time intervals do not line up.'
      ENDIF


      ! Obtaing pointing vector for first ck kernel.
      CALL CKUPF(CKPATH2)
      CALL FURNSH(CKPATH1)

      K=1

      I=ETS
10    CONTINUE
      CALL SCE2C(-64,I,SCLKDP)
      CALL CKGP(INST,SCLKDP,0.D0,REF,CMAT,CLKOUT,FOUND)
      IF(FOUND) THEN
        CALL MTXV(CMAT,BV,VEC1(K,:))
        ETA(K)=I
      ELSE
        ETA(K)=0.D0
      ENDIF
      K=K+1
      I=I+STEP
      IF(I.LE.ETE) THEN
        GOTO 10
      ENDIF


      ! Obtaining pointing vector for second ck kernel.
      CALL CKUPF(CKPATH1)
      CALL FURNSH(CKPATH2)

      K=1

      I=ETS
15    CONTINUE
      CALL SCE2C(-64,I,SCLKDP)
      CALL CKGP(INST,SCLKDP,0.D0,REF,CMAT,CLKOUT,FOUND)
      IF(FOUND) THEN
        CALL MTXV(CMAT,BV,VEC2(K,:))
        ETB(K)=I
      ELSE
        ETB(K)=0.D0
      ENDIF
      K=K+1
      I=I+STEP
      IF(I.LE.ETE) THEN
        GOTO 15
      ENDIF


      ! Calculating the angular seperation between ck kernels.
      WRITE(1,*) '# Angular Deviation (mr):'

      DO L=1,K-1
        CALL TIMOUT(ETA(L),'YYYY Mon DD, HR:MN:SC ::UTC',UTC)
        ANGLE=VSEP(VEC1(L,:),VEC2(L,:))*1000
        WRITE(1,FMT='(D20.10,20X,A21)') ANGLE, UTC
      ENDDO

      CLOSE(UNIT=1)



      CALL KCLEAR




      RETURN
      END









