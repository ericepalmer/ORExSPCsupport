C$ Procedure

C$ Abstract
C
C     This code compares the position of OSIRIS-REx between two spk
C     kernels by outputing the magnitude between the spacecrafts
C     specified by the spk kernels at a specific ephemeris time.
C     An error is outputted to the terminal window if the times
C     associated with the spk kernels do not match up.
C
C$ Disclaimer
C
C     This code currently only works on ormacsrv1 2 & 3 due to the meta
C     kernel being called. If you would like this code to work in any
C     environment change the paths or contact Kristofer Drozd.
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
      PARAMETER            (MAXIV=1000000)

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
      DOUBLE PRECISION      VEC
      DOUBLE PRECISION      LT(MAXIV,3)
      DOUBLE PRECISION      ETA(MAXIV)
      DOUBLE PRECISION      ETB(MAXIV)
      DOUBLE PRECISION      COVER (LBCELL:WINSIZ)
      DOUBLE PRECISION      I
      DOUBLE PRECISION      ROT(3,3)
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      a



      INTEGER               NIV
      INTEGER               J
      INTEGER               WNCARD
      INTEGER               L
      INTEGER               K
      INTEGER               INST
      INTEGER               STEP

      CHARACTER*(FILSIZ)    CK
      CHARACTER*1           ENTRY
      CHARACTER*80          SPKPATH1
      CHARACTER*80          SPKPATH2
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
C     SPKMagnitudeDeviation.TXT      O   File displaying Magnitude
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
C     VNORM
C     WNCARD
C
C$ SPICELIB_subroutines_called
C     FURNSH
C     SSIZED
C     SCARDD
C     WNFETD
C     SPKCOV
C     WNFETD
C     TIMOUT
C     CKUPF
C     VSUB
C     SPKPOS
C     UTC2ET
C
C$ History
C     2016_06_29  Birth

      ! Setting up definition
      REF='J2000'

      ! Terminal instructions
      WRITE(*,*) 'Enter in path to reference SPK kernel (.bsp).'
      READ(*,*) SPKPATH1
      WRITE(*,*)
      WRITE(*,*) 'Enter in path to second SPK kernel (.bsp).'
      READ(*,*) SPKPATH2
      WRITE(*,*)
      WRITE(*,*) 'Note that the meta kernel KernelDeviationMeta.mk conta
     .ining all other necessary kernels is loaded in automatically. This
     . meta kernel is located in the /opt/local/spc/metakernel directory
     . on ormacsrv1 2 & 3.'
      WRITE(*,*)
      WRITE(*,*) '  Enter in the step size.'
      READ(*,*) STEP
      WRITE(*,*)

      ! Reading in kernels.
      CALL FURNSH('/opt/local/spc/metakernel/KernelDeviationMeta.mk')

      ! Opening up file to be written to.
      OPEN(UNIT=1,FILE='SPKMagnitudeDeviation.TXT',STATUS='UNKNOWN')

      ! Finding time coverage of first spk kernel.
      CALL FURNSH(SPKPATH1)
      CALL SSIZED(WINSIZ,COVER)
      CALL SCARDD(0,COVER)
      CALL SPKCOV(SPKPATH1,-64,COVER)
      NIV=WNCARD(COVER)
      CALL WNFETD(COVER,1,ETS1,a)
      CALL WNFETD(COVER,NIV,a,ETE1)
      CALL TIMOUT(ETS1,'YYYY Mon DD, HR:MN:SC ::UTC',UTCS1)
      CALL TIMOUT(ETE1,'YYYY Mon DD, HR:MN:SC ::UTC',UTCE1)

      WRITE(1,*) '# First SPK Kernel:'
      WRITE(1,*) '# '//SPKPATH1
      WRITE(1,*) '# '//UTCS1//'   to   '//UTCE1
      WRITE(1,*) '#'

      ! Finding time coverage of second spk kernel
      CALL CKUPF(SPKPATH1)
      CALL FURNSH(SPKPATH2)
      CALL SSIZED (WINSIZ,COVER)
      CALL SCARDD(0,COVER)
      CALL SPKCOV(SPKPATH2,-64,COVER)
      NIV=WNCARD(COVER)
      CALL WNFETD(COVER,1,ETS2,a)
      CALL WNFETD(COVER,NIV,a,ETE2)
      CALL TIMOUT(ETS2,'YYYY Mon DD, HR:MN:SC ::UTC',UTCS2)
      CALL TIMOUT(ETE2,'YYYY Mon DD, HR:MN:SC ::UTC',UTCE2)

      WRITE(1,*) '# Second SPK Kernel:'
      WRITE(1,*) '# '//SPKPATH2
      WRITE(1,*) '# '//UTCS2//'   to   '//UTCE2
      WRITE(1,*) '#'
      WRITE(1,*) '# Step Size (s):'
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

      ! Obtaing spacecraft position from first spk kernel.
      CALL CKUPF(SPKPATH2)
      CALL FURNSH(SPKPATH1)

      K=1

      I=ETS
10    CONTINUE
      CALL SPKPOS ('-64',I,REF,'NONE','BENNU',VEC1(K,:),LT(K,:))
      ETA(K)=I
      K=K+1
      I=I+STEP
      IF(I.LE.ETE) THEN
        GOTO 10
      ENDIF

      ! Obtaining spacecraft position from second spk kernel.
      CALL CKUPF(SPKPATH1)
      CALL FURNSH(SPKPATH2)

      K=1

      I=ETS
15    CONTINUE
      CALL SPKPOS ('-64',I,REF,'NONE','BENNU',VEC2(K,:),LT(K,:))
      ETB(K)=I
      K=K+1
      I=I+STEP
      IF(I.LE.ETE) THEN
        GOTO 15
      ENDIF

      ! Calculating magnitude.
      WRITE(1,*) '# Magnitude Deviation (KM):'

      DO L=1,K-1
        CALL TIMOUT(ETA(L),'YYYY Mon DD, HR:MN:SC ::UTC',UTC)
        CALL VSUB(VEC1(L,:),VEC2(L,:),VEC)
        MAG=VNORM(VEC)
        WRITE(1,FMT='(D20.10,20X,A21)') MAG, UTC
      ENDDO

      CLOSE(UNIT=1)


      CALL KCLEAR

      RETURN
      END


