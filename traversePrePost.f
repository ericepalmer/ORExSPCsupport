C Eric E Palmer - 25 Nov 2024.
C transitPrePost.f - this calculates and displays
C     the height for two different directories for comet 67P
C     from before and after perhelion (pre and post)
C     Data for the input can come from spc_tools Depth

      IMPLICIT NONE

      INTEGER           NTMP
      PARAMETER        (NTMP=2501)

      DOUBLE PRECISION  SCALE
      DOUBLE PRECISION  V(3)
      DOUBLE PRECISION  UX(3)
      DOUBLE PRECISION  UY(3)
      DOUBLE PRECISION  UZ(3)
      DOUBLE PRECISION  Z0, Z1, Z2
      DOUBLE PRECISION  X, Y
      DOUBLE PRECISION  C(0:3)

      REAL*4                HT1(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                HT2(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                ALB(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                GPre(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                GPost(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                HPre(-NTMP:NTMP)
      REAL*4                HPost(-NTMP:NTMP)
      REAL*4                DNPre(-NTMP:NTMP,NTMP)
      REAL*4                DNPost(-NTMP:NTMP,NTMP)

      INTEGER            I, I1
      INTEGER            J, J1
      INTEGER            IC, IL
      INTEGER            JC, JL
      INTEGER            K, L
      INTEGER            QSZ
      INTEGER            Q, Q0, Q1

      CHARACTER*6        MAPNM
      CHARACTER*72       MAPFILE
      CHARACTER*72       INFILE
      CHARACTER*72       OUTFILE
      CHARACTER*10002    LINE

      LOGICAL            GUSE(-NTMP:NTMP,-NTMP:NTMP)
      real               version

      version = 1.0
      write (*,*) 'Run this in the upper level directory'
      write (*,*) "Version: ", version

      WRITE(6,*) 'Input mapname'
      READ(5,FMT='(A6)') MAPNM

      WRITE(6,*) 'Input center'
      READ(5,*) IC, JC

      WRITE(6,*) 'Input left'
      READ(5,*) IL, JL

      MAPFILE='pre1/MAPFILES/'//MAPNM//'.MAP'
C      MAPFILE='MAPFILES/'//MAPNM//'.MAP'
      CALL READ_MAP(MAPFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT1,ALB)
      MAPFILE='post1/MAPFILES/'//MAPNM//'.MAP'
C      MAPFILE='MAPFILES/'//MAPNM//'.MAP'
      CALL READ_MAP(MAPFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT2,ALB)

C     Set the grid values for usable
C     G is the height over the line
C     Q0 is the limited size of matrix
      Z1=0
      Z2=0
      Q0=2*MAX(ABS(IC-IL), ABS(JC-JL))
      DO I=-Q0,Q0
      DO J=-Q0,Q0
        GPre(I,J)=0.E0
        GPost(I,J)=0.E0
        GUSE(I,J)=.FALSE.
        IF((I+IC-QSZ-1.GE.-QSZ).AND.(I+IC-QSZ.LE.QSZ).AND.
     .      (J+JC-QSZ.GE.-QSZ).AND.(J+JC-QSZ.LE.QSZ)) THEN
          GPre(I,J)=HT1(I+IC-QSZ-1,J+JC-QSZ)-HT1(I+IC-QSZ,J+JC-QSZ)
          GPost(I,J)=HT2(I+IC-QSZ-1,J+JC-QSZ)-HT2(I+IC-QSZ,J+JC-QSZ)
          Z1=MIN(GPre(I,J),Z1)
          Z2=MAX(GPre(I,J),Z2)
          Z1=MIN(GPost(I,J),Z1)
          Z2=MAX(GPost(I,J),Z2)
          GUSE(I,J)=.TRUE.
        ENDIF
      ENDDO
      ENDDO

C     Rescale things from min/max
      DO I=-Q0,Q0
      DO J=-Q0,Q0
      IF(GUSE(I,J)) THEN
        GPre(I,J)=REAL((GPre(I,J)-Z1)/(Z2-Z1))
        GPost(I,J)=REAL((GPost(I,J)-Z1)/(Z2-Z1))
      ENDIF
      ENDDO
      ENDDO

C     Set width
C     Q1 is the number of pixels that the line will be mapped to
      Z0=2*SQRT(1.D0*((IC-IL)**2+(JC-JL)**2))
      Q1=NINT(Z0)

C------------------------------------------------------------------------
C     H is the height, set in a lineral array
C     Calcualte the height in 2D.  Set min/max also
      Z1= 1.D10
      Z2=-1.D10
      DO K=-Q1,Q1
        X=IC+K*(IC-IL)/Z0-0.5
        Y=JC+K*(JC-JL)/Z0
        I=NINT(X)
        J=NINT(Y)
        GPre(I-IC,J-JC)=1
        GPost(I-IC,J-JC)=1
        I=INT(X)
        J=INT(Y)
        X=X-I
        Y=Y-J

C       Calculate Pre
        L=-1
        DO J1=0,1
        DO I1=0,1
          L=L+1
          C(L)=HT1(I+I1-QSZ, J+J1-QSZ)
        ENDDO
        ENDDO
        C(3)=C(0)-C(1)-C(2)+C(3)
        C(1)=C(1)-C(0)
        C(2)=C(2)-C(0)
        HPre(K)=REAL(C(0)+C(1)*X+C(2)*Y+C(3)*X*Y) 
        Z1=MIN(Z1,HPre(K))
        Z2=MAX(Z2,HPre(K))

C       Calculate Post
        L=-1
        DO J1=0,1
        DO I1=0,1
          L=L+1
          C(L)=HT2(I+I1-QSZ, J+J1-QSZ)
        ENDDO
        ENDDO
        C(3)=C(0)-C(1)-C(2)+C(3)
        C(1)=C(1)-C(0)
        C(2)=C(2)-C(0)
        HPost(K)=REAL(C(0)+C(1)*X+C(2)*Y+C(3)*X*Y) 
        Z1=MIN(Z1,HPost(K))
        Z2=MAX(Z2,HPost(K))

      ENDDO
C------------------------------------------------------------------------

      K=INT(5*(Z2-Z1))+1

C     Q is display width, largest of all
      Q=MAX(Q0,Q1)

C     Fill whole frame with black
      DO I=-Q,Q                                                         column
        DO J=1,2*Q0+20+K                                                row
          DNPre(I,J)=.05
          DNPost(I,J)=.10
        ENDDO
      ENDDO
 
C     Top is image with tracking line
C     Make top - fill with current topo with line enhanced
C     Pre
      DO I=-Q0,Q0
      DO J=1,2*Q0+1
        DNPre(I,J)=GPre(I,J-Q0-1)
      ENDDO
      ENDDO
C     Post
      DO I=-Q0,Q0
      DO J=1,2*Q0+1
        DNPost(I,J)=GPost(I,J-Q0-1)
      ENDDO
      ENDDO

C     Bottom is profile, J is the height, scaled
C     Pre
      DO I=-Q1,Q1
        J=NINT(5*(Z2-HPre(I)))+2*Q0+10+1
        DNPre(I,J)=1
        DNPre(I,2*Q0+2)=.5
      ENDDO

C     Post
      DO I=-Q1,Q1
        J=NINT(5*(Z2-HPost(I)))+2*Q0+10+1
        DNPost(I,J)=.75
        DNPost(I,2*Q0+2)=.5
      ENDDO

C------------------------------------------------------------------------
C     Write the file and covert it to PGM
      INFILE='TEMPFILE.GRAY'
      OPEN(UNIT=10, FILE=INFILE, ACCESS='DIRECT',
     .     RECL=4*Q+1, STATUS='UNKNOWN')
        DO J=1,2*Q0+20+K
          DO I=-Q,Q
            LINE(I+Q+1:I+Q+1)=CHAR(NINT(255*DNPre(I,J)))
          ENDDO
          DO I=-Q,Q
            LINE(I+3*Q+1:I+3*Q+1)=CHAR(NINT(255*DNPost(I,J)))
          ENDDO
          WRITE(10,REC=J) LINE(1:4*Q+1)
        ENDDO
      CLOSE(UNIT=10)

      OUTFILE='TEMPFILE.pgm'

      CALL RAW2PGM(INFILE, OUTFILE, 4*Q+1, 2*Q0+20+K)
c      OPEN(UNIT=63, FILE=INFILE, STATUS='OLD')
c      CLOSE(UNIT=63, STATUS='DELETE')

      STOP
      END


